{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HKF.Check where

import Control.Monad.Writer
import Data.Bifunctor
import qualified Data.List as L
import Data.Validation
import GHC.Num (intToNatural)
import HKF.Ast (Spanned (..), StaticLayerEntry (ExpressionEntry), spanOf, unspan)
import qualified HKF.Ast as A
import Relude.Extra (DynamicMap (insert), StaticMap (lookup, member))
import qualified Relude.String.Reexport as T

data EType
  = TBroken -- eg: we can't infer a type because the underlying declaration errors out somewhere else
  | TKeycode -- in a way, this is a chord with one element
  | TChord -- in a way, this is a sequence with one element
  | TSequence -- [Chord], more or less
  | TTemplate -- type of templates
  | TArrow EType EType
  deriving (Show)

data Context = MkContext
  { scope :: HashMap T.Text (Spanned A.ToplevelDeclaration),
    types :: HashMap T.Text EType
  }
  deriving (Show)

type Checked = Writer [CheckError]

data TypeExpectationReason
  = MustSatisfyCall
      { mscFunction :: A.Expression,
        mscArgument :: A.Expression,
        mscArgumentType :: EType
      }
  | MustSatisfyChord
      { mscChord :: A.Expression,
        mscTerm :: A.Expression
      }
  | MustSatisfySequence
      { mssChord :: A.Expression,
        mssTerm :: A.Expression
      }
  | StaticLayerMember
      { slmExpression :: A.Expression
      }
  | StaticLayersRequireTemplate
      { slrtLayer :: Spanned A.StaticLayer,
        slrtTemplateName :: Spanned T.Text
      }
  deriving (Show)

data CheckError
  = WrongType
      { expectedType :: EType,
        actualType :: EType,
        expression :: A.Expression,
        contradictions :: [(EType, EType)],
        expectedBecause :: TypeExpectationReason
      }
  | WrongDeclarationType
      { expectedType :: EType,
        actualType :: EType,
        source :: Spanned A.ToplevelDeclaration,
        contradictions :: [(EType, EType)],
        expectedBecause :: TypeExpectationReason
      }
  | WrongArgument
      { waFunction :: A.Expression,
        waArgument :: A.Expression,
        waFunctionType :: EType,
        waArgumentType :: EType,
        waContradictions :: [(EType, EType)]
      }
  | NotCallable
      { ncFunction :: A.Expression,
        ncArgument :: A.Expression,
        typeofFunction :: EType,
        typeofArgument :: EType
      }
  | VarNotInScope
      { vnsSource :: Spanned T.Text
      }
  | AlreadyInScope T.Text
  | WrongTemplateLength (Spanned A.StaticLayer) (Spanned T.Text) Natural Natural
  | DuplicateTemplateKeycode (Spanned A.LayerTemplate) T.Text
  | InvalidKeycode (Spanned T.Text)
  deriving (Show)

---------- Helpers
lookupScope :: T.Text -> Context -> Maybe (Spanned A.ToplevelDeclaration)
lookupScope name ctx = lookup name (scope ctx)

lookupTypeScope :: T.Text -> Context -> Maybe EType
lookupTypeScope name ctx = lookup name (types ctx)

lookupTyped :: Spanned T.Text -> (EType, TypeExpectationReason) -> Context -> Checked (Maybe (Spanned A.ToplevelDeclaration))
lookupTyped spannedName@(Spanned _ name) (expected, reason) ctx = case (lookupScope name ctx, lookupTypeScope name ctx) of
  (Just decl, Just ty) -> do
    let contradictions = subtype ty expected
    unless (L.null contradictions) do
      tell [WrongDeclarationType expected ty decl contradictions reason]
    pure $ Just decl
  _ -> do
    tell [VarNotInScope spannedName]
    pure Nothing

validKeyCode :: T.Text -> Bool
validKeyCode "wrong" = False
validKeyCode _ = True

---------- Validators
-- Validates a list and errors out on every duplicate found
checkForDuplicates :: Eq a => (a -> CheckError) -> [a] -> Checked ()
checkForDuplicates mkE arr = do
  unless (L.null duplicates) do
    tell (mkE <$> duplicates)
  where
    duplicates = L.nub [a | a <- arr, length [b | b <- arr, a == b] > 1]

-- Ensures the type of a is less general than the type of b
subtype :: EType -> EType -> [(EType, EType)]
subtype left right = case (left, right) of
  (TKeycode, TKeycode) -> trivial
  (TKeycode, TChord) -> trivial
  (TChord, TChord) -> trivial
  (TKeycode, TSequence) -> trivial
  (TChord, TSequence) -> trivial
  (TSequence, TSequence) -> trivial
  -- easy way out for variables
  (TArrow f t, TArrow f' t') ->
    let e = subtype f' f
        e' = subtype t t'
     in e <> e'
  (TBroken, a) -> trivial
  (a, TBroken) -> trivial
  (a, b) -> pure (a, b) -- oh no, we found a contradiction!
  where
    trivial = []

inferType :: A.Expression -> Context -> Checked EType
inferType spanned@(Spanned span e) ctx = case e of
  A.Key keycode -> checkKeycode keycode $> TKeycode
  A.Variable spannedName -> case lookup (unspan spannedName) (types ctx) of
    Just ty -> pure ty
    Nothing -> do
      tell [VarNotInScope spannedName]
      pure TBroken
  A.Call f [] -> inferType f ctx
  A.Call f args -> do
    fType <- inferType f ctx
    go f fType args
    where
      -- TODO: consider matching all the argument types at once, for better errors?
      go f fType [] = pure fType
      go f fType (h : t) = do
        oType <- inferSingleCall f fType h
        go (Spanned (spanOf f <> spanOf h) $ A.Call f [h]) oType t
  A.Chord inner -> do
    forM_ inner \t ->
      checkExpression t (TChord, reason t) ctx
    pure TChord
    where
      reason = MustSatisfyChord spanned
  A.Sequence inner -> do
    forM_ inner \t ->
      checkExpression t (TSequence, reason t) ctx
    pure TSequence
    where
      reason = MustSatisfySequence spanned
  where
    inferSingleCall func fType arg = do
      aType <- inferType arg ctx
      case fType of
        TBroken -> pure TBroken -- oof, we really fucked up by getting here
        TArrow from to -> do
          let contradictions = subtype aType from
          unless (L.null contradictions) do
            let error = WrongArgument func arg fType aType contradictions
            tell [error]
          pure to
        somethingElse -> do
          let error = NotCallable func arg fType aType
          tell [error]
          pure TBroken

checkExpression ::
  A.Expression ->
  (EType, TypeExpectationReason) ->
  Context ->
  Checked ()
checkExpression spanned@(Spanned span e) (expected, reason) ctx = case e of
  other -> do
    inferred <- inferType spanned ctx
    let contradictions = subtype inferred expected
    unless (L.null contradictions) do
      let error = WrongType expected inferred spanned contradictions reason
      tell [error]

checkKeycode :: Spanned T.Text -> Checked ()
checkKeycode keycode =
  unless (validKeyCode (unspan keycode)) do
    tell [InvalidKeycode keycode]

-- Runs all the checks on a given template
checkLayerTemplate ::
  Spanned A.LayerTemplate ->
  Context ->
  Checked ()
checkLayerTemplate this@(Spanned _ template) ctx = noDuplicates *> keycodeCheck
  where
    keycodeCheck =
      for_
        (A.templateKeycodes template)
        checkKeycode

    noDuplicates =
      checkForDuplicates
        (DuplicateTemplateKeycode this)
        (unspan <$> A.templateKeycodes template)

checkStaticLayer ::
  Spanned A.StaticLayer ->
  Context ->
  Checked ()
checkStaticLayer this@(Spanned _ layer) ctx = lengthCheck *> expressionCheck
  where
    expressionCheck = for_
      (A.staticLayerContents layer)
      \case
        A.WildcardEntry -> pure ()
        A.ExpressionEntry expr -> checkExpression expr (TSequence, StaticLayerMember expr) ctx

    lengthCheck = do
      let templateName = A.staticLayerTemplate layer
      res <- lookupTyped templateName (TTemplate, StaticLayersRequireTemplate this templateName) ctx
      case res of
        Just (Spanned _ (A.LayerTemplate template)) ->
          unless (templateLength == layerLength) do
            tell
              [ WrongTemplateLength
                  this
                  templateName
                  (intToNatural templateLength)
                  (intToNatural layerLength)
              ]
          where
            templateLength = length (A.templateKeycodes template)
            layerLength = length (A.staticLayerContents layer)
        _ -> pure ()

checkConfig ::
  [(Spanned T.Text, Spanned A.ToplevelDeclaration)] ->
  Context ->
  Checked ()
checkConfig t ctx = foldM_ (flip checkDeclaration) ctx t

checkDeclaration ::
  (Spanned T.Text, Spanned A.ToplevelDeclaration) ->
  Context ->
  Checked Context
checkDeclaration (name, decl) ctx = do
  let span = spanOf decl
  let tLayer = TArrow TChord TSequence
  ty <- case unspan decl of
    A.LayerTemplate template -> do
      checkLayerTemplate (Spanned span template) ctx
      pure TTemplate
    A.Layer (A.StaticLayer layer) -> do
      checkStaticLayer (Spanned span layer) ctx
      pure tLayer
    A.Alias expr -> do
      inferType expr ctx
    _ -> pure TBroken

  pure $
    MkContext
      { scope = insert (unspan name) decl (scope ctx),
        types = insert (unspan name) ty (types ctx)
      }

--         Spanned span (A.Layer (A.StaticLayer layer)) ->
--           first (map $ StaticLayerError . Spanned span) $
--             checkStaticLayer layer ctx
--         _ -> pure ()
