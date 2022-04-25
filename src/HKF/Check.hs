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
import Relude.Extra (StaticMap (lookup, member))
import qualified Relude.String.Reexport as T

data EType
  = TBroken -- eg: we can't infer a type because the underlying declaration errors out somewhere else
  | TKeycode -- in a way, this is a chord with one element
  | TChord -- in a way, this is a sequence with one element
  | TSequence -- [Chord], more or less
  | TArrow EType EType
  deriving (Show)

data Context = MkContext
  { scope :: [(Spanned T.Text, Spanned A.ToplevelDeclaration)],
    types :: HashMap T.Text EType
  }
  deriving (Show)

type Checked = Writer [ExpressionError]

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
  deriving (Show)

data ExpressionError
  = WrongType
      { expectedType :: EType,
        actualType :: EType,
        expression :: A.Expression,
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
      { vnsSource :: A.Expression
      }
  deriving (Show)

data StaticLayerError
  = NotATemplate (Spanned T.Text) (Spanned A.ToplevelDeclaration)
  | CannotPickTemplate (Spanned T.Text) [Spanned A.ToplevelDeclaration]
  | WrongTemplateLength (Spanned T.Text) Natural Natural
  | NoTemplateInScope (Spanned T.Text)
  | ExpressionMemberError ExpressionError
  deriving (Show)

data TemplateCheckError
  = DuplicateTemplateKeycode T.Text
  | InvalidTemplateKeycode (Spanned T.Text)
  deriving (Show)

data CheckError
  = SameName T.Text
  | TemplateCheckError (Spanned TemplateCheckError)
  | StaticLayerError (Spanned StaticLayerError)
  deriving (Show)

type CheckM a = Context -> Validation [CheckError] a

---------- Helpers
lookupScope :: T.Text -> Context -> [Spanned A.ToplevelDeclaration]
lookupScope name ctx = [snd v | v <- scope ctx, unspan (fst v) == name]

lookupTemplate :: Spanned T.Text -> Context -> Validation [StaticLayerError] A.LayerTemplate
lookupTemplate spannedName@(Spanned _ name) ctx = case lookupScope name ctx of
  [] -> Failure [NoTemplateInScope spannedName]
  [Spanned _ (A.LayerTemplate t)] -> pure t
  [somethingElse] -> Failure [NotATemplate spannedName somethingElse]
  multiple -> Failure [CannotPickTemplate spannedName multiple]

validKeyCode :: T.Text -> Bool
validKeyCode "wrong" = False
validKeyCode _ = True

---------- Validators
-- Validates a list and errors out on every duplicate found
checkForDuplicates :: Eq a => (a -> e) -> [a] -> Validation [e] ()
checkForDuplicates mkE arr = case duplicates of
  [] -> Success ()
  e -> Failure $ mkE <$> e
  where
    duplicates = L.nub (arr `sub` L.nub arr)
    sub x y = [a | a <- x, a `notElem` y]

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
  A.Key _ -> pure TKeycode
  A.Variable name -> case lookup name (types ctx) of
    Just ty -> pure ty
    Nothing -> do
      tell [VarNotInScope spanned]
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

-- Runs all the checks on a given template
checkLayerTemplate ::
  A.LayerTemplate ->
  Context ->
  Validation [TemplateCheckError] ()
checkLayerTemplate template ctx = noDuplicates *> keycodeCheck
  where
    keycodeCheck = for_
      (zip [0 ..] $ A.templateKeycodes template)
      \(i, keycode) ->
        if validKeyCode (unspan keycode)
          then Success ()
          else Failure [InvalidTemplateKeycode keycode]

    noDuplicates =
      checkForDuplicates
        DuplicateTemplateKeycode
        (unspan <$> A.templateKeycodes template)

checkStaticLayer ::
  A.StaticLayer ->
  Context ->
  Validation [StaticLayerError] ()
checkStaticLayer layer ctx = lengthCheck *> expressionCheck
  where
    expressionCheck = for_
      (A.staticLayerContents layer)
      \case
        A.WildcardEntry -> pure ()
        A.ExpressionEntry expr -> case execWriter $ checkExpression expr (TSequence, StaticLayerMember expr) ctx of
          [] -> Success ()
          errors -> Failure $ ExpressionMemberError <$> errors
    lengthCheck = case lookupTemplate templateName ctx of
      Failure e -> Failure e
      Success template ->
        if templateLength == layerLength
          then Success ()
          else
            Failure
              [ WrongTemplateLength
                  templateName
                  (intToNatural templateLength)
                  (intToNatural layerLength)
              ]
        where
          templateLength = length (A.templateKeycodes template)
          layerLength = length (A.staticLayerContents layer)
      where
        templateName = A.staticLayerTemplate layer

checkDeclarations ::
  Context ->
  Validation [CheckError] Context
checkDeclarations ctx =
  ctx <$ traverse mainCheck (scope ctx)
  where
    mainCheck ::
      (Spanned T.Text, Spanned A.ToplevelDeclaration) ->
      Validation [CheckError] ()
    mainCheck (name, declaration) =
      case declaration of
        Spanned span (A.LayerTemplate template) ->
          first (map $ TemplateCheckError . Spanned span) $
            checkLayerTemplate template ctx
        Spanned span (A.Layer (A.StaticLayer layer)) ->
          first (map $ StaticLayerError . Spanned span) $
            checkStaticLayer layer ctx
        _ -> pure ()
