module HKF.Check where

import Control.Monad.Writer as W
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text.Metrics (damerauLevenshtein)
import Data.Validation
import GHC.Num (intToNatural)
import HKF.Ast (EType (..), Spanned (..), StaticLayerEntry (ExpressionEntry), spanOf, unspan)
import qualified HKF.Ast as A
import Relude.Extra (DynamicMap (insert), StaticMap (lookup, member))
import Relude.Extra.Map (keys)
import qualified Relude.String.Reexport as T

data Context = MkContext
  { scope :: HashMap Text (Spanned A.ToplevelDeclaration),
    types :: HashMap Text EType,
    nameSpans :: HashMap Text A.Span,
    generalLocation :: Maybe (Spanned CheckErrorGeneralLocation)
  }
  deriving (Show)

type Checked = Writer [CheckError]

data TypeExpectationReason
  = MustSatisfyChord
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
        slrtTemplateName :: Spanned Text
      }
  | ComputeLayerMember
      { clmExpression :: A.Expression
      }
  | AnnotationSaidSo
      { assAnnotation :: A.Expression,
        assInner :: A.Expression,
        assType :: Spanned EType
      }
  deriving (Show)

data CheckError = MkCheckError
  { ceGeneralLocation :: Maybe (Spanned CheckErrorGeneralLocation),
    ceDetails :: CheckErrorDetails
  }
  deriving (Show)

data CheckErrorGeneralLocation
  = WhileCheckingStaticLayer Text
  | WhileCheckingComputeLayer Text
  | WhileCheckingLayerTemplate Text
  | WhileCheckingAlias Text
  deriving (Show)

data CheckErrorDetails
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
        waExpected :: EType,
        waActual :: EType,
        waContradictions :: [(EType, EType)]
      }
  | NotCallable
      { ncFunction :: A.Expression,
        ncArgument :: A.Expression,
        typeofFunction :: EType,
        typeofArgument :: EType
      }
  | VarNotInScope
      { vnsSource :: Spanned Text,
        similar :: [Spanned Text],
        definedLater :: Maybe (Spanned Text)
      }
  | WrongTemplateLength (Spanned [A.StaticLayerEntry]) (Spanned [Spanned Text]) (Spanned Text) Natural Natural
  | DuplicateTemplateKeycode
      { dtkTemplate :: Spanned [Spanned Text],
        dtkInstances :: NonEmpty (Spanned Text),
        dtkKeycode :: Text
      }
  | DuplicatePatternBinder
      { dpbBranch :: Spanned A.PatternMatchBranch,
        dpbInstances :: NonEmpty (Spanned Text),
        dpbName :: Text
      }
  | DuplicatePatternKeycode
      { dpkBranch :: Spanned A.PatternMatchBranch,
        dpkInstances :: NonEmpty (Spanned Text),
        dpkKeycode :: Text
      }
  | InvalidKeycode (Spanned Text)
  deriving (Show)

---------- Helpers
mapDetails :: (CheckErrorDetails -> CheckErrorDetails) -> CheckError -> CheckError
mapDetails f (MkCheckError loc details) = MkCheckError loc (f details)

tcError :: CheckError -> Checked ()
tcError = tell . pure

ctxTcError :: Context -> CheckErrorDetails -> Checked ()
ctxTcError ctx = tcError . MkCheckError (generalLocation ctx)

lookupScope :: Text -> Context -> Maybe (Spanned A.ToplevelDeclaration)
lookupScope name ctx = lookup name (scope ctx)

lookupTypeScope :: Text -> Context -> Maybe EType
lookupTypeScope name ctx = lookup name (types ctx)

-- TODO: quide this by the type
similarInScope :: Text -> Context -> [Spanned Text]
similarInScope name ctx
  | T.length name < 3 = []
  | otherwise = mapMaybe fetchSpan $ take 3 $ fmap fst $ sortBy (on compare snd) $ mapMaybe similarity $ keys (types ctx)
  where
    similarity t = if result < 3 then Just (t, result) else Nothing
      where
        result = damerauLevenshtein name t
    fetchSpan t = lookup t (nameSpans ctx) <&> \span -> Spanned span t

lookupTyped :: Spanned Text -> (EType, TypeExpectationReason) -> Context -> Checked (Maybe (Spanned A.ToplevelDeclaration))
lookupTyped spannedName@(Spanned span name) (expected, reason) ctx = case (lookupScope name ctx, lookupTypeScope name ctx) of
  (Just decl, Just ty) -> do
    let contradictions = subtype ty expected
    unless (L.null contradictions) do
      ctxTcError ctx $
        WrongType
          expected
          ty
          (Spanned span $ A.Variable spannedName)
          contradictions
          reason

    pure $ Just decl
  _ -> do
    ctxTcError ctx $ VarNotInScope spannedName (similarInScope name ctx) Nothing
    pure Nothing

validKeyCode :: Text -> Bool
validKeyCode "wrong" = False
validKeyCode _ = True

---------- Validators
-- Validates a list and errors out on every duplicate found
checkForDuplicates :: (a -> a -> Bool) -> (NonEmpty a -> CheckError) -> [a] -> Checked ()
checkForDuplicates eq mkE arr = do
  unless (L.null duplicates) do
    for_ duplicates \d -> case nonEmpty d of
      Nothing -> pure ()
      Just d -> tcError (mkE d)
  where
    duplicates = L.nubBy arraysEq [d | a <- arr, let d = [b | b <- arr, eq a b], length d > 1]
    arraysEq [] [] = True
    arraysEq (a : _) (b : _) = eq a b
    arraysEq _ _ = False

checkForSpannedDuplicates :: Eq a => (NonEmpty (Spanned a) -> CheckError) -> [Spanned a] -> Checked ()
checkForSpannedDuplicates = checkForDuplicates (on (==) unspan)

-- Ensures the type of a is less general than the type of b
subtype :: EType -> EType -> [(EType, EType)]
subtype left right = case (left, right) of
  (TTemplate, TTemplate) -> trivial
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
  A.Key keycode -> checkKeycode keycode ctx $> TKeycode
  A.Variable spannedName -> case lookup (unspan spannedName) (types ctx) of
    Just ty -> pure ty
    Nothing -> do
      ctxTcError ctx $ VarNotInScope spannedName (similarInScope (unspan spannedName) ctx) Nothing
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
        go (Spanned (fromMaybe (spanOf f) $ A.mergeSpans (spanOf f) (spanOf h)) $ A.Call f [h]) oType t
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
  A.Annotation ty inner -> do
    let reason = AnnotationSaidSo spanned inner ty
    checkExpression inner (unspan ty, reason) ctx
    pure (unspan ty)
  A.Lambda name ty body -> do
    inferType body $ extendContext (name, unspan ty) ctx
  where
    inferSingleCall func fType arg = do
      aType <- inferType arg ctx
      case fType of
        TBroken -> pure TBroken -- oof, we really fucked up by getting here
        TArrow from to -> do
          let contradictions = subtype aType from
          unless (L.null contradictions) do
            let error = WrongArgument func arg from aType contradictions
            ctxTcError ctx error
          pure to
        somethingElse -> do
          let error = NotCallable func arg fType aType
          ctxTcError ctx error
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
      ctxTcError ctx error

extendContext :: (Spanned Text, EType) -> Context -> Context
extendContext (Spanned span name, ty) ctx =
  ctx
    { types = insert name ty (types ctx),
      nameSpans = insert name span (nameSpans ctx)
    }

checkPatternMatchBranch ::
  (Spanned A.PatternMatchBranch, A.Expression) ->
  Context ->
  Checked ()
checkPatternMatchBranch (this@(Spanned _ branch), inner) ctx = do
  keycodeValidityCheck
  keycodeDuplicateCheck
  binderDuplicateCheck

  let ctx' = foldr (withBinder TKeycode) withRest (A.pmVars branch)
  checkExpression inner (TSequence, reason) ctx'
  where
    reason = ComputeLayerMember inner

    withRest = case A.pmRest branch of
      Nothing -> ctx
      Just binder -> withBinder TChord binder ctx

    withBinder :: EType -> Spanned A.Binder -> Context -> Context
    withBinder ty (Spanned span (A.Named name)) ctx =
      extendContext (Spanned span name, ty) ctx
    withBinder ty _ ctx = ctx

    keycodeValidityCheck = for_ (A.pmKeycodes branch) (`checkKeycode` ctx)

    keycodeDuplicateCheck = checkForSpannedDuplicates mkError keycodes
      where
        keycodes = A.pmKeycodes branch
        mkError instances =
          MkCheckError (generalLocation ctx) $
            DuplicatePatternKeycode
              this
              instances
              (unspan $ head instances)

    binderDuplicateCheck = checkForSpannedDuplicates mkError binders
      where
        binders =
          mapMaybe
            (traverse A.binderText)
            $ A.pmVars branch
              <> maybeToList (A.pmRest branch)

        mkError instances =
          MkCheckError (generalLocation ctx) $
            DuplicatePatternBinder
              this
              instances
              (unspan $ head instances)

checkComputeLayer :: Spanned A.ComputeLayer -> Context -> Checked ()
checkComputeLayer (Spanned _ layer) ctx = for_ (A.branches layer) (`checkPatternMatchBranch` ctx)

checkKeycode :: Spanned Text -> Context -> Checked ()
checkKeycode keycode ctx =
  unless (validKeyCode (unspan keycode)) do
    ctxTcError ctx $ InvalidKeycode keycode

-- Runs all the checks on a given template
checkLayerTemplate ::
  Spanned A.LayerTemplate ->
  Context ->
  Checked ()
checkLayerTemplate this@(Spanned _ template) ctx = noDuplicates *> keycodeCheck
  where
    keycodeCheck =
      for_
        (unspan $ A.templateKeycodes template)
        (`checkKeycode` ctx)

    noDuplicates =
      checkForSpannedDuplicates
        mkError
        (unspan $ A.templateKeycodes template)
      where
        mkError duplicates =
          MkCheckError (generalLocation ctx) $
            DuplicateTemplateKeycode
              (A.templateKeycodes template)
              duplicates
              (unspan $ head duplicates)

checkStaticLayer ::
  Spanned A.StaticLayer ->
  Context ->
  Checked ()
checkStaticLayer this@(Spanned _ layer) ctx = lengthCheck *> expressionCheck
  where
    expressionCheck = for_
      (unspan $ A.staticLayerContents layer)
      \case
        A.WildcardEntry -> pure ()
        A.ExpressionEntry expr -> checkExpression expr (TSequence, StaticLayerMember expr) ctx

    lengthCheck = do
      let templateName = A.staticLayerTemplate layer
      res <- lookupTyped templateName (TTemplate, StaticLayersRequireTemplate this templateName) ctx
      case res of
        Just (Spanned _ (A.LayerTemplate template)) ->
          unless (templateLength == layerLength) do
            ctxTcError
              ctx
              $ WrongTemplateLength
                (A.staticLayerContents layer)
                (A.templateKeycodes template)
                templateName
                (intToNatural templateLength)
                (intToNatural layerLength)
          where
            templateLength = length (unspan $ A.templateKeycodes template)
            layerLength = length (unspan $ A.staticLayerContents layer)
        _ -> pure ()

checkConfig ::
  [Spanned (Spanned Text, Spanned A.ToplevelDeclaration)] ->
  Context ->
  Checked Context
checkConfig t ctx = go t (pure ctx)
  where
    go [] ctx = ctx
    go (h : t) ctx = go t $ checkDeclaration h ctx

checkDeclaration ::
  Spanned (Spanned Text, Spanned A.ToplevelDeclaration) ->
  Checked Context ->
  Checked Context
checkDeclaration (Spanned wholeSpan (name, decl)) continuation = do
  ctx <- censor (fmap $ attemptFixingVNSErrors name) continuation
  let span = spanOf decl
  let tLayer = TArrow TChord TSequence
  let withLocation loc = ctx {generalLocation = Just $ Spanned (spanOf name) (loc $ unspan name)}

  ty <- case unspan decl of
    A.LayerTemplate template -> do
      checkLayerTemplate (Spanned span template) $ withLocation WhileCheckingLayerTemplate
      pure TTemplate
    A.Layer (A.StaticLayer layer) -> do
      checkStaticLayer (Spanned span layer) $ withLocation WhileCheckingStaticLayer
      pure tLayer
    A.Layer (A.ComputeLayer layer) -> do
      checkComputeLayer (Spanned span layer) $ withLocation WhileCheckingComputeLayer
      pure tLayer
    A.Alias expr -> do
      inferType expr $ withLocation WhileCheckingAlias
    A.Assumption ty -> do
      pure (unspan ty)
  pure $
    ctx
      { scope = insert (unspan name) decl (scope ctx),
        types = insert (unspan name) ty (types ctx),
        nameSpans = insert (unspan name) (spanOf name) (nameSpans ctx),
        generalLocation = Nothing
      }
  where
    attemptFixingVNSErrors target = mapDetails \case
      VarNotInScope name similar Nothing | unspan name == unspan target -> VarNotInScope name similar (Just target)
      other -> other
