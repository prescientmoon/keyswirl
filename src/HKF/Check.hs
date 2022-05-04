module HKF.Check where

import Control.Monad.Writer as W
import Data.Bifunctor
import Data.Foldable (foldl)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text.Metrics (damerauLevenshtein)
import Data.Validation
import GHC.Num (intToNatural)
import HKF.Ast (EType (..), Spanned (..), StaticLayerEntry (ExpressionEntry), VarName, spanOf, unspan)
import qualified HKF.Ast as A
import Relude.Extra (DynamicMap (insert), StaticMap (lookup, member))
import Relude.Extra.Map (keys)
import qualified Relude.String.Reexport as T

data ScopeBinding = MkScopeBinding
  { seNameSpan :: A.Span,
    seType :: EType,
    seValue :: Maybe (Spanned A.ToplevelDeclaration)
  }
  deriving (Show)

data ScopeEntry
  = ScopeBinding ScopeBinding
  | Reddirect VarName
  deriving (Show)

newtype Scope = MkScope
  { scope :: HashMap Text ScopeEntry
  }
  deriving (Show)

data Module = MkModule
  { moduleScope :: Scope,
    publicMembers :: [Text]
  }
  deriving (Show)

data ModuleBridge = MkModuleBridge
  { actualModule :: Spanned A.ModuleName,
    importedNames :: [Text]
  }
  deriving (Show)

data Context = MkContext
  { moduleScopes :: HashMap A.ModuleName Module,
    importedModules :: HashMap A.ModuleName ModuleBridge,
    currentScope :: Scope,
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
        slrtTemplateName :: Spanned VarName
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
      { vnsSource :: Spanned VarName,
        similar :: [Spanned VarName],
        definedLater :: Maybe (Spanned VarName)
      }
  | WrongTemplateLength (Spanned [A.StaticLayerEntry]) (Spanned [Spanned Text]) (Spanned VarName) Natural Natural
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
  | NoSuchExport
      { nseModule :: Spanned A.ModuleName,
        nseName :: Spanned Text
      }
  | ModuleNotFound (Spanned A.ModuleName)
  deriving (Show)

---------- Helpers
mapDetails :: (CheckErrorDetails -> CheckErrorDetails) -> CheckError -> CheckError
mapDetails f (MkCheckError loc details) = MkCheckError loc (f details)

tcError :: CheckError -> Checked ()
tcError = tell . pure

mkCtxTcError :: Context -> CheckErrorDetails -> CheckError
mkCtxTcError ctx = MkCheckError (generalLocation ctx)

ctxTcError :: Context -> CheckErrorDetails -> Checked ()
ctxTcError ctx = tcError . mkCtxTcError ctx

mapScope :: (HashMap Text ScopeEntry -> HashMap Text ScopeEntry) -> Scope -> Scope
mapScope f = MkScope . f . scope

lookupScope :: VarName -> Context -> Maybe ScopeBinding
lookupScope = go False
  where
    go direct (moduleName, name) ctx =
      mightRecurse . lookup name . scope <=< getModuleScope direct moduleName $
        ctx
      where
        mightRecurse (Just (Reddirect to)) = go True to ctx
        mightRecurse (Just (ScopeBinding binding)) = Just binding
        mightRecurse Nothing = Nothing

getModule :: A.ModuleName -> Context -> Maybe Module
getModule name = lookup name . moduleScopes

getModuleScope :: Bool -> Maybe A.ModuleName -> Context -> Maybe Scope
getModuleScope direct Nothing ctx = Just $ currentScope ctx
getModuleScope direct (Just moduleName) ctx =
  if direct
    then fmap publicModuleScope . lookup moduleName . moduleScopes $ ctx
    else publicBridgeScope <=< lookup moduleName . importedModules $ ctx
  where
    publicModuleScope :: Module -> Scope
    publicModuleScope module_ = mapScope (publicOnly $ publicMembers module_) (moduleScope module_)

    publicBridgeScope :: ModuleBridge -> Maybe Scope
    publicBridgeScope (MkModuleBridge reddirect exports) =
      fmap (mapScope (publicOnly exports) . moduleScope) (lookup (unspan reddirect) (moduleScopes ctx))

    publicOnly exports =
      HM.mapMaybeWithKey
        \k v ->
          if k `elem` exports
            then Just v
            else Nothing

lookupTypeOf :: VarName -> Context -> Maybe EType
lookupTypeOf name = fmap seType . lookupScope name

lookupSpanOf :: VarName -> Context -> Maybe A.Span
lookupSpanOf name = fmap seNameSpan . lookupScope name

addNameSpan :: VarName -> Context -> Maybe (Spanned VarName)
addNameSpan name = fmap (flip Spanned name . seNameSpan) . lookupScope name

-- TODO: quide this by the type
similarInScope :: VarName -> Context -> [Spanned VarName]
similarInScope fullName@(moduleName, name) ctx
  | T.length name < 3 = []
  | otherwise =
    mapMaybe (\name -> addNameSpan (moduleName, name) ctx) $
      take 3
        . fmap fst
        . sortBy (on compare snd)
        . mapMaybe similarity
        $ namesInScope moduleName ctx
  where
    namesInScope :: Maybe A.ModuleName -> Context -> [Text]
    namesInScope moduleName = maybe [] (keys . scope) . getModuleScope False moduleName

    similarity :: Text -> Maybe (Text, Int)
    similarity t = if result < 4 then Just (t, result) else Nothing
      where
        result = damerauLevenshtein name t

lookupTyped :: Spanned VarName -> (EType, TypeExpectationReason) -> Context -> Checked (Maybe (Spanned A.ToplevelDeclaration))
lookupTyped spannedName@(Spanned span name) (expected, reason) ctx = case lookupScope name ctx of
  (Just (MkScopeBinding _ ty (Just decl))) -> do
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
  A.Variable spannedName -> case lookupTypeOf (unspan spannedName) ctx of
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
    inner <- inferType body $ extendContext (name, unspan ty, Nothing) ctx
    pure $ TArrow (unspan ty) inner
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

reddirectInScope :: Text -> VarName -> Scope -> Scope
reddirectInScope from to = mapScope (insert from (Reddirect to))

reddirectInCurrentScope :: Text -> VarName -> Context -> Context
reddirectInCurrentScope from to ctx =
  ctx
    { currentScope = reddirectInScope from to (currentScope ctx)
    }

extendScope :: (Spanned Text, EType, Maybe (Spanned A.ToplevelDeclaration)) -> Scope -> Scope
extendScope (Spanned span name, ty, val) ctx =
  ctx
    { scope = insert name (ScopeBinding $ MkScopeBinding span ty val) (scope ctx)
    }

extendContext :: (Spanned Text, EType, Maybe (Spanned A.ToplevelDeclaration)) -> Context -> Context
extendContext (name, ty, val) ctx =
  ctx
    { currentScope = extendScope (name, ty, val) (currentScope ctx)
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
      extendContext (Spanned span name, ty, Nothing) ctx
    withBinder ty _ ctx = ctx

    keycodeValidityCheck = for_ (A.pmKeycodes branch) (`checkKeycode` ctx)

    keycodeDuplicateCheck = checkForSpannedDuplicates mkError keycodes
      where
        keycodes = A.pmKeycodes branch
        mkError instances =
          mkCtxTcError ctx $
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
          mkCtxTcError ctx $
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
          mkCtxTcError ctx $
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

resolveImport :: A.Import -> Context -> Checked Context
resolveImport (A.MkImport path list importAs) ctx = case getModule (unspan path) ctx of
  Nothing -> do
    ctxTcError ctx $
      ModuleNotFound
        path
    pure ctx
  Just module_ -> do
    let exports = publicMembers module_

    for_ list $ traverse_
      \name ->
        if unspan name `notElem` exports
          then ctxTcError ctx $ NoSuchExport path name
          else pure ()

    -- TODO: check for duplicates
    let imports = maybe exports (fmap unspan) list

    case importAs of
      Nothing -> pure $ foldl (flip importOne) ctx imports
        where
          importOne name =
            reddirectInCurrentScope
              name
              (Just (unspan path), name)
      Just importAs ->
        pure $
          ctx
            { importedModules = insert (unspan importAs) newModule $ importedModules ctx
            }
        where
          newModule :: ModuleBridge
          newModule = MkModuleBridge path imports

checkModule :: A.ModuleName -> A.Module -> Context -> Checked Context
checkModule path (A.MkModule (A.MkHeader isUnsafe exports imports) inner) ctx = do
  withImports <- foldM (flip $ resolveImport . unspan) ctx imports
  checked <- checkConfig inner withImports

  for_ (unspan $ A.unConfigExports exports) $ traverse \export -> do
    let name = (Nothing, unspan export)
    unless (isJust $ lookupScope name checked) do
      ctxTcError checked $
        VarNotInScope
          (Spanned (spanOf export) name)
          (similarInScope name checked)
          Nothing
  let exportList = maybe (newNamesInScope checked) (fmap unspan) (unspan $ A.unConfigExports exports)
  let currentModule :: Module
      currentModule = MkModule (currentScope checked) exportList

  pure $
    checked
      { currentScope = MkScope mempty,
        moduleScopes = insert path currentModule (moduleScopes checked)
      }
  where
    newNamesInScope :: Context -> [Text]
    newNamesInScope = keys . HM.filter mapper . scope . currentScope
      where
        mapper (Reddirect _) = False
        mapper _ = True

checkConfig :: A.Config -> Context -> Checked Context
checkConfig (A.MkConfig config) =
  checkNamedDeclarations $
    mapMaybe (traverse toNamed) config
  where
    toNamed (A.NamedConfigEntry name decl) = Just (name, decl)
    toNamed _ = Nothing

checkNamedDeclarations ::
  [Spanned (Spanned Text, Spanned A.ToplevelDeclaration)] ->
  Context ->
  Checked Context
checkNamedDeclarations t ctx = go t (pure ctx)
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
  pure $ extendContext (name, ty, Just decl) ctx
  where
    attemptFixingVNSErrors spannedTarget@(Spanned _ target) = mapDetails \case
      VarNotInScope spannedName@(Spanned _ (Nothing, name)) similar Nothing
        | name == target ->
          VarNotInScope
            spannedName
            similar
            (Just $ fmap (Nothing,) spannedTarget)
      other -> other
