module HKF.IR where

import HKF.Ast (EType, ModuleName, PatternMatchBranch, Span, Spanned (Spanned), VarName)
import Relude.Container.Reexport (HashMap)
import Relude.Extra (DynamicMap (insert), StaticMap (lookup))
import qualified Text.Show

type IRID = Int

data IR = MkIR
  { irExpr :: RawIR,
    irId :: IRID,
    irType :: EType,
    irSpan :: Span
  }
  deriving (Show)

data BuiltIn
  = -- | Placeholder
    TrustMe
  | -- | Adds a layer to the stack
    WithLayer
  | -- | Sets base layer in the stack
    SetBase
  | -- | Splits a sequence into it's up and down components
    Split
  | -- | Let's the next layer handle an action
    Continue
  deriving (Show)

data RawIR
  = Key Text
  | Variable Text
  | Native BuiltIn
  | Call IR IR
  | Lambda Text (Spanned EType) IR
  | Sequence [IR]
  | Chord [IR]
  | PatternMatch IR [(Spanned PatternMatchBranch, IR)]
  deriving (Show)

data IRToplevelEntry = MkToplevelEntry
  { toplevelEntryType :: EType,
    toplevelEntryExpression :: Spanned RawIR
  }

data RawLambda
  = -- | Lambda written by the user
    UserLambda IRScope Text IR
  | -- | Lambda executing a native function
    -- | The IR is only there for error message purpouses
    NativeLambda ((IR, EvalResult) -> EvalM EvalResult)

instance Show RawLambda where
  show (UserLambda a b c) = show (a, b, c)
  show _ = "<Native lambda>"

type Lambda = (IRID, RawLambda)

type LayerStack = [Lambda]

data RuntimeAction
  = PressKey Text
  | ReleaseKey Text
  | Suspended IR
  | EditStack (LayerStack -> LayerStack)
  | ActionContinue

instance Show RuntimeAction where
  show _ = "<RuntimeAction>"

type RuntimeProgram = [RuntimeAction]

type RuntimeSequence = (RuntimeProgram, RuntimeProgram)

type EvalError = (Span, Text)

type EvalM = Either EvalError

type IRScope = HashMap Text EvalResult

data EvalResult
  = ERSequence RuntimeSequence
  | ERLambda Lambda
  deriving (Show)

evaluate :: IRScope -> IR -> EvalM EvalResult
evaluate scope ir = case irExpr ir of
  Key key -> pure $ ERSequence ([PressKey key], [ReleaseKey key])
  Variable name -> case lookup name scope of
    Nothing -> Left (irSpan ir, "No var named " <> name)
    Just something -> pure something
  Lambda arg ty body -> pure $ ERLambda (irId ir, UserLambda scope arg body)
  Chord chord -> do
    steps <- evalMany chord
    pure $ ERSequence (steps >>= fst, steps >>= snd)
  Sequence seq -> do
    steps <- evalMany seq
    case reverse steps of
      [] -> pure $ ERSequence mempty
      h : t -> do
        let down = (reverse t >>= uncurry (<>)) <> fst h
        let up = snd h
        pure $ ERSequence (down, up)
  Native TrustMe -> Left (irSpan ir, "Encountered trustMe")
  Native SetBase -> mkNativeFn ir \(layerIR, layer) -> do
    layerLambda <- extractLambda layerIR layer
    pure $ ERSequence ([EditStack (const [layerLambda])], [])
  Native WithLayer -> mkNativeFn ir \(layerIR, layer) -> do
    layerLambda <- extractLambda layerIR layer
    -- Removes the first occurence of a layer from the stack
    let removeLayer [] = []
        removeLayer (h : t)
          | fst h == irId layerIR = t
          | otherwise = h : removeLayer t
    pure $
      ERSequence
        ( [EditStack (layerLambda :)],
          [EditStack removeLayer]
        )
  --  Splits a sequence into individual up and down actions
  --  TODO: investigate whether using the same id in both places is usnafe
  Native Split -> mkNativeFn ir \(seqIR, seq) ->
    mkNativeFn ir \(callbackIR, callback) -> do
      seq' <- extractSequence seqIR seq
      let down = fst seq'
      let up = snd seq'
      partiallyApplied <- callIr (callbackIR, callback) (seqIR, ERSequence (down, []))
      callIr (callbackIR, partiallyApplied) (seqIR, ERSequence ([], up))
  Call func arg -> do
    func' <- evaluate scope func
    arg' <- evaluate scope arg
    callIr (func, func') (arg, arg')
  _ -> Left (irSpan ir, "unimplemented")
  where
    mkNativeFn for exec = pure $ ERLambda (irId for, NativeLambda exec)

    callIr (funcIR, func) (argIR, arg) =
      extractLambda funcIR func >>= \case
        (_, UserLambda scope argName res) ->
          evaluate (insert argName arg scope) res
        (_, NativeLambda exec) -> exec (argIR, arg)

    extractLambda _ (ERLambda good) = pure good
    extractLambda for (ERSequence _) =
      Left (irSpan for, show for <> " is not a closure")

    extractSequence _ (ERSequence good) = pure good
    extractSequence for (ERLambda _) =
      Left (irSpan for, show for <> " is not a valid sequence")

    evalMany many =
      forM many \step -> do
        evaluated <- evaluate scope step
        extractSequence step evaluated
