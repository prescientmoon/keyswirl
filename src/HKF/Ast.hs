module HKF.Ast where

import qualified Data.Text as T
import Error.Diagnose (Position (Position))
import Error.Diagnose.Position (Position)
import Relude.Extra.Map (DynamicMap (insert))
import Text.Megaparsec (SourcePos)
import qualified Text.Show

type VarName = (Maybe ModuleName, Text)

type Span = Position

data Spanned a = Spanned Span a
  deriving (Functor, Foldable, Traversable)

instance Show a => Show (Spanned a) where
  show (Spanned _ a) = show a

type Expression = Spanned RawExpression

type ModuleName = Text

data RawExpression
  = Key (Spanned Text)
  | Call Expression [Expression]
  | Variable (Spanned VarName)
  | -- Functions
    Lambda (Spanned Text) (Spanned EType) Expression
  | -- Array of keys pressed and released individually in order
    Sequence [Expression]
  | -- Array of keys first all pressed down and then all released
    Chord [Expression]
  | -- Type annotation coming from the user
    Annotation (Spanned EType) Expression
  deriving (Show)

newtype LayerTemplate = MkLayerTemplate
  { templateKeycodes :: Spanned [Spanned T.Text]
  }
  deriving (Show)

data StaticLayerEntry
  = ExpressionEntry Expression
  | WildcardEntry
  deriving (Show)

data StaticLayer = MkStaticLayer
  { staticLayerTemplate :: Spanned VarName,
    staticLayerContents :: Spanned [StaticLayerEntry]
  }
  deriving (Show)

data Binder = Named T.Text | Wildcard
  deriving (Show)

data PatternMatchBranch = MkPatternMatchBranch
  { pmKeycodes :: [Spanned Text],
    pmVars :: [Spanned Binder],
    pmRest :: Maybe (Spanned Binder)
  }
  deriving (Show)

newtype ComputeLayer = MkComputeLayer
  { branches :: [(Spanned PatternMatchBranch, Expression)]
  }
  deriving (Show)

-- Layers are just functions mapping multiple keypresses to a single keypress
data Layer
  = ComputeLayer ComputeLayer
  | StaticLayer StaticLayer
  deriving (Show)

data Input
  = InputByName T.Text
  | InputByPath T.Text
  deriving (Show)

data ToplevelDeclaration
  = Layer Layer
  | LayerTemplate LayerTemplate
  | Alias Expression
  | Assumption (Spanned EType)
  deriving (Show)

data UnnamedConfigEntry
  = Input Input
  | Output T.Text
  deriving (Show)

data ConfigEntry
  = NamedConfigEntry (Spanned T.Text) (Spanned ToplevelDeclaration)
  | UnnamedConfigEntry UnnamedConfigEntry
  deriving (Show)

data Import = MkImport
  { importPath :: Spanned ModuleName,
    importList :: Maybe [Spanned Text],
    importAs :: Maybe (Spanned ModuleName)
  }
  deriving (Show)

newtype Exports = MkExports
  { unConfigExports :: Spanned (Maybe [Spanned Text])
  }
  deriving (Show)

data Header = MkHeader
  { moduleIsUnsafe :: Bool,
    exports :: Exports,
    imports :: [Spanned Import]
  }
  deriving (Show)

data Module = MkModule
  { configHeader :: Header,
    config :: Config
  }
  deriving (Show)

newtype Config = MkConfig [Spanned ConfigEntry]
  deriving (Show)

newtype CompleteConfig = MkCompleteConfig
  { modules :: HashMap ModuleName Module
  }
  deriving (Show)

data EType
  = TBroken -- eg: we can't infer a type because the underlying declaration errors out somewhere else
  | TKeycode -- in a way, this is a chord with one element
  | TChord -- in a way, this is a sequence with one element
  | TSequence -- [Chord], more or less
  | TTemplate -- type of templates
  | TArrow EType EType
  deriving (Show)

---------- Helpers
unspan :: Spanned a -> a
unspan (Spanned _ a) = a

spanOf :: Spanned a -> Span
spanOf (Spanned s _) = s

span :: (Int, Int) -> (Int, Int) -> FilePath -> Span
span = Position

binderText :: Binder -> Maybe Text
binderText Wildcard = Nothing
binderText (Named name) = Just name

mergeManySpans :: [Span] -> Maybe Span
mergeManySpans [] = Nothing
mergeManySpans [x] = Just x
mergeManySpans (h : t) = mergeManySpans t >>= mergeSpans h

mergeSpans' :: Span -> Span -> Span
mergeSpans' a b = fromMaybe a (mergeSpans a b)

mergeSpans :: Span -> Span -> Maybe Span
mergeSpans (Position s e f) (Position s' e' f')
  | f == f' =
    Just $
      Position
        (mergeSourcePositions min s s')
        (mergeSourcePositions max e e')
        f
  | otherwise = Nothing
  where
    mergeSourcePositions :: (Int -> Int -> Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
    mergeSourcePositions f (l0, c0) (l1, c1)
      | l0 == l1 = (l0, f c0 c1)
      | otherwise =
        if f l0 l1 == l0
          then (l0, c0)
          else (l1, c1)

addModule :: ModuleName -> Module -> CompleteConfig -> CompleteConfig
addModule k v c = c {modules = insert k v $ modules c}

unqualifyName :: (ModuleName, Text) -> Text
unqualifyName (a, b) = a <> "." <> b
