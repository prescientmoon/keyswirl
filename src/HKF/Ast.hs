module HKF.Ast where

import qualified Data.Text as T
import Text.Megaparsec (SourcePos)
import qualified Text.Show

data Span = Span SourcePos SourcePos
  deriving (Show)

instance Semigroup Span where
  (<>) (Span f t) (Span f' t') = Span (min f f') (max t t')

data Spanned a = Spanned Span a

instance Show a => Show (Spanned a) where
  show (Spanned _ a) = show a

type Expression = Spanned RawExpression

data RawExpression
  = Key T.Text
  | Call Expression [Expression]
  | Variable T.Text
  | -- Array of chords
    Sequence [Expression]
  | -- Array of keys, more or less
    Chord [Expression]
  deriving (Show)

newtype LayerTemplate = MkLayerTemplate
  { templateKeycodes :: [Spanned T.Text]
  }
  deriving (Show)

data StaticLayerEntry
  = ExpressionEntry Expression
  | WildcardEntry
  deriving (Show)

data StaticLayer = MkStaticLayer
  { staticLayerTemplate :: Spanned T.Text,
    staticLayerContents :: [StaticLayerEntry]
  }
  deriving (Show)

data Binder = Named T.Text | Wildcard
  deriving (Show)

data PatternMatchBranch = MkPatternMatchBranch
  { pmKeycodes :: [T.Text],
    pmVars :: [Binder],
    pmRest :: Maybe Binder
  }
  deriving (Show)

newtype ComputeLayer = MkComputeLayer
  { branches :: [(PatternMatchBranch, Expression)]
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
  | Alias Expression -- argument support perhaps?
  deriving (Show)

data UnnamedConfigEntry
  = Input Input
  | Output T.Text
  deriving (Show)

data ConfigEntry
  = NamedConfigEntry (Spanned T.Text) (Spanned ToplevelDeclaration)
  | UnnamedConfigEntry UnnamedConfigEntry
  deriving (Show)

newtype Config = MkConfig [Spanned ConfigEntry]
  deriving (Show)

---------- Helpers
unspan :: Spanned a -> a
unspan (Spanned _ a) = a

spanOf :: Spanned a -> Span
spanOf (Spanned s _) = s
