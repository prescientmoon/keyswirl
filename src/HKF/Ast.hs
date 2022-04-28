module HKF.Ast where

import qualified Data.Text as T
import Error.Diagnose (Position (Position))
import Error.Diagnose.Position (Position)
import Text.Megaparsec (SourcePos)
import qualified Text.Show

type Span = Position

data Spanned a = Spanned Span a
  deriving (Functor)

instance Show a => Show (Spanned a) where
  show (Spanned _ a) = show a

type Expression = Spanned RawExpression

data RawExpression
  = Key (Spanned T.Text)
  | Call Expression [Expression]
  | Variable (Spanned T.Text)
  | -- Array of keys pressed and released individually in order
    Sequence [Expression]
  | -- Array of keys first all pressed down and then all released
    Chord [Expression]
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
  { staticLayerTemplate :: Spanned T.Text,
    staticLayerContents :: Spanned [StaticLayerEntry]
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

span :: (Int, Int) -> (Int, Int) -> FilePath -> Span
span = Position

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
