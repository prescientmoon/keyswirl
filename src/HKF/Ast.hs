module HKF.Ast where

import qualified Data.Text as T

data Expression
  = Key T.Text
  | Call Expression [Expression]
  | Variable T.Text
  deriving (Show)

newtype LayerTemplate = MkLayerTemplate
  { templateKeycodes :: [T.Text]
  }
  deriving (Show)

data StaticLayerEntry
  = ExpressionEntry Expression
  | WildcardEntry
  deriving (Show)

data StaticLayer = MkStaticLayer
  { staticLayerTemplate :: T.Text,
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
  = NamedConfigEntry T.Text ToplevelDeclaration
  | UnnamedConfigEntry UnnamedConfigEntry
  deriving (Show)

newtype Config = MkConfig [ConfigEntry]
  deriving (Show)
