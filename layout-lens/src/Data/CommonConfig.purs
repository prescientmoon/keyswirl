module LayoutLens.Data.CommonConfig where

import LayoutLens.Prelude

data LayerVisualPosition = Center | TopLeft | TopRight | BottomLeft | BottomRight

data ActionDisplay
  = DisplaySymbol String
  | DisplayLayerColor

data ActionEffect
  = LayerSwitch String
  | StickyLayerSwitch String

newtype Action = Action
  { display :: ActionDisplay
  , effect :: ActionEffect
  }

newtype Chord = Chord
  { from :: Array String
  , to :: Array String
  , fill :: Color
  , fontSizeModifier :: Number
  }

newtype Layer = Layer
  { name :: String
  , textColor :: Maybe Color
  , keys :: Array String
  }

data ConfigElement
  = LayerGroup (HashMap LayerVisualPosition Layer)
  | ChordGroup (Array Chord)

newtype ConfigSection = ConfigSection
  { columns :: Int
  , elements :: Array ConfigElement
  }

layerName :: Layer -> String
layerName (Layer { name }) = name

sectionElements :: ConfigSection -> Array ConfigElement
sectionElements (ConfigSection { elements }) = elements

derive instance Eq LayerVisualPosition
derive instance Eq ActionDisplay
derive instance Eq ActionEffect
derive instance Eq Action
derive instance Eq Chord
derive instance Eq Layer
derive instance Eq ConfigElement
derive instance Eq ConfigSection

derive instance Generic LayerVisualPosition _
derive instance Generic ActionDisplay _
derive instance Generic ActionEffect _
derive instance Generic Action _
derive instance Generic Chord _
derive instance Generic Layer _
derive instance Generic ConfigElement _
derive instance Generic ConfigSection _

instance Debug LayerVisualPosition where
  debug = genericDebug

instance Debug ActionDisplay where
  debug = genericDebug

instance Debug ActionEffect where
  debug = genericDebug

instance Debug Action where
  debug = genericDebug

instance Debug Chord where
  debug = genericDebug

instance Debug Layer where
  debug = genericDebug

instance Debug ConfigElement where
  debug = genericDebug

instance Debug ConfigSection where
  debug = genericDebug

instance Show LayerVisualPosition where
  show = genericShow

instance Hashable LayerVisualPosition where
  hash Center = 0
  hash TopLeft = 1
  hash TopRight = 2
  hash BottomLeft = 3
  hash BottomRight = 4

