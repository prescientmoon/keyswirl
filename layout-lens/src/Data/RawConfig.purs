module LayoutLens.Data.RawConfig where

import LayoutLens.Prelude

import LayoutLens.Data.Config (LayerVisualPosition)
import LayoutLens.Data.Vec2 (Vec2, Radians)

data RawPhysicalActionStep
  = Place
      { offset :: Vec2
      , rotateBy :: Radians
      , rotateAround :: Vec2
      }
  | Point
      { position :: Vec2
      , size :: Vec2
      , rotateBy :: Radians
      , rotateAround :: Vec2
      }

data RawPhysicalStep
  = Block (Array RawPhysicalActionStep)
  | PhysicalAction RawPhysicalActionStep

newtype RawPhysical = RawPhysical (Array RawPhysicalStep)
newtype RawKeySymbol = RawKeySymbol String

newtype RawChord = RawChord
  { from :: Array RawKeySymbol
  , to :: Array RawKeySymbol
  , fill :: Color
  , fontSizeModifier :: Number
  }

newtype RawLayer = RawLayer
  { name :: String
  , textColor :: Maybe Color
  , keys :: Array RawKeySymbol
  }

layerName :: RawLayer -> String
layerName (RawLayer { name }) = name

data RawElement
  = RawLayerGroup (HashMap LayerVisualPosition RawLayer)
  | RawChordGroup (Array RawChord)

newtype RawSection = RawSection
  { columns :: Int
  , elements :: Array RawElement
  }

sectionElements :: RawSection -> Array RawElement
sectionElements (RawSection { elements }) = elements

data RawActionDisplay
  = DisplaySymbol RawKeySymbol
  | DisplayLayerColor

data RawActionEffect
  = LayerSwitch String
  | StickyLayerSwitch String

newtype RawAction = RawAction
  { display :: RawActionDisplay
  , effect :: RawActionEffect
  }

newtype RawConfig = RawConfig
  { physical :: RawPhysical
  , actions :: HashMap String RawAction
  , sections :: Array RawSection
  }

derive instance Eq RawPhysicalActionStep
derive instance Eq RawPhysicalStep
derive instance Eq RawPhysical
derive instance Eq RawKeySymbol
derive instance Eq RawChord
derive instance Eq RawLayer
derive instance Eq RawElement
derive instance Eq RawSection
derive instance Eq RawActionDisplay
derive instance Eq RawActionEffect
derive instance Eq RawAction
derive instance Eq RawConfig

derive instance Generic RawPhysicalActionStep _
derive instance Generic RawPhysicalStep _
derive instance Generic RawPhysical _
derive instance Generic RawKeySymbol _
derive instance Generic RawChord _
derive instance Generic RawLayer _
derive instance Generic RawElement _
derive instance Generic RawSection _
derive instance Generic RawActionDisplay _
derive instance Generic RawActionEffect _
derive instance Generic RawAction _
derive instance Generic RawConfig _

instance Debug RawPhysicalActionStep where
  debug = genericDebug

instance Debug RawPhysicalStep where
  debug = genericDebug

instance Debug RawPhysical where
  debug = genericDebug

instance Debug RawKeySymbol where
  debug = genericDebug

instance Debug RawChord where
  debug = genericDebug

instance Debug RawLayer where
  debug = genericDebug

instance Debug RawElement where
  debug = genericDebug

instance Debug RawActionDisplay where
  debug = genericDebug

instance Debug RawActionEffect where
  debug = genericDebug

instance Debug RawAction where
  debug = genericDebug

instance Debug RawSection where
  debug = genericDebug

instance Debug RawConfig where
  debug = genericDebug

