module LayoutLens.Data.RawConfig where

import LayoutLens.Prelude

import LayoutLens.Data.CommonConfig (Action, ConfigSection)
import LayoutLens.Data.Vec2 (RawScalePreservingTransform, Vec2)

data RawPhysicalActionStep
  = Place RawScalePreservingTransform
  | Point
      { size :: Vec2
      , transform :: RawScalePreservingTransform
      }

data RawPhysicalStep
  = Block (Array RawPhysicalActionStep)
  | PhysicalAction RawPhysicalActionStep

newtype RawPhysical = RawPhysical (Array RawPhysicalStep)

newtype RawConfig = RawConfig
  { physical :: RawPhysical
  , actions :: HashMap String Action
  , sections :: Array ConfigSection
  }

derive instance Eq RawPhysicalActionStep
derive instance Eq RawPhysicalStep
derive instance Eq RawPhysical
derive instance Eq RawConfig

derive instance Generic RawPhysicalActionStep _
derive instance Generic RawPhysicalStep _
derive instance Generic RawPhysical _
derive instance Generic RawConfig _

instance Debug RawPhysicalActionStep where
  debug = genericDebug

instance Debug RawPhysicalStep where
  debug = genericDebug

instance Debug RawPhysical where
  debug = genericDebug

instance Debug RawConfig where
  debug = genericDebug

