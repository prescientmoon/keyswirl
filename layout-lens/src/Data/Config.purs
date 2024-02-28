module LayoutLens.Data.Config
  ( PhysicalKey(..)
  , PhysicalLayout(..)
  , LensConfig(..)
  , buildConfig
  , buildPhysical
  ) where

import LayoutLens.Prelude

import Data.Array as Array
import LayoutLens.Data.CommonConfig (Action, ConfigSection)
import LayoutLens.Data.RawConfig
  ( RawConfig(..)
  , RawPhysical(..)
  , RawPhysicalActionStep(..)
  , RawPhysicalStep(..)
  )
import LayoutLens.Data.Vec2
  ( ScalePreservingTransform
  , Vec2
  , composeScalePreservingTransforms
  , normalizeScalePreservingTransform
  )

-- {{{ Physical layouts
newtype PhysicalKey = PhysicalKey
  { transform :: ScalePreservingTransform
  , size :: Vec2
  }

newtype PhysicalLayout = PhysicalLayout (Array PhysicalKey)

type PhysicalExecutionStep = { block :: Array PhysicalKey, keys :: Array PhysicalKey }

transformKey :: ScalePreservingTransform -> PhysicalKey -> PhysicalKey
transformKey transform (PhysicalKey key) = PhysicalKey
  { size: key.size
  , transform: composeScalePreservingTransforms key.transform transform
  }

buildPhysical :: RawPhysical -> PhysicalLayout
buildPhysical (RawPhysical steps) = PhysicalLayout $ _.keys =<< final
  where
  final :: Array PhysicalExecutionStep
  final = Array.scanl (loop <<< _.block) initial steps

  initial :: PhysicalExecutionStep
  initial = { block: [], keys: [] }

  execStep :: Array PhysicalKey -> RawPhysicalActionStep -> Array PhysicalKey
  execStep block = case _ of
    Point { size, transform } -> pure $ PhysicalKey
      { size
      , transform: normalizeScalePreservingTransform transform
      }
    Place transform -> block <#> transformKey (normalizeScalePreservingTransform transform)

  loop :: Array PhysicalKey -> RawPhysicalStep -> PhysicalExecutionStep
  loop block = case _ of
    Block actions -> { block: actions >>= execStep block, keys: [] }
    PhysicalAction action -> { block, keys: execStep block action }

-- }}}
-- {{{ Config
newtype LensConfig = LensConfig
  { physical :: PhysicalLayout
  , actions :: HashMap String Action
  , sections :: Array ConfigSection
  }

buildConfig :: RawConfig -> LensConfig
buildConfig (RawConfig config) = LensConfig
  { physical: buildPhysical config.physical
  , actions: config.actions
  , sections: config.sections
  }

-- }}}

derive instance Eq PhysicalKey
derive instance Eq PhysicalLayout
derive instance Eq LensConfig
derive instance Generic PhysicalKey _
derive instance Generic PhysicalLayout _
derive instance Generic LensConfig _

instance Debug PhysicalKey where
  debug = genericDebug

instance Debug PhysicalLayout where
  debug = genericDebug

instance Debug LensConfig where
  debug = genericDebug
