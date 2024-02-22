module LayoutLens.Data.Config where

import LayoutLens.Prelude

data LayerVisualPosition = Center | TopLeft | TopRight | BottomLeft | BottomRight

derive instance Eq LayerVisualPosition
derive instance Generic LayerVisualPosition _

instance Debug LayerVisualPosition where
  debug = genericDebug

instance Show LayerVisualPosition where
  show = genericShow

instance Hashable LayerVisualPosition where
  hash Center = 0
  hash TopLeft = 1
  hash TopRight = 2
  hash BottomLeft = 3
  hash BottomRight = 4
