module LayoutLens.Data.Vec2 where

import LayoutLens.Prelude

newtype Radians = Radians Number

data Vec2 = Vec2 Number Number

derive instance Eq Vec2
derive instance Eq Radians
derive instance Generic Vec2 _
derive instance Generic Radians _

instance Debug Vec2 where
  debug = genericDebug

instance Debug Radians where
  debug = genericDebug
