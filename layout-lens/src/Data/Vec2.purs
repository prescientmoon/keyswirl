module LayoutLens.Data.Vec2 where

import LayoutLens.Prelude

newtype Radians = Radians Number
data Vec2 = Vec2 Number Number

-- {{{ Base helpers
rotateBy :: Radians -> Vec2 -> Vec2
rotateBy (Radians angle) (Vec2 x y) = Vec2 (x * c - y * s) (x * s + y * c)
  where
  c = cos angle
  s = sin angle

vinv :: Vec2 -> Vec2
vinv (Vec2 x y) = Vec2 (-x) (-y)

vscale :: Number -> Vec2 -> Vec2
vscale n (Vec2 x y) = Vec2 (n * x) (n * y)

vsub :: Vec2 -> Vec2 -> Vec2
vsub a b = a <> vinv b

radiansToDegrees :: Radians -> Number
radiansToDegrees (Radians r) = r * 180.0 / pi

x :: Vec2 -> Number
x (Vec2 x _) = x

y :: Vec2 -> Number
y (Vec2 _ y) = y

-- }}}
-- {{{ Shapes
newtype AABB = AABB { position :: Vec2, size :: Vec2 }
newtype Polygon = Polygon (Array Vec2)

aabbToPolygon :: AABB -> Polygon
aabbToPolygon (AABB aabb@{ size: Vec2 sx sy }) = Polygon
  [ aabb.position
  , aabb.position <> Vec2 0.0 sy
  , aabb.position <> aabb.size
  , aabb.position <> Vec2 sx 0.0
  ]

mapPoints :: (Vec2 -> Vec2) -> (Polygon -> Polygon)
mapPoints f (Polygon points) = Polygon $ f <$> points

aabbCenter :: AABB -> Vec2
aabbCenter (AABB aabb) = aabb.position <> vscale 0.5 aabb.size

-- }}}
-- {{{ Transforms
newtype RawScalePreservingTransform = RawScalePreservingTransform
  { position :: Vec2
  , rotateBy :: Radians
  , rotateAround :: Vec2
  }

newtype ScalePreservingTransform = ScalePreservingTransform
  { position :: Vec2
  , rotation :: Radians
  }

normalizeScalePreservingTransform
  :: RawScalePreservingTransform -> ScalePreservingTransform
normalizeScalePreservingTransform (RawScalePreservingTransform t) = ScalePreservingTransform
  { rotation: t.rotateBy
  , position: t.rotateAround <> rotateBy t.rotateBy (vsub t.position t.rotateAround)
  }

composeScalePreservingTransforms
  :: ScalePreservingTransform -> ScalePreservingTransform -> ScalePreservingTransform
composeScalePreservingTransforms (ScalePreservingTransform first) (ScalePreservingTransform second) =
  ScalePreservingTransform
    { rotation: first.rotation <> second.rotation
    , position: second.position <> rotateBy second.rotation first.position
    }

applyScalePreservingTransform :: ScalePreservingTransform -> Vec2 -> Vec2
applyScalePreservingTransform (ScalePreservingTransform transform) v =
  rotateBy transform.rotation v <> transform.position

-- }}}

derive instance Eq Vec2
derive instance Eq Radians
derive instance Eq AABB
derive instance Eq Polygon
derive instance Eq RawScalePreservingTransform
derive instance Eq ScalePreservingTransform
derive instance Generic Vec2 _
derive instance Generic Radians _
derive instance Generic AABB _
derive instance Generic Polygon _
derive instance Generic RawScalePreservingTransform _
derive instance Generic ScalePreservingTransform _

instance Debug Vec2 where
  debug = genericDebug

instance Debug Radians where
  debug = genericDebug

instance Debug AABB where
  debug = genericDebug

instance Debug Polygon where
  debug = genericDebug

instance Debug RawScalePreservingTransform where
  debug = genericDebug

instance Debug ScalePreservingTransform where
  debug = genericDebug

instance Semigroup Vec2 where
  append (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)

instance Semigroup Radians where
  append (Radians a) (Radians b) = Radians (a + b)

derive newtype instance Semigroup Polygon

instance Monoid Vec2 where
  mempty = Vec2 0.0 0.0

instance Monoid Radians where
  mempty = Radians 0.0

derive newtype instance Monoid Polygon
