module LayoutLens.Data.Vec2 where

import LayoutLens.Prelude

import Data.Array.NonEmpty as NEA
import Partial.Unsafe (unsafePartial)

newtype Radians = Radians Number
data Vec2 = Vec2 Number Number

-- {{{ Base helpers
-- | Multiply by the matrix
-- |   cos Θ  -sin Θ
-- |   sin Θ   cos Θ
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

origin :: Vec2
origin = Vec2 0.0 0.0

-- }}}
-- {{{ Shapes
newtype AABB = AABB { position :: Vec2, size :: Vec2 }
newtype Polygon = Polygon (NonEmptyArray Vec2)

aabbToPolygon :: AABB -> Polygon
aabbToPolygon (AABB aabb@{ size: Vec2 sx sy }) = Polygon
  $ unsafePartial -- Safe because the array always has four elements
  $ fromJust
  $ NEA.fromArray
      [ aabb.position
      , aabb.position <> Vec2 0.0 sy
      , aabb.position <> aabb.size
      , aabb.position <> Vec2 sx 0.0
      ]

-- | The left-inverse of `aabbToPolygon`
boundingBox :: Polygon -> AABB
boundingBox (Polygon points) = AABB
  { position: Vec2 minX minY
  , size: Vec2 (maxX - minX) (maxY - minY)
  }
  where
  minX = NEA.foldl1 min $ x <$> points
  minY = NEA.foldl1 min $ y <$> points
  maxX = NEA.foldl1 max $ x <$> points
  maxY = NEA.foldl1 max $ y <$> points

mapPoints :: (Vec2 -> Vec2) -> (Polygon -> Polygon)
mapPoints f (Polygon points) = Polygon $ f <$> points

aabbCenter :: AABB -> Vec2
aabbCenter (AABB aabb) = aabb.position <> vscale 0.5 aabb.size

originAabb :: Vec2 -> AABB
originAabb size = AABB { position: origin, size }

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

newtype Transform = Transform
  { scale :: Number
  , position :: Vec2
  , rotation :: Radians
  }

-- | Intuitivey, the raw transformation has form
-- |   v ↦ r_Θ(v + a) - a + p,
-- | but rotations are linear, so the above can be rewritten as
-- |   v ↦ r_Θ(v) + (r_Θ(a) - a + p),
-- | which is what this function does.
-- |
-- | The other case works similarly
normalizeScalePreservingTransform
  :: RawScalePreservingTransform -> ScalePreservingTransform
normalizeScalePreservingTransform (RawScalePreservingTransform t) =
  ScalePreservingTransform
    { rotation: t.rotateBy
    , position: t.rotateAround
        <> rotateBy t.rotateBy (t.position <> vinv t.rotateAround)
    }

-- | We want to compose
-- |   f(v) = r_Θ(v) + p_f
-- |   s(v) = r_ϕ(v) + p_s,
-- | which yields
-- |   (s ∘ f)(v) = r_ϕ(r_Θ(v) + p_f) + p_s
-- |              = r_ϕ(r_Θ(v)) + r_ϕ(p_f) + p_s
-- |              = r_(ϕ+Θ)(v) + (r_ϕ(p_f) + p_s)
composeScalePreservingTransforms
  :: ScalePreservingTransform -> ScalePreservingTransform -> ScalePreservingTransform
composeScalePreservingTransforms (ScalePreservingTransform second) (ScalePreservingTransform first) =
  ScalePreservingTransform
    { rotation: first.rotation <> second.rotation
    , position: second.position <> rotateBy second.rotation first.position
    }

forgetScalePreservingStructure :: ScalePreservingTransform -> Transform
forgetScalePreservingStructure (ScalePreservingTransform transform) =
  Transform
    { position: transform.position
    , rotation: transform.rotation
    , scale: 1.0
    }

applyScalePreservingTransform :: ScalePreservingTransform -> Vec2 -> Vec2
applyScalePreservingTransform = forgetScalePreservingStructure >>> applyTransform

applyTransform :: Transform -> Vec2 -> Vec2
applyTransform (Transform transform) v =
  -- Rotations are linear, thus they commute with scaling.
  vscale transform.scale (rotateBy transform.rotation v) <> transform.position

tScale :: Number -> Transform
tScale factor = Transform
  { scale: factor
  , position: origin
  , rotation: mempty
  }

tTranslate :: Vec2 -> Transform
tTranslate position = Transform
  { scale: 1.0
  , rotation: mempty
  , position
  }

-- }}}

derive instance Eq Vec2
derive instance Eq Radians
derive instance Eq AABB
derive instance Eq Polygon
derive instance Eq RawScalePreservingTransform
derive instance Eq ScalePreservingTransform
derive instance Eq Transform
derive instance Generic Vec2 _
derive instance Generic Radians _
derive instance Generic AABB _
derive instance Generic Polygon _
derive instance Generic RawScalePreservingTransform _
derive instance Generic ScalePreservingTransform _
derive instance Generic Transform _

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

instance Debug Transform where
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

derive instance Newtype Radians _
