module LayoutLens.Data.Geometry where

import LayoutLens.Prelude

import Data.Array as Array
import LayoutLens.Data.Vec2
  ( AABB(..)
  , Polygon(..)
  , ScalePreservingTransform
  , Vec2(..)
  , aabbToPolygon
  , applyScalePreservingTransform
  , mapPoints
  , vinv
  , vscale
  )

data Attribute = Fill Color | Stroke Color
type Attributes = Array Attribute
type GenericAttributes = Array (String /\ String)

type Arc =
  { radius :: Number
  , to :: Vec2
  }

data PathStep
  = MoveTo Vec2
  | LineTo Vec2
  | Arc Arc
  | Close

data Geometry
  = Transform ScalePreservingTransform Geometry
  | Many (Array Geometry)
  | Text GenericAttributes Attributes String
  | Rect AABB Attributes
  | Path (Array PathStep) Attributes

-- Approximate the size of some geometry by fitting a polygon around it
boundingPolygon :: Geometry -> Polygon
boundingPolygon = case _ of
  Rect aabb _ -> aabbToPolygon aabb
  Text _ _ _ -> mempty
  Many array -> foldMap boundingPolygon array
  Transform t g -> mapPoints (applyScalePreservingTransform t) $ boundingPolygon g
  Path steps _ -> foldMap snd $ Array.scanl (points <<< fst) mempty steps
    where
    points :: Vec2 -> PathStep -> Vec2 /\ Polygon
    points prev = case _ of
      Close -> prev /\ Polygon []
      MoveTo a -> a /\ Polygon [ a ]
      LineTo a -> a /\ Polygon [ a ]
      -- This is just an approximation where we fit an AABB around the circle.
      Arc arc -> arc.to /\ aabbToPolygon aabb
        where
        aabb = AABB
          { position: center <> vinv diagonal
          , size: vscale 2.0 diagonal
          }

        diagonal = Vec2 arc.radius arc.radius
        center = vscale 0.5 $ arc.to <> prev

derive instance Eq Attribute
derive instance Eq PathStep
derive instance Eq Geometry
derive instance Generic Attribute _
derive instance Generic PathStep _
derive instance Generic Geometry _

instance Debug Attribute where
  debug = genericDebug

instance Debug PathStep where
  debug = genericDebug

instance Debug Geometry where
  debug a = genericDebug a
