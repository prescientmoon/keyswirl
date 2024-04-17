module LayoutLens.Data.Geometry where

import LayoutLens.Prelude

import Data.Array as Array
import LayoutLens.Data.Vec2 as V
import LayoutLens.Data.Vec2
  ( AABB(..)
  , Polygon(..)
  , Vec2(..)
  , aabbToPolygon
  , applyTransform
  , boundingBox
  , mapPoints
  , tTranslate
  , vinv
  , vscale
  )

data Attribute = Fill Color | Stroke Color | StrokeWidth Number
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
  = Transform V.Transform Geometry
  | Many (Array Geometry)
  | Text GenericAttributes Attributes String
  | Rect AABB Attributes
  | Path (Array PathStep) Attributes
  | Invisible Geometry

-- Approximate the size of some geometry by fitting a polygon around it
boundingPolygon :: Geometry -> Maybe Polygon
boundingPolygon = case _ of
  Rect aabb _ -> pure $ aabbToPolygon aabb
  Text _ _ _ -> Nothing
  Many array -> foldMap boundingPolygon array
  Invisible g -> boundingPolygon g
  Transform t g -> mapPoints (applyTransform t) <$> boundingPolygon g
  Path steps _ -> foldMap snd $ Array.scanl (points <<< fst) mempty steps
    where
    points :: Vec2 -> PathStep -> Vec2 /\ Maybe Polygon
    points prev = case _ of
      Close -> prev /\ Nothing
      MoveTo a -> a /\ (pure $ Polygon $ pure a)
      LineTo a -> a /\ (pure $ Polygon $ pure a)
      -- This is just an approximation where we fit an AABB around the circle.
      Arc arc -> arc.to /\ pure (aabbToPolygon aabb)
        where
        aabb = AABB
          { position: center <> vinv diagonal
          , size: vscale 2.0 diagonal
          }

        diagonal = Vec2 arc.radius arc.radius
        center = vscale 0.5 $ arc.to <> prev

-- | Add padding around some geometry
pad :: Vec2 -> Geometry -> Geometry
pad padding geometry = case boundingBox <$> boundingPolygon geometry of
  Just (AABB box) -> Many
    [ Transform (tTranslate padding) geometry
    , Invisible $ Rect
        ( AABB
            { position: box.position
            , size: box.size <> vscale 2.0 padding
            }
        )
        []
    ]
  Nothing -> geometry

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
