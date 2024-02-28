module LayoutLens.Data.Svg where

import LayoutLens.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import LayoutLens.Data.Geometry (Geometry(..), Attribute(..), Attributes, GenericAttributes, PathStep(..))
import LayoutLens.Data.Vec2 (AABB(..), ScalePreservingTransform(..), Vec2(..), radiansToDegrees, x, y)

indent :: String -> String
indent = split (Pattern "\n") >>> map ("  " <> _) >>> unlines

tag :: String -> GenericAttributes -> String -> String
tag name attributes child = fold
  [ "<"
  , name
  , " "
  , joinWith " "
      $ uncurry (\key value -> fold [ key, "=\"", value, "\"" ])
      <$> attributes
  , ">"
  , indent child
  , "</"
  , name
  , ">"
  ]

leaf :: String -> GenericAttributes -> String
leaf name attributes = tag name attributes mempty

printGeometryAttributes :: Attributes -> GenericAttributes
printGeometryAttributes = map case _ of
  Fill color -> "fill" /\ toHexString color
  Stroke color -> "stroke" /\ toHexString color

px :: Number -> String
px n = show n <> "px"

-- Render a geometry to svg
renderGeometry :: Geometry -> String
renderGeometry = case _ of
  Many array -> unlines $ renderGeometry <$> array

  Rect (AABB aabb) proper ->
    leaf "rect"
      $ printGeometryAttributes proper
      <>
        [ "x" /\ show (x aabb.position)
        , "y" /\ show (y aabb.position)
        , "width" /\ px (x aabb.size)
        , "height" /\ px (y aabb.size)
        ]

  Path steps proper ->
    leaf "path" $
      Array.snoc
        (printGeometryAttributes proper)
        ("d" /\ joinWith " " (mkStep <$> steps))
    where
    vec2 (Vec2 x y) = show x <> " " <> show y
    mkStep = case _ of
      MoveTo p -> "M " <> vec2 p
      LineTo p -> "L " <> vec2 p
      Arc arc -> joinWith " "
        [ "A"
        , show arc.radius
        , show arc.radius
        , "0"
        , "0"
        , "0"
        , vec2 arc.to
        ]
      Close -> "Z"

  Text generic proper string ->
    tag
      "text"
      (generic <> printGeometryAttributes proper)
      string

  Transform (ScalePreservingTransform transform) g ->
    tag "g"
      [ "transform" /\ fold
          [ "rotate("
          , show $ radiansToDegrees transform.rotation
          , ")"
          ]
      ]
      $ renderGeometry g
