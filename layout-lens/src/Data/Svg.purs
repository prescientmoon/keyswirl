module LayoutLens.Data.Svg where

import LayoutLens.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import LayoutLens.Data.Geometry (Attribute(..), Attributes, GenericAttributes, Geometry(..), PathStep(..), boundingPolygon)
import LayoutLens.Data.Vec2 (AABB(..), Vec2(..), boundingBox, x, y)
import LayoutLens.Data.Vec2 as V

indent :: String -> String
indent = split (Pattern "\n") >>> map ("  " <> _) >>> unlines

printAttributes :: GenericAttributes -> String
printAttributes attributes = joinWith " "
  $ uncurry (\key value -> fold [ key, "=\"", value, "\"" ])
  <$> attributes

tag :: String -> GenericAttributes -> String -> String
tag name attributes child = fold
  [ "<"
  , name
  , " "
  , printAttributes attributes
  , ">"
  , indent child
  , "</"
  , name
  , ">"
  ]

leaf :: String -> GenericAttributes -> String
leaf name attributes = fold
  [ "<"
  , name
  , " "
  , printAttributes attributes
  , "/>"
  ]

printGeometryAttributes :: Attributes -> GenericAttributes
printGeometryAttributes = map case _ of
  Fill color -> "fill" /\ toHexString color
  Stroke color -> "stroke" /\ toHexString color
  StrokeWidth number -> "stroke-width" /\ show number

px :: Number -> String
px n = show n <> "px"

-- Render a geometry to svg
renderGeometry :: Geometry -> String
renderGeometry = case _ of
  Invisible _ -> ""
  Many array -> unlines $ renderGeometry <$> array

  Rect (AABB aabb) proper ->
    leaf "rect"
      $ printGeometryAttributes proper
      <>
        [ "x" /\ show (x aabb.position)
        , "y" /\ show (y aabb.position)
        , "width" /\ show (x aabb.size)
        , "height" /\ show (y aabb.size)
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

  Transform (V.Transform transform) g ->
    tag "g"
      [ "transform" /\ joinWith " "
          [ "matrix("
          , show $ transform.scale * cos (unwrap transform.rotation)
          , show $ transform.scale * sin (unwrap transform.rotation)
          , show $ transform.scale * -sin (unwrap transform.rotation)
          , show $ transform.scale * cos (unwrap transform.rotation)
          , show $ x transform.position
          , show $ y transform.position
          , ")"
          ]
      ]
      $ renderGeometry g

-- | Adds the necessary boilerplate to store svg inside a file
makeSvgDocument :: Geometry -> String
makeSvgDocument geometry = tag "svg" attributes $ renderGeometry geometry
  where
  attributes =
    [ "xmlns" /\ "http://www.w3.org/2000/svg"
    , "xmlns:xlink" /\ "http://www.w3.org/1999/xlink"
    ]
      <>
        case boundingBox <$> boundingPolygon geometry of
          Nothing -> []
          Just (AABB box) -> pure
            $ "viewBox"
            /\ joinWith " "
              [ show $ x box.position
              , show $ y box.position
              , show $ x box.size
              , show $ y box.size
              ]
