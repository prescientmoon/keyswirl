module Main where

import LayoutLens.Prelude

import LayoutLens.Data.Config (LensConfig(..), buildConfig)
import LayoutLens.Data.Geometry (pad)
import LayoutLens.Data.Svg (makeSvgDocument)
import LayoutLens.Data.Vec2 (Vec2(..))
import LayoutLens.Generate.Svg (renderPhysicalLayout)
import LayoutLens.Parser (parseConfig)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)

main :: Effect Unit
main = launchAff_ do
  file <- readTextFile UTF8 "../keyboards/qmk/ferris-sweep/config.lens"
  -- file <- readTextFile UTF8 "./input.lens"
  case parseConfig file of
    Left err -> log err
    Right result -> do
      let (LensConfig config) = buildConfig result
      logPretty config
      -- logPretty $ boundingPolygon $ renderPhysicalLayout config.physical
      writeTextFile UTF8 "./output.svg"
        $ makeSvgDocument
        $ pad (Vec2 30.0 30.0)
        $ renderPhysicalLayout
        $ config.physical

