module Main where

import LayoutLens.Prelude

import LayoutLens.Parser (parseConfig)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  file <- readTextFile UTF8 "../keyboards/qmk/ferris-sweep/config.lens"
  case parseConfig file of
    Left err -> log err
    Right result -> log
      $ prettyPrintWith
          defaultPrettyPrintOptions { maxDepth = Nothing }
      $ debug result
