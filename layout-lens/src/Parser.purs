module LayoutLens.Parser (parseConfig) where

import LayoutLens.Prelude hiding (string)

import Data.Array as Array
import Data.HashMap as HM
import Data.HashSet as HS
import Data.Int as Int
import Data.Number as Number
import Data.String.CodeUnits as String
import LayoutLens.Data.CommonConfig
  ( Action(..)
  , ActionDisplay(..)
  , ActionEffect(..)
  , Chord(..)
  , ConfigElement(..)
  , ConfigSection(..)
  , Layer(..)
  , LayerVisualPosition(..)
  , layerName
  , sectionElements
  )
import LayoutLens.Data.RawConfig
  ( RawConfig(..)
  , RawPhysical(..)
  , RawPhysicalActionStep(..)
  , RawPhysicalStep(..)
  )
import LayoutLens.Data.Vec2 (Radians(..), RawScalePreservingTransform(..), Vec2(..))
import Safe.Coerce (coerce)
import StringParser (Parser, printParserError, runParser)
import StringParser as P

-- {{{ Base combinators
singleNewline :: Parser Unit
singleNewline = void $ flip P.withError "failed to match single newline" $ P.regex "\n"

iws :: Parser Unit
iws = void $ flip P.withError "failed to match one or more inline whitespace" $ P.regex "[ \t]+"

oiws :: Parser Unit
oiws = void $ flip P.withError "failed to match zero or more inline whitespace" $ P.regex "[ \t]*"

ows :: Parser Unit
ows = void $ flip P.withError "failed to match zero or more whitespace" $ P.regex "[ \t\n]*"

ws :: Parser Unit
ws = void $ flip P.withError "failed to match one or more whitespace" $ P.regex "[ \t\n]+"

newline :: Parser Unit
newline = void $ P.many1 (P.try $ (oiws *> singleNewline)) *> oiws

manyLines :: forall a. Parser a -> Parser (Array a)
manyLines p = Array.fromFoldable <$> P.many (p <* ows)

tok :: forall a. Parser a -> Parser a
tok p = iws *> p

number :: Parser Number
number = do
  string <- flip P.withError "failed to match real" $ P.regex "[-+]?[0-9]*\\.?[0-9]+"
  case Number.fromString string of
    Just num -> pure num
    Nothing -> P.fail $ "Invalid number " <> string

nat :: Parser Int
nat = do
  string <- flip P.withError "failed to match natural" $ P.regex "[0-9]+"
  case Int.fromString string of
    Just num -> pure num
    Nothing -> P.fail $ "Invalid natural " <> string

vec2 :: Parser Vec2
vec2 = Vec2 <$> (iws *> number) <*> (iws *> number)

radians :: Parser Radians
radians = Radians <$> (iws *> number)

name :: Parser String
name = ows *> P.try do
  result <- P.regex "\\S+"
  when (Array.elem result kws) $
    P.fail "Names cannot be keywords"
  pure result
  where
  kws =
    [ "layergroup"
    , "chordgroup"
    , "columns"
    , "section"
    , "layer"
    , "block"
    , "pre"
    , "end"
    , "point"
    , "place"
    , "physical"
    , "action"
    ]

displaySymbol :: Parser String
displaySymbol = do
  result <- name
  pure
    if result == "⚔️" then ""
    else result

color :: Parser Color
color = do
  string <- flip P.withError "failed to match hexstring" $ P.regex "#[0-9a-fA-F]{6}"
  case fromHexString string of
    Just color -> pure color
    Nothing -> P.fail $ "Invalid color " <> string

string :: String -> Parser Unit
string s = oiws <* P.string s

noDuplicates :: String -> Array String -> Parser Unit
noDuplicates what arr =
  void $
    Array.foldM
      ( \prev name -> do
          when (HS.member name prev) do
            P.fail $ what <> " " <> name <> " defined multiple times"
          pure $ HS.insert name prev
      )
      HS.empty
      arr

-- }}}

physical :: Parser RawPhysical
physical = do
  string "physical" *> newline
  RawPhysical <$> manyLines (block <|> (PhysicalAction <$> actionStep))
  where
  block :: Parser RawPhysicalStep
  block = do
    string "block" *> newline
    steps <- manyLines actionStep
    string "end"
    pure $ Block steps

  actionStep :: Parser RawPhysicalActionStep
  actionStep = place <|> point

  place :: Parser RawPhysicalActionStep
  place = flip P.withError "failed to parse 'place' command" do
    string "place"
    position <- vec2
    rotateBy /\ rotateAround <- P.option (Radians 0.0 /\ position) do
      angle <- radians
      around <- P.option position vec2
      pure $ angle /\ around
    pure $ Place $
      RawScalePreservingTransform
        { position
        , rotateBy
        , rotateAround
        }

  point :: Parser RawPhysicalActionStep
  point = do
    string "key"
    position <- vec2
    arguments <- Array.fromFoldable <$> P.many (iws *> number)
    let size = Vec2 1.0 1.0
    let rotateBy = Radians 0.0
    let rotateAround = position
    let
      point a b c d = Point
        { transform: RawScalePreservingTransform
            { position: a
            , rotateBy: b
            , rotateAround: c
            }
        , size: d
        }
    case arguments of
      [] -> pure $ point position rotateBy rotateAround size
      [ angle ] -> pure $ point position (Radians angle) rotateAround size
      [ sx, sy ] -> pure $ point position rotateBy rotateAround (Vec2 sx sy)
      [ sx, sy, angle ] -> pure $ point position (Radians angle) rotateAround (Vec2 sx sy)
      [ sx, sy, angle, rx, ry ] -> pure $ point position (Radians angle) (Vec2 rx ry) (Vec2 sx sy)

      _ -> P.fail "Too many arguments provided to point"

layer :: Parser (LayerVisualPosition /\ Layer)
layer = do
  string "layer"
  layerName <- tok name
  position <- tok $ oneOf
    [ string "center" $> Center
    , string "topleft" $> TopLeft
    , string "topright" $> TopRight
    , string "bottomleft" $> BottomLeft
    , string "bottomright" $> BottomRight
    ]
  textColor <- P.optionMaybe $ tok color
  newline
  keys <- Array.fromFoldable
    <$> P.many1Till
      (displaySymbol <* ws)
      (string "end")
  pure $ position /\ Layer { name: layerName, keys, textColor }

layergroup :: Parser ConfigElement
layergroup = do
  string "layergroup" *> newline
  layers <- manyLines layer
  noDuplicates "Layer" $ (show <<< fst) <$> layers
  pure $ LayerGroup $ coerce $ Array.foldMap
    (\(name /\ value) -> HM.singleton name $ wrapInto @(First Layer) value)
    layers

chord :: Parser Chord
chord = do
  from <- Array.fromFoldable <$> P.manyTill
    (name <* iws)
    (P.string "=>")

  to <- tok $ Array.fromFoldable <$> P.manyTill
    (displaySymbol <* iws)
    (P.lookAhead color)

  fill <- color
  fontSizeModifier <- P.option 1.0 $ tok number
  pure $ Chord { from, to, fill, fontSizeModifier }

chordgroup :: Parser ConfigElement
chordgroup = do
  string "chordgroup" *> newline
  c <- ChordGroup <$> manyLines chord
  pure c

type NamedAction = String /\ Action

action :: Parser NamedAction
action = do
  string "action"
  actionName <- tok name
  display <- tok $ oneOf
    [ DisplayLayerColor <$ string "🌈"
    , DisplaySymbol <$> displaySymbol
    ]
  effect <- tok $ oneOf
    [ string "switch" *> (LayerSwitch <$> tok name)
    , string "sticky-switch" *> (StickyLayerSwitch <$> tok name)
    ]
  pure $ actionName /\ Action { display, effect }

section :: Parser (Array NamedAction /\ ConfigSection)
section = do
  string "section" *> newline
  actions /\ columnCounts /\ elements <- map fold $ manyLines $ oneOf
    [ action <#> \action -> [ action ] /\ [] /\ []
    , parseColumns <#> \amount -> [] /\ [ amount ] /\ []
    , layergroup <|> chordgroup <#> \element -> [] /\ [] /\ [ element ]
    ]

  columns <- case columnCounts of
    [] -> pure 1
    [ single ] -> pure single
    _ -> P.fail $ "Column count defined multiple times " <> show columnCounts

  pure $ actions /\ ConfigSection { columns, elements }
  where
  parseColumns :: Parser Int
  parseColumns = P.string "columns" *> tok nat

config :: Parser RawConfig
config = do
  physical <- physical
  sections <- ows *> manyLines section <* ows <* P.eof

  sections
    >>= fst
    <#> fst
    # noDuplicates "Action"

  sections
    <#> snd
    >>= sectionElements
    >>= case _ of
      LayerGroup layers -> layerName <$> HM.values layers
      ChordGroup _ -> []
    # noDuplicates "Layer"

  pure $ RawConfig
    { physical
    , sections: sections <#> snd
    , actions: sections
        >>= fst
        # Array.foldMap
            (\(name /\ action) -> HM.singleton name $ wrapInto @(First _) action)
        # coerce
    }

parseConfig :: String -> Either String RawConfig
parseConfig input = case runParser config input of
  Left err -> Left $ fold [ printParserError err, "\n", String.slice (err.pos) (err.pos + 30) input ]
  Right result -> Right result
