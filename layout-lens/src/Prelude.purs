module LayoutLens.Prelude
  ( module Prelude
  , module Data.Maybe
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Effect
  , module Effect.Class
  , module Effect.Class.Console
  , module Data.Generic.Rep
  , module Data.Debug
  , module Color
  , module Data.HashMap
  , module Data.HashSet
  , module Data.Hashable
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional)
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry)
import Data.Tuple.Nested (type (/\), T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, Tuple1, Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9, curry1, curry10, curry2, curry3, curry4, curry5, curry6, curry7, curry8, curry9, get1, get10, get2, get3, get4, get5, get6, get7, get8, get9, over1, over10, over2, over3, over4, over5, over6, over7, over8, over9, tuple1, tuple10, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9, uncurry1, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, uncurry6, uncurry7, uncurry8, uncurry9, (/\))
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (clear, error, errorShow, group, groupCollapsed, groupEnd, grouped, info, infoShow, log, logShow, time, timeEnd, timeLog, warn, warnShow)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, repOf, to)
import Data.Debug (class Debug, class DebugRowList, class GenericDebug, class GenericDebugArgs, DiffOptions, PrettyPrintOptions, Repr, ReprDelta, array, assoc, boolean, char, collection, constructor, debug, debugRowList, defaultDiffOptions, defaultPrettyPrintOptions, diff, diffRepr, diffReprWith, genericDebug, genericDebug', genericDebugArgs, int, number, opaque, opaqueLiteral, opaque_, prettyPrint, prettyPrintDelta, prettyPrintDeltaWith, prettyPrintWith, record, string)
import Color (Color, ColorSpace(..), Interpolator, black, brightness, complementary, contrast, cssStringHSLA, cssStringRGBA, darken, desaturate, distance, fromHexString, fromInt, graytone, hsl, hsla, hsv, hsva, isLight, isReadable, lab, lch, lighten, luminance, mix, mixCubehelix, rgb, rgb', rgba, rgba', rotateHue, saturate, textColor, toGray, toHSLA, toHSVA, toHexString, toLCh, toLab, toRGBA, toRGBA', toXYZ, white, xyz)
import Data.HashMap (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (class Hashable)
