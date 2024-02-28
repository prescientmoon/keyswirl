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
  , module Control.Monad.Reader
  , module Control.Alternative
  , module Data.Foldable
  , module Data.Show.Generic
  , module Data.Either
  , module Effect.Aff
  , module Safe.Coerce
  , module Data.Newtype
  , module Data.Semigroup.First
  , module Data.Number
  , module Data.Semigroup.Generic
  , module Data.Monoid.Generic
  , module Data.String
  , module Data.List
  , wrapInto
  , unimplemented
  , logPretty
  , unlines
  ) where

import Prelude

import Color (Color, ColorSpace(..), Interpolator, black, brightness, complementary, contrast, cssStringHSLA, cssStringRGBA, darken, desaturate, distance, fromHexString, fromInt, graytone, hsl, hsla, hsv, hsva, isLight, isReadable, lab, lch, lighten, luminance, mix, mixCubehelix, rgb, rgb', rgba, rgba', rotateHue, saturate, textColor, toGray, toHSLA, toHSVA, toHexString, toLCh, toLab, toRGBA, toRGBA', toXYZ, white, xyz)
import Control.Alternative (class Alt, class Alternative, empty, (<|>))
import Control.Monad.Reader (Reader, ReaderT, runReader, runReaderT, ask, asks, local)
import Data.Debug (class Debug, class DebugRowList, class GenericDebug, class GenericDebugArgs, DiffOptions, PrettyPrintOptions, Repr, ReprDelta, array, assoc, boolean, char, collection, constructor, debug, debugRowList, defaultDiffOptions, defaultPrettyPrintOptions, diff, diffRepr, diffReprWith, genericDebug, genericDebug', genericDebugArgs, opaque, opaqueLiteral, opaque_, prettyPrint, prettyPrintDelta, prettyPrintDeltaWith, prettyPrintWith, record, string)
import Data.Either (Either(..), blush, choose, either, fromLeft, fromLeft', fromRight, fromRight', hush, isLeft, isRight, note, note')
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for_, indexl, indexr, intercalate, length, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, surround, surroundMap, traverse_)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, repOf, to)
import Data.HashMap (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional)
import Data.Monoid.Generic (genericMempty)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Number (cos, sin, tan, pi)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Generic (genericAppend)
import Data.Show.Generic (genericShow)
import Data.String (joinWith, split)
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry)
import Data.List (List(..))
import Data.Tuple.Nested (type (/\), T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, Tuple1, Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9, curry1, curry10, curry2, curry3, curry4, curry5, curry6, curry7, curry8, curry9, get1, get10, get2, get3, get4, get5, get6, get7, get8, get9, over1, over10, over2, over3, over4, over5, over6, over7, over8, over9, tuple1, tuple10, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9, uncurry1, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, uncurry6, uncurry7, uncurry8, uncurry9, (/\))
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Aff (Aff, BracketConditions, Canceler(..), Error, Fiber, Milliseconds(..), ParAff, apathize, attempt, bracket, cancelWith, catchError, delay, effectCanceler, error, fiberCanceler, finally, forkAff, generalBracket, invincible, joinFiber, killFiber, launchAff, launchAff_, launchSuspendedAff, makeAff, message, never, nonCanceler, parallel, runAff, runAff_, runSuspendedAff, sequential, supervise, suspendAff, throwError, try)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (clear, group, groupCollapsed, groupEnd, grouped, info, infoShow, log, logShow, time, timeEnd, timeLog, warn, warnShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Prim.TypeError (class Warn, Text)
import Safe.Coerce (class Coercible, coerce)

unimplemented :: forall a. Warn (Text "unimplemenet") => a
unimplemented = unsafeThrow "unimplemented"

wrapInto :: forall a @t. Newtype t a => a -> t
wrapInto = wrap

logPretty :: forall m @a. Debug a => MonadEffect m => a -> m Unit
logPretty a = log
  $ prettyPrintWith
      defaultPrettyPrintOptions { maxDepth = Nothing }
  $ debug a

unlines :: Array String -> String
unlines = joinWith "\n"
