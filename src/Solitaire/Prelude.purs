module Solitaire.Prelude
  ( triangle, choose, fact
  , succOrLoop, predOrLoop
  , module ReExports
  ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Enum (class BoundedEnum, succ, pred)

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(EQ, GT, LT), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, id, ifM, join, lcm, liftA1, liftM1, map, max, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as ReExports
import Control.MonadZero (guard) as ReExports
import Control.Monad.Trans.Class (lift) as ReExports
import Control.Monad.State.Trans (StateT, runStateT, evalStateT, execStateT) as ReExports
import Control.Monad.State.Class (gets, get, put, modify) as ReExports
import Control.Monad.Eff (Eff) as ReExports
import Control.Monad.Eff.Class (class MonadEff, liftEff) as ReExports
import Control.Monad.Eff.Console (log, logShow, CONSOLE) as ReExports
import Control.Monad.Eff.Unsafe (unsafePerformEff) as ReExports
import Control.Monad.Eff.Ref (Ref, REF, newRef, writeRef, readRef) as ReExports
import Control.Monad.Eff.Random (RANDOM) as ReExports
import Control.Monad.Eff.Exception (EXCEPTION) as ReExports
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), pred, succ, toEnum, fromEnum, defaultPred, defaultSucc, enumFromTo) as ReExports
import Data.Tuple (Tuple(..), fst, snd) as ReExports
import Data.List (List(..), (:)) as ReExports
import Data.Maybe (Maybe(..), maybe, fromMaybe, fromJust) as ReExports
import Data.Map (Map) as ReExports
import Data.Monoid (class Monoid, mempty, power) as ReExports
import Data.Foldable (class Foldable, foldr, foldl, fold, foldMap, for_, traverse_, intercalate, maximum, maximumBy, all) as ReExports
import Data.Function (on) as ReExports
import Data.Profunctor.Strong (first, second) as ReExports
import Data.Newtype (class Newtype, wrap, unwrap, over) as ReExports
import Data.Generic.Rep (class Generic) as ReExports
import Data.Generic.Rep.Show (genericShow) as ReExports
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson) as ReExports
import Data.Argonaut.Core (stringify) as ReExports
import Data.Argonaut.Parser (jsonParser) as ReExports
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson) as ReExports
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson) as ReExports
import Partial.Unsafe (unsafeCrashWith, unsafePartial) as ReExports

-- | The nth triangle number
triangle :: Int -> Int
triangle n = choose (n + 1) 2

-- | Binomial coefficients
choose :: Int -> Int -> Int
choose n k = fact n / (fact k * fact (n - k))

-- | Factorial
fact :: Int -> Int
fact n =
  if n <= 0
    then 1
    else
      let
        go acc 1 = acc
        go acc m = go (acc * m) (m - 1)
      in
        go 1 n

succOrLoop :: forall a. BoundedEnum a => a -> a
succOrLoop x = fromMaybe bottom (succ x)

predOrLoop :: forall a. BoundedEnum a => a -> a
predOrLoop x = fromMaybe top (pred x)
