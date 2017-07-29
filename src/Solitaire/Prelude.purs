module Solitaire.Prelude
  ( module ReExports
  ) where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(EQ, GT, LT), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, id, ifM, join, lcm, liftA1, liftM1, map, max, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as ReExports
import Control.MonadZero (guard) as ReExports
import Control.Monad.Trans.Class (lift) as ReExports
import Control.Monad.State.Trans (StateT) as ReExports
import Control.Monad.State.Class (gets, put, modify) as ReExports
import Control.Monad.Eff.Console (log, logShow) as ReExports
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), pred, succ, toEnum, fromEnum, defaultPred, defaultSucc) as ReExports
import Data.Tuple (Tuple(..)) as ReExports
import Data.List (List(..), (:)) as ReExports
import Data.Maybe (Maybe(..), maybe, fromMaybe) as ReExports
import Data.Map (Map) as ReExports
import Data.Monoid (class Monoid, mempty, power) as ReExports
import Data.Foldable (class Foldable, fold, for_, traverse_, intercalate) as ReExports 
import Data.Profunctor.Strong (first, second) as ReExports
import Partial.Unsafe (unsafeCrashWith) as ReExports
