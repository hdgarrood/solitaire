module Solitaire.Tableaux
  ( Tableau(..)
  , addCard
  , TableauIndex
  , Tableaux
  , get
  , put
  , modify
  ) where

import Solitaire.Prelude hiding (put, modify)
import Data.Map as Map

import Solitaire.Stack (Stack)
import Solitaire.Stack as Stack
import Solitaire.Card (Card, Rank(..))

-- | One of the seven central piles (tableaux) in a solitaire game.
data Tableau
  = EmptySpace
  | Tableau { stack :: Stack, faceDown :: List Card }

-- | Attempt to add a card to the bottom of a tableau's stack.
addCard :: Card -> Tableau -> Maybe Tableau
addCard card =
  case _ of
    EmptySpace ->
      if (card.rank == King)
        then Just (Tableau { stack: Stack.singleton card, faceDown: Nil })
        else Nothing
    Tableau { stack, faceDown } ->
      let
        reconstruct newStack = Tableau { stack: newStack, faceDown }
      in
        map reconstruct (Stack.push card stack)

-- | Identifies one of the seven tableaux: an `Int` between 0 and 6
-- | (inclusive). The index 0 represents the left-most tableau.
newtype TableauIndex
  = TableauIndex Int

derive newtype instance eqTableauIndex :: Eq TableauIndex
derive newtype instance ordTableauIndex :: Ord TableauIndex
derive newtype instance showTableauIndex :: Show TableauIndex

instance boundedTableauIndex :: Bounded TableauIndex where
  bottom = TableauIndex 0
  top = TableauIndex 6

instance enumTableauIndex :: Enum TableauIndex where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedEnumTableauIndex :: BoundedEnum TableauIndex where
  cardinality =
    Cardinality 7
  fromEnum (TableauIndex i) =
    i
  toEnum i =
    if between 0 6 i
      then Just (TableauIndex i)
      else Nothing

-- | This data type encapsulates the seven tableaux in a solitaire game.
newtype Tableaux =
  Tableaux (Map TableauIndex { stack :: Stack, faceDown :: List Card })

runTableaux :: Tableaux -> Map TableauIndex { stack :: Stack, faceDown :: List Card }
runTableaux (Tableaux m) = m

get :: TableauIndex -> Tableaux -> Tableau
get ix = maybe EmptySpace Tableau <<< Map.lookup ix <<< runTableaux

put :: TableauIndex -> Tableau -> Tableaux -> Tableaux
put ix tableau (Tableaux m) =
  case tableau of
    EmptySpace -> Tableaux (Map.delete ix m)
    Tableau t -> Tableaux (Map.insert ix t m)

modify :: forall a.
  TableauIndex ->
  (Tableau -> Maybe (Tuple a Tableau)) ->
  Tableaux ->
  Maybe (Tuple a Tableaux)
modify ix f tableaux = do
  t <- f (get ix tableaux)
  pure $ second (flip (put ix) tableaux) t
