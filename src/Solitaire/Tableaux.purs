module Solitaire.Tableaux
  ( Tableau(..)
  , addCard
  , takeCard
  , addStack
  , takeStack
  , TableauIndex
  , Tableaux
  , initial
  , get
  , put
  , modify
  ) where

import Solitaire.Prelude hiding (put, modify)
import Data.Map as Map
import Data.Array as Array

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

-- | Attempt to take a single card from a tableau. Returns `Nothing` if the
-- | tableau is an empty space.
takeCard :: Tableau -> Maybe (Tuple Card Tableau)
takeCard =
  case _ of
    EmptySpace ->
      Nothing
    Tableau { stack, faceDown } ->
      case Stack.pop stack of
        { card, remaining } ->
          let
            fromNewStack s = Tableau { stack: s, faceDown }
          in
            Just (Tuple card (maybe (fromCards faceDown) fromNewStack remaining))

-- | Attempt to add another stack to the bottom of a tableau's stack.
addStack :: Stack -> Tableau -> Maybe Tableau
addStack newStack =
  case _ of
    EmptySpace ->
      if (Stack.highCard newStack).rank == King
        then Just (Tableau { stack: newStack, faceDown: Nil })
        else Nothing
    Tableau { stack, faceDown } ->
      let
        reconstruct combined = Tableau { stack: combined, faceDown }
      in
        map reconstruct (Stack.join newStack stack)

-- | Attempt to take a stack of the given size from a tableau. Returns
-- | `Nothing` if there are not that many cards in the given tableau's stack.
takeStack :: Int -> Tableau -> Maybe (Tuple Stack Tableau)
takeStack size =
  case _ of
    EmptySpace ->
      Nothing
    Tableau { stack, faceDown } ->
      if size == Stack.size stack
        then
          Just (Tuple stack (fromCards faceDown))
        else
          let
            reconstruct { top: t, bottom: b } =
              Tuple b (Tableau { stack: t, faceDown })
          in
            map reconstruct (Stack.split size stack)
      
-- | Create a `Tableau` from a list of cards by turning the head card face-up
-- | and leaving the rest face-down.
fromCards :: List Card -> Tableau
fromCards =
  case _ of
    c:cs ->
      Tableau { stack: Stack.singleton c, faceDown: cs }
    Nil ->
      EmptySpace

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

run :: Tableaux -> Map TableauIndex { stack :: Stack, faceDown :: List Card }
run (Tableaux m) = m

empty :: Tableaux
empty = Tableaux Map.empty

-- | Create an initial set of tableaux.
initial :: Array Card -> { tableaux :: Tableaux, leftover :: Array Card }
initial cards =
  let
    sliceFor ix = Tuple (triangle ix) (triangle (ix + 1))
    tableaux = foldr go empty (Array.range 0 6)
    go ix =
      case sliceFor ix of
        Tuple from to ->
          let
            cs = Array.toUnfoldable (Array.slice from to cards)
          in
            put (TableauIndex ix) (fromCards cs)
    leftover = Array.drop (triangle 7) cards
  in
    { tableaux, leftover }

get :: TableauIndex -> Tableaux -> Tableau
get ix = maybe EmptySpace Tableau <<< Map.lookup ix <<< run

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
