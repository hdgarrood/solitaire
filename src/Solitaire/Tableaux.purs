module Solitaire.Tableaux
  ( Tableau(..)
  , addStack
  , takeStack
  , stackSize
  , TableauIndex
  , ixFromInt
  , allIndices
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
import Solitaire.Card (Card, Rank(..), cardRank)

-- | One of the seven central piles (tableaux) in a solitaire game.
data Tableau
  = EmptySpace
  | Tableau { stack :: Stack, faceDown :: List Card }

derive instance eqTableau :: Eq Tableau
derive instance genericTableau :: Generic Tableau _

instance encodeJsonTableau :: EncodeJson Tableau where
  encodeJson = genericEncodeJson

instance decodeJsonTableau :: DecodeJson Tableau where
  decodeJson = genericDecodeJson

-- | Attempt to add another stack to the bottom of a tableau's stack.
addStack :: Stack -> Tableau -> Maybe Tableau
addStack newStack =
  case _ of
    EmptySpace ->
      if cardRank (Stack.highCard newStack) == King
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

-- | Returns the size of the stack, or `Nothing` if the tableau is an empty
-- | space.
stackSize :: Tableau -> Maybe Int
stackSize =
  case _ of
    EmptySpace ->
      Nothing
    Tableau { stack } ->
      Just (Stack.size stack)
      
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

-- | A convenience function for creating a `TableauIndex`.
ixFromInt :: Int -> TableauIndex
ixFromInt i =
  case toEnum i of
    Just r -> r
    Nothing
      | i <= 0    -> bottom
      | otherwise -> top

allIndices :: Array TableauIndex
allIndices = enumFromTo bottom top

derive newtype instance eqTableauIndex :: Eq TableauIndex
derive newtype instance ordTableauIndex :: Ord TableauIndex
derive newtype instance showTableauIndex :: Show TableauIndex
derive instance genericTableauIndex :: Generic TableauIndex _

derive newtype instance encodeJsonTableauIndex :: EncodeJson TableauIndex
derive newtype instance decodeJsonTableauIndex :: DecodeJson TableauIndex

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

instance encodeJsonTableaux :: EncodeJson Tableaux where
  encodeJson = encodeJson <<< toJson

instance decodeJsonTableaux :: DecodeJson Tableaux where
  decodeJson = map fromJson <<< decodeJson

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

-- Just for Json encoding/decoding
newtype TableauxJson =
  TableauxJson (Map TableauIndex (Tuple Stack (List Card)))

derive newtype instance encodeJsonTableauxJson :: EncodeJson TableauxJson
derive newtype instance decodeJsonTableauxJson :: DecodeJson TableauxJson

toJson :: Tableaux -> TableauxJson
toJson (Tableaux m) =
  TableauxJson $ map (\{ stack, faceDown } -> Tuple stack faceDown) m

fromJson :: TableauxJson -> Tableaux
fromJson (TableauxJson m) =
  Tableaux $ map (\(Tuple stack faceDown) -> { stack,  faceDown }) m
