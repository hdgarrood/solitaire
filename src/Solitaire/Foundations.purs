module Solitaire.Foundations
  ( Foundations
  , initial
  , lookupSuit
  , addCard
  , toArray
  , isComplete
  ) where

import Solitaire.Prelude
import Data.Map as Map
import Solitaire.Card (Card(..), Suit, allSuits, Rank(King))

-- | This data type encapsulates the four foundation piles in a solitaire game.
newtype Foundations
  = Foundations (Map Suit Rank)

derive newtype instance encodeJsonFoundations :: EncodeJson Foundations
derive newtype instance decodeJsonFoundations :: DecodeJson Foundations

initial :: Foundations
initial = Foundations (Map.fromFoldable [])

-- | Lookup the highest card in the foundation pile for a given suit; returns
-- | `Nothing` if there are no cards in the pile for the given suit.
lookupSuit :: Suit -> Foundations -> Maybe Rank
lookupSuit suit (Foundations m) =
  Map.lookup suit m

-- | Attempt to add a `Card` to a `Foundations`. Returns `Nothing` if the card
-- | could not be added.
addCard :: Card -> Foundations -> Maybe Foundations
addCard (Card suit rank) f@(Foundations m) =
  if pred rank == lookupSuit suit f
    then Just (Foundations (Map.insert suit rank m))
    else Nothing

toArray :: Foundations -> Array (Maybe Card)
toArray fdn =
  let
    go suit =
      map (Card suit) (lookupSuit suit fdn)
  in
    map go allSuits

isComplete :: Foundations -> Boolean
isComplete fdn =
  all (\s -> lookupSuit s fdn == Just King) allSuits
