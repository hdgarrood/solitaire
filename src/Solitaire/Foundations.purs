module Solitaire.Foundations
  ( Foundations
  , initialFoundations
  , lookupSuit
  , addCard
  ) where

import Solitaire.Prelude
import Data.Map as Map
import Solitaire.Card (Card, Suit, Rank)

-- | This data type encapsulates the four foundation piles in a solitaire game.
data Foundations
  = Foundations (Map Suit Rank)

initialFoundations :: Foundations
initialFoundations = Foundations (Map.fromFoldable [])

-- | Lookup the highest card in the foundation pile for a given suit; returns
-- | `Nothing` if there are no cards in the pile for the given suit.
lookupSuit :: Suit -> Foundations -> Maybe Rank
lookupSuit suit (Foundations m) =
  Map.lookup suit m

-- | Attempt to add a `Card` to a `Foundations`. Returns `Nothing` if the card
-- | could not be added.
addCard :: Card -> Foundations -> Maybe Foundations
addCard {suit, rank} f@(Foundations m) =
  if pred rank == lookupSuit suit f
    then Just (Foundations (Map.insert suit rank m))
    else Nothing
