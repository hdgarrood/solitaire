module Solitaire.Deck
  ( Deck
  , unshuffled
  , fromSeed
  , run
  ) where

import Solitaire.Prelude
import Test.QuickCheck.Gen

import Solitaire.Card (Card, Suit, Rank)

-- | A `Deck` contains every card exactly once.
newtype Deck = Deck (Array Card)

run :: Deck -> Array Card
run (Deck cards) = cards

unshuffled :: Deck
unshuffled = Deck $ do
  suit <- allSuits
  rank <- allRanks
  pure $ { suit, rank }
  where
  allSuits = enumFromTo bottom top :: Array Suit
  allRanks = enumFromTo bottom top :: Array Rank

fromSeed = 0
