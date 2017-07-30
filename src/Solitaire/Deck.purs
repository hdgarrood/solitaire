module Solitaire.Deck
  ( Deck
  , unshuffled
  , fromSeed
  , run
  ) where

import Solitaire.Prelude
import Test.QuickCheck.Gen (evalGen, shuffle)
import Test.QuickCheck.LCG (mkSeed)

import Solitaire.Card (Card(..), Suit, Rank)

-- | A `Deck` contains every card exactly once.
newtype Deck = Deck (Array Card)

run :: Deck -> Array Card
run (Deck cards) = cards

unshuffled :: Array Card
unshuffled = do
  suit <- allSuits
  rank <- allRanks
  pure $ Card suit rank
  where
  allSuits = enumFromTo bottom top
  allRanks = enumFromTo bottom top

fromSeed :: Int -> Deck
fromSeed s =
  Deck $ evalGen (shuffle unshuffled) { newSeed: mkSeed s, size: 0 }
