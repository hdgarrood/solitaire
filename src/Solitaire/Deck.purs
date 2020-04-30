module Solitaire.Deck
  ( Deck
  , unshuffled
  , fromSeed
  , run
  , randomDeck
  ) where

import Solitaire.Prelude
import Test.QuickCheck.Gen (evalGen, shuffle)
import Random.LCG (Seed, randomSeed)

import Solitaire.Card (Card(..), allSuits, allRanks)

-- | A `Deck` contains every card exactly once.
newtype Deck = Deck (Array Card)

run :: Deck -> Array Card
run (Deck cards) = cards

unshuffled :: Array Card
unshuffled =
  Card <$> allSuits <*> allRanks

fromSeed :: Seed -> Deck
fromSeed newSeed =
  Deck $ evalGen (shuffle unshuffled) { newSeed, size: 0 }

randomDeck :: Effect Deck
randomDeck = map fromSeed randomSeed
