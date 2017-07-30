module Solitaire.Stock
  ( Stock
  , initial
  , run
  , head
  , take
  , anyFaceDown
  , reset
  , advance
  ) where

import Solitaire.Prelude
import Data.List as List

import Solitaire.Card (Card)

-- | This data type encapsulates the stock and waste piles.
data Stock
  -- Fields: face-up cards (waste), face-down cards (stock).
  = Stock (List Card) (List Card)

derive instance genericStock :: Generic Stock _

instance encodeJsonStock :: EncodeJson Stock where
  encodeJson = genericEncodeJson

instance decodeJsonStock :: DecodeJson Stock where
  decodeJson = genericDecodeJson

initial :: List Card -> Stock
initial stock = Stock Nil stock

run :: Stock -> { waste :: List Card, stock :: List Card }
run (Stock waste stock) = { waste, stock }

-- | Returns the visible card at the top of the face-up pile (waste), if any.
head :: Stock -> Maybe Card
head (Stock waste _) = List.head waste

-- | Attempt to remove the card from the top of the waste pile; returns
-- | `Nothing` if the waste pile is empty.
take :: Stock -> Maybe { card :: Card, stock :: Stock }
take =
  case _ of
    Stock (card:waste) stock ->
      Just { card, stock: Stock waste stock }
    _ ->
      Nothing

-- | Indicates whether there are any more stock cards which can be moved into
-- | the waste pile.
anyFaceDown :: Stock -> Boolean
anyFaceDown =
  case _ of
    Stock _ Nil -> false
    _ -> true

-- | If there are no more cards in the stock pile, this resets the `Stock` by
-- | moving all the cards from the waste pile back to the stock pile.
-- | Otherwise, this function returns `Nothing`.
reset :: Stock -> Maybe Stock
reset =
  case _ of
    Stock waste Nil -> Just (Stock Nil (List.reverse waste))
    _ -> Nothing

-- | Attempt to advance the `Stock` by taking one card from the stock pile and
-- | putting it at the top of the waste pile.
advance :: Stock -> Maybe Stock
advance =
  case _ of
    Stock waste (next:stock) -> Just (Stock (next:waste) stock)
    _ -> Nothing
