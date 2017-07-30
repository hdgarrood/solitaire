module Solitaire.Card where

import Solitaire.Prelude

data Card
  = Card Suit Rank

derive instance eqCard :: Eq Card
derive instance ordCard :: Ord Card
derive instance genericCard :: Generic Card _

instance encodeJsonCard :: EncodeJson Card where
  encodeJson = genericEncodeJson

instance decodeJsonCard :: DecodeJson Card where
  decodeJson = genericDecodeJson

instance showCard :: Show Card where
  show = displayCard

displayCard :: Card -> String
displayCard (Card suit rank ) =
  displayRank rank <> displaySuit suit

cardRank :: Card -> Rank
cardRank (Card _ r) = r

cardSuit :: Card -> Suit
cardSuit (Card s _) = s

-- | A suit. The `Ord` and `Bounded` instances use the bridge ordering: spades
-- | (highest), then hearts, diamons, and clubs.
data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades

derive instance eqSuit :: Eq Suit
derive instance ordSuit :: Ord Suit
derive instance genericSuit :: Generic Suit _

instance encodeJsonSuit :: EncodeJson Suit where
  encodeJson = genericEncodeJson

instance decodeJsonSuit :: DecodeJson Suit where
  decodeJson = genericDecodeJson

displaySuit :: Suit -> String
displaySuit =
  case _ of
    Hearts -> "♥"
    Diamonds -> "♦"
    Clubs -> "♣"
    Spades -> "♠"

instance showSuit :: Show Suit where
  show = displaySuit

instance boundedSuit :: Bounded Suit where
  top = Spades
  bottom = Clubs

instance enumSuit :: Enum Suit where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedEnumSuit :: BoundedEnum Suit where
  cardinality = Cardinality 4

  fromEnum =
    case _ of
      Clubs    -> 0
      Diamonds -> 1
      Hearts   -> 2
      Spades   -> 3

  toEnum =
    case _ of
      0 -> Just Clubs
      1 -> Just Diamonds
      2 -> Just Hearts
      3 -> Just Spades
      _ -> Nothing

data Colour
  = Red
  | Black

derive instance eqColour :: Eq Colour
derive instance ordColour :: Ord Colour

suitColour :: Suit -> Colour
suitColour =
  case _ of
    Hearts -> Red
    Diamonds -> Red
    Clubs -> Black
    Spades -> Black

-- | A card rank. The `Ord` and `Bounded` instances consider `Ace` to be low.
data Rank
  = Ace
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | C10
  | Jack
  | Queen
  | King

derive instance eqRank :: Eq Rank
derive instance ordRank :: Ord Rank
derive instance genericRank :: Generic Rank _

instance encodeJsonRank :: EncodeJson Rank where
  encodeJson = genericEncodeJson

instance decodeJsonRank :: DecodeJson Rank where
  decodeJson = genericDecodeJson

instance boundedRank :: Bounded Rank where
  top = King
  bottom = Ace

instance enumRank :: Enum Rank where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedEnumRank :: BoundedEnum Rank where
  cardinality = Cardinality 13

  fromEnum =
    case _ of
      Ace   -> 0
      C2    -> 1
      C3    -> 2
      C4    -> 3
      C5    -> 4
      C6    -> 5
      C7    -> 6
      C8    -> 7
      C9    -> 8
      C10   -> 9
      Jack  -> 10
      Queen -> 11
      King  -> 12

  toEnum =
    case _ of
      0 -> Just Ace
      1 -> Just C2
      2 -> Just C3
      3 -> Just C4
      4 -> Just C5
      5 -> Just C6
      6 -> Just C7
      7 -> Just C8
      8 -> Just C9
      9 -> Just C10
      10 -> Just Jack
      11 -> Just Queen
      12 -> Just King
      _ -> Nothing

displayRank :: Rank -> String
displayRank =
  case _ of
    Ace -> "A"
    C2 -> "2"
    C3 -> "3"
    C4 -> "4"
    C5 -> "5"
    C6 -> "6"
    C7 -> "7"
    C8 -> "8"
    C9 -> "9"
    C10 -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"

instance showRank :: Show Rank where
  show = displayRank
