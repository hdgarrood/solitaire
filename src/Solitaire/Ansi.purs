module Solitaire.Ansi where

import Solitaire.Prelude
import Data.Array as Array
import Data.List.NonEmpty (NonEmptyList)
import Data.List as List
import Data.String as String
import Ansi.Codes as Ansi
import Ansi.Output (withGraphics, foreground, background, bold)

import Solitaire.Card (Card, Suit, Colour(..), displayCard, suitColour, cardSuit)
import Solitaire.Stack (Stack)
import Solitaire.Stack as Stack
import Solitaire.Foundations (Foundations)
import Solitaire.Foundations as Foundations
import Solitaire.Stock (Stock)
import Solitaire.Stock as Stock
import Solitaire.Tableaux (Tableau(..))
import Solitaire.Tableaux as Tableaux
import Solitaire.Game (Game)

newtype Column = Column (Array String)

derive instance newtypeColumn :: Newtype Column _
derive newtype instance semigroupColumn :: Semigroup Column
derive newtype instance monoidColumn :: Monoid Column

instance showColumn :: Show Column where
  show = String.joinWith "\n" <<< unwrap

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose =
  Array.toUnfoldable
  >>> map Array.toUnfoldable
  >>> List.transpose
  >>> map Array.fromFoldable
  >>> Array.fromFoldable

foundations :: Foundations -> Array Column
foundations =
  map (maybe emptySpace singleCard) <<< Foundations.toArray

stock :: Stock -> Array Column
stock s =
  [ maybe emptySpace singleCard (Stock.head s)
  , if Stock.anyFaceDown s then fullFaceDown else emptySpace
  ]

stack :: Stack -> Column
stack s =
  let
    cards = Array.reverse (Array.fromFoldable (Stack.run s))
  in
    foldMap halfCard cards <> bottomHalfCard

tableau :: Tableau -> Column
tableau =
  case _ of
    EmptySpace -> Column []
    Tableau { stack: s, faceDown } ->
      power halfFaceDown (List.length faceDown) <>
      stack s

game :: Game -> String
game g = topHalf <> "\n" <> bottomHalf
  where
  topHalf =
    displayColumns $
      foundations (unwrap g).foundations <>
      [ justSpaces ] <>
      stock (unwrap g).stock

  bottomHalf =
    let
      cols =
        map (\ix -> tableau (Tableaux.get ix (unwrap g).tableaux))
            (enumFromTo bottom top)
      longest =
        fromMaybe 0 (maximum (map (Array.length <<< unwrap) cols))
    in
      displayColumns $
        map (over Column (extend longest (power " " (cardWidth + 2)))) cols

-- | Extend an array with copies of a given element to ensure that it reaches
-- | a certain length.
extend :: forall a. Int -> a -> Array a -> Array a
extend minLen extra arr =
  let
    actualLen = Array.length arr
    diff = minLen - actualLen
  in
    if diff > 0
      then arr <> Array.replicate diff extra
      else arr

displayColumns :: Array Column -> String
displayColumns =
  String.joinWith "\n" <<<
  map (intercalate " ") <<<
  transpose <<<
  map unwrap

topLeft :: String
topLeft = "┌"

topRight :: String
topRight = "┐"

bottomLeft :: String
bottomLeft = "└"

bottomRight :: String
bottomRight = "┘"

horizontal :: String
horizontal = "─"

vertical :: String
vertical = "│"

horizontalDotted :: String
horizontalDotted = "┄"

verticalDotted :: String
verticalDotted = "┆"

emptySpace :: Column
emptySpace =
  Column $
    [ topLeft <> power horizontalDotted cardWidth <> topRight
    , verticalDotted <> power " " cardWidth <> verticalDotted
    , verticalDotted <> power " " cardWidth <> verticalDotted
    , bottomLeft <> power horizontalDotted cardWidth <> bottomRight
    ]

justSpaces :: Column
justSpaces =
  Column $
    Array.replicate cardHeight (power " " (cardWidth + 2))

halfJustSpaces :: Column
halfJustSpaces =
  Column $
    Array.replicate (cardHeight / 2) (power " " (cardWidth + 2))

singleCard :: Card -> Column
singleCard c =
  halfCard c <> bottomHalfCard

halfCard :: Card -> Column
halfCard c =
  Column $
    [ topLeft <> power horizontal cardWidth <> topRight
    , vertical <> withGraphics (graphicsForSuit (cardSuit c)) (rpad cardWidth (displayCard c)) <> vertical
    ]

halfFaceDown :: Column
halfFaceDown =
  Column $
    [ topLeft <> power horizontal cardWidth <> topRight
    , vertical <> power "*" cardWidth <> vertical
    ]

fullFaceDown :: Column
fullFaceDown =
  halfFaceDown <>
  Column
    [ vertical <> power "*" cardWidth <> vertical
    , bottomLeft <> power horizontal cardWidth <> bottomRight
    ]

bottomHalfCard :: Column
bottomHalfCard =
  Column
    [ vertical <> withGraphics (background Ansi.White) (power " " cardWidth) <> vertical
    , bottomLeft <> power horizontal cardWidth <> bottomRight
    ]

cardWidth :: Int
cardWidth = 4

cardHeight :: Int
cardHeight = 4

graphicsForSuit :: Suit -> NonEmptyList Ansi.GraphicsParam
graphicsForSuit suit =
  foreground (ansiColorForSuit suit) <>
  background Ansi.White <>
  bold

ansiColorForSuit :: Suit -> Ansi.Color
ansiColorForSuit suit =
  case suitColour suit of
    Red -> Ansi.BrightRed
    Black -> Ansi.BrightBlack

rpad :: Int -> String -> String
rpad min str =
  let
    diff = min - String.length str
  in
    if diff > 0
      then str <> power " " diff
      else str
