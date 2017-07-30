module Solitaire.Ansi where

import Solitaire.Prelude
import Data.Array as Array
import Data.List as List
import Data.String as String
import Ansi.Codes as Ansi
import Ansi.Output (withGraphics, foreground, background, bold)

import Solitaire.Card (Card(..), Suit(..), Rank(..), Colour(..), displayCard, suitColour)
import Solitaire.Stack (Stack)
import Solitaire.Stack as Stack
import Solitaire.Foundations (Foundations)
import Solitaire.Foundations as Foundations
import Solitaire.Stock (Stock)
import Solitaire.Stock as Stock
import Solitaire.Tableaux (Tableaux, Tableau(..), TableauIndex)
import Solitaire.Tableaux as Tableaux
import Solitaire.Game (Game)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose =
  Array.toUnfoldable
  >>> map Array.toUnfoldable
  >>> List.transpose
  >>> map Array.fromFoldable
  >>> Array.fromFoldable

foundations :: Foundations -> Array (Array String)
foundations =
  map (maybe emptySpace singleCard) <<< Foundations.toArray

stock :: Stock -> Array (Array String)
stock s =
  [ maybe emptySpace singleCard (Stock.head s)
  , if Stock.anyFaceDown s then fullFaceDown else emptySpace
  ]

topHalf :: Game -> String
topHalf game =
  displayColumns $
    foundations game.foundations <>
    [ justSpaces ] <>
    stock game.stock

displayColumns :: Array (Array String) -> String
displayColumns =
  String.joinWith "\n" <<<
  map (intercalate " ") <<<
  transpose

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

emptySpace :: Array String
emptySpace =
  [ topLeft <> power horizontalDotted cardWidth <> topRight
  , verticalDotted <> power " " cardWidth <> verticalDotted
  , verticalDotted <> power " " cardWidth <> verticalDotted
  , bottomLeft <> power horizontalDotted cardWidth <> bottomRight
  ]

justSpaces :: Array String
justSpaces =
  Array.replicate cardHeight (power " " (cardWidth + 2))

singleCard :: Card -> Array String
singleCard c =
  halfCard c <> bottomHalfCard

halfCard :: Card -> Array String
halfCard c =
  [ topLeft <> power horizontal cardWidth <> topRight
  , vertical <> withGraphics (graphicsForSuit c.suit) (rpad cardWidth (displayCard c)) <> vertical
  ]

halfFaceDown :: Array String
halfFaceDown =
  [ topLeft <> power horizontal cardWidth <> topRight
  , vertical <> power "*" cardWidth <> vertical
  ]

fullFaceDown :: Array String
fullFaceDown =
  halfFaceDown <>
  [ vertical <> power "*" cardWidth <> vertical
  , bottomLeft <> power horizontal cardWidth <> bottomRight
  ]

bottomHalfCard :: Array String
bottomHalfCard =
  [ vertical <> withGraphics (background Ansi.White) (power " " cardWidth) <> vertical
  , bottomLeft <> power horizontal cardWidth <> bottomRight
  ]

cardWidth :: Int
cardWidth = 4

cardHeight :: Int
cardHeight = 4

graphicsForSuit :: Suit -> Array Ansi.GraphicsParam
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
