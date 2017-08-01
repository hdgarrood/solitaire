module Solitaire.Web.Positioning where

import Solitaire.Prelude
import Data.List as List
import Data.Array as Array

import Solitaire.Card (Card)
import Solitaire.Stack as Stack
import Solitaire.Foundations (Foundations)
import Solitaire.Foundations as Foundations
import Solitaire.Stock (Stock)
import Solitaire.Stock as Stock
import Solitaire.Tableaux (Tableaux, Tableau, TableauIndex)
import Solitaire.Tableaux as Tableaux
import Solitaire.Game (Game(..), Cursor(..), StackCursor)

data CardPosition
  -- A card lies in the top half of the board, i.e. the foundations, waste, and
  -- stock. The argument says which column it should be in (should be in the
  -- range 0-6).
  = TopHalf Int
  -- A card lies in the bottom half of the board, i.e. the tableaux. The
  -- arguments say which column it should be in (0-6), and how far down the
  -- stack it should be (0-17).
  | BottomHalf Int Int

-- | The width of a card as it appears in the game, in pixels
cardWidth :: Int
cardWidth = 60

-- | The height of a card as it appears in the game, in pixels
cardHeight :: Int
cardHeight = (cardWidth * 8) / 5

-- | The width of the horizontal space between rows, in pixels
horizontalMargin :: Int
horizontalMargin = cardWidth / 7

-- | The height of the vertical space between the foundations, stock, & waste
-- | piles, and the tableaux below.
verticalMargin :: Int
verticalMargin = (horizontalMargin * 3) / 2

-- | How far a card should be offset from the card below when fanned
stackOffset :: Int
stackOffset = cardHeight / 3

totalWidth :: Int
totalWidth =
  -- Columns x7
  (7 * cardWidth) +
  -- Gaps between columns x8
  (8 * horizontalMargin)

totalHeight :: Int
totalHeight =
  -- top section
  cardHeight +
  -- bottom section
  cardHeight + (18 * stackOffset) +
  -- Gaps between sections x3
  (3 * verticalMargin)

evalPosition :: CardPosition -> Tuple Int Int
evalPosition =
  case _ of
    TopHalf i ->
      Tuple
        (horizontalMargin + (i * (horizontalMargin + cardWidth)))
        verticalMargin
    BottomHalf i j ->
      Tuple
        (horizontalMargin + (i * (horizontalMargin + cardWidth)))
        ((2 * verticalMargin) + cardHeight + (j * stackOffset))

data CardDisplay
  = FaceUp Card
  | FaceDown
  | EmptySpace

displayGame :: Game -> Array (Tuple CardDisplay CardPosition)
displayGame (Game g) =
  displayFoundations g.foundations <>
  displayStock g.stock <>
  displayTableaux g.tableaux

-- | Returns the top-left corner of the cursor as a CardPosition, plus its
-- | height in pixels
displayCursor :: Game -> Cursor -> { pos :: CardPosition, height :: Int }
displayCursor game =
  case _ of
    WasteCursor ->
      { pos: TopHalf 5, height: cardHeight }
    StackCursor csr ->
      displayStackCursor game csr

displayStackCursor :: Game -> StackCursor -> { pos :: CardPosition, height :: Int }
displayStackCursor (Game g) { ix, size } =
  case Tableaux.get ix g.tableaux of
    Tableaux.EmptySpace ->
      -- shouldn't happen, but we'll handle it anyway
      { pos: pos 0
      , height: cardHeight
      }
    Tableaux.Tableau { stack, faceDown } ->
      let
        startAt = List.length faceDown + (Stack.size stack - size)
      in
        { pos: pos startAt
        , height: cardHeight + (stackOffset * (size - 1))
      }

  where
  pos = BottomHalf (fromEnum ix)

displayFoundations :: Foundations -> Array (Tuple CardDisplay CardPosition)
displayFoundations =
  Array.mapWithIndex go <<< Foundations.toArray
  where
  go i card =
    Tuple (maybe EmptySpace FaceUp card) (TopHalf i)

displayStock :: Stock -> Array (Tuple CardDisplay CardPosition)
displayStock s =
  [ Tuple (maybe EmptySpace FaceUp (Stock.head s)) (TopHalf 5)
  , Tuple (if Stock.anyFaceDown s then FaceDown else EmptySpace) (TopHalf 6)
  ]

displayTableaux :: Tableaux -> Array (Tuple CardDisplay CardPosition)
displayTableaux tblx =
  fold $
    map (\ix -> displayTableau ix (Tableaux.get ix tblx)) Tableaux.allIndices

displayTableau :: TableauIndex -> Tableau -> Array (Tuple CardDisplay CardPosition)
displayTableau ix tableau =
  let
    pos = BottomHalf (fromEnum ix)
  in
    case tableau of
      Tableaux.EmptySpace ->
        [ Tuple EmptySpace (pos 0) ]
      Tableaux.Tableau { stack, faceDown } ->
        let
          numFaceDown =
            List.length faceDown
          faceDowns =
            Array.mapWithIndex
              (\i _ -> Tuple FaceDown (pos i))
              (Array.replicate numFaceDown unit)
          faceUps =
            Array.mapWithIndex
              (\i card -> Tuple (FaceUp card) (pos (numFaceDown + i)))
              (List.toUnfoldable (List.reverse (Stack.run stack)))
        in
          faceDowns <> faceUps
