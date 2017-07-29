module Solitaire.Game where

import Solitaire.Prelude

import Solitaire.Card (Card(..), Suit(..), Rank(..), suitColour)
import Solitaire.Stack (Stack)
import Solitaire.Stack as Stack
import Solitaire.Foundations (Foundations)
import Solitaire.Stock (Stock)
import Solitaire.Stock as Stock
import Solitaire.Tableaux (Tableaux, Tableau, TableauIndex)
import Solitaire.Tableaux as Tableaux

type Game
  = { stock :: Stock
    , tableaux :: Tableaux
    , foundations :: Foundations
    }

-- | A `GameM` action is one which can modify the state of a `Game`, and which
-- | can fail (if an illegal move is attempted).
type GameM = StateT Game Maybe

-- | A cursor which points to a single card in an in-progress game.
data CardCursor
  -- | Points to the card at the top of the waste pile.
  = FromWaste
  -- | Points to the card at the bottom of a tableau stack.
  | FromTableau TableauIndex

-- | A cursor which points to a stack in an in-progress game.
data StackCursor
  = StackCursor { tableau :: TableauIndex, size :: Int }

data Move
  = ResetStock
  | AdvanceStock
  | WasteToTableau TableauIndex 
  | MoveToFoundations CardCursor
  | MoveStack StackCursor TableauIndex

applyMove :: Move -> GameM Unit
applyMove =
  case _ of
    ResetStock -> resetStock
    AdvanceStock -> advanceStock
    WasteToTableau ix -> wasteToTableau ix
    MoveToFoundations csr -> moveToFoundations csr
    MoveStack csr ix -> moveStack csr ix

withStock :: forall a. (Stock -> Maybe (Tuple a Stock)) -> GameM a
withStock f = do
  stock <- gets _.stock
  Tuple x newStock <- lift $ f stock
  modify (_ { stock = newStock })
  pure x

withTableau :: forall a.
  TableauIndex -> (Tableau -> Maybe (Tuple a Tableau)) -> GameM a
withTableau ix f = do
  tableaux <- gets _.tableaux
  Tuple x newTableaux <- lift $ Tableaux.modify ix f tableaux
  modify (_ { tableaux = newTableaux })
  pure x

resetStock :: GameM Unit
resetStock =
  withStock (map (Tuple unit) <<< Stock.reset)
 
advanceStock :: GameM Unit
advanceStock =
  withStock (map (Tuple unit) <<< Stock.advance)

takeFromWaste :: GameM Card
takeFromWaste =
  withStock \s ->
    map (\{ card, stock } -> Tuple card stock) (Stock.take s)

wasteToTableau :: TableauIndex -> GameM Unit
wasteToTableau ix = do
  card <- takeFromWaste
  withTableau ix (map (Tuple unit) <<< Tableaux.addCard card)

moveToFoundations :: CardCursor -> GameM Unit
moveToFoundations csr = do
  pure unit

moveStack :: StackCursor -> TableauIndex -> GameM Unit
moveStack csr ix = do
  pure unit
