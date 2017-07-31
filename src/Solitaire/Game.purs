module Solitaire.Game where

import Solitaire.Prelude

import Data.Array as Array

import Solitaire.Card (Card)
import Solitaire.Stack (Stack)
import Solitaire.Stack as Stack
import Solitaire.Foundations (Foundations)
import Solitaire.Foundations as Foundations
import Solitaire.Stock (Stock)
import Solitaire.Stock as Stock
import Solitaire.Tableaux (Tableaux, Tableau, TableauIndex)
import Solitaire.Tableaux as Tableaux
import Solitaire.Deck (Deck)
import Solitaire.Deck as Deck

newtype Game = Game
  { stock :: Stock
  , tableaux :: Tableaux
  , foundations :: Foundations
  }

derive instance newtypeGame :: Newtype Game _
derive instance genericGame :: Generic Game _

instance encodeJsonGame :: EncodeJson Game where
  encodeJson = genericEncodeJson

instance decodeJsonGame :: DecodeJson Game where
  decodeJson = genericDecodeJson

-- | A `GameM` action is one which can modify the state of a `Game`, and which
-- | can fail (if an illegal move is attempted).
type GameM = StateT Game Maybe

-- | A cursor which points to a stack in an in-progress game.
type StackCursor
  = { ix :: TableauIndex, size :: Int }

-- | A cursor which points to any movable object in a game
data Cursor
  -- | Points to the card at the top of the waste pile.
  = WasteCursor
  -- | Points to a stack at the bottom of a tableau.
  | StackCursor StackCursor

derive instance eqCursor :: Eq Cursor
derive instance ordCursor :: Ord Cursor

-- | A destination for some movable object.
data Destination
  = ToFoundation
  | ToTableau TableauIndex

derive instance eqDestination :: Eq Destination
derive instance ordDestination :: Ord Destination

data Move
  = ResetStock
  | AdvanceStock
  | MoveStack Cursor Destination

derive instance eqMove :: Eq Move
derive instance ordMove :: Ord Move

applyMove :: Move -> GameM Unit
applyMove =
  case _ of
    ResetStock -> resetStock
    AdvanceStock -> advanceStock
    MoveStack csr dest -> moveStack csr dest

withStock :: forall a. (Stock -> Maybe (Tuple a Stock)) -> GameM a
withStock f = do
  stock <- gets (_.stock <<< unwrap)
  Tuple x newStock <- lift $ f stock
  modify (over Game (_ { stock = newStock }))
  pure x

withTableau :: forall a.
  TableauIndex -> (Tableau -> Maybe (Tuple a Tableau)) -> GameM a
withTableau ix f = do
  tableaux <- gets (_.tableaux <<< unwrap)
  Tuple x newTableaux <- lift $ Tableaux.modify ix f tableaux
  modify (over Game (_ { tableaux = newTableaux }))
  pure x

-- | Attempt to add a single card to the appropriate foundation pile.
addToFoundations :: Card -> GameM Unit
addToFoundations card = do
  foundations <- gets (_.foundations <<< unwrap)
  newFoundations <- lift $ Foundations.addCard card foundations
  modify (over Game (_ { foundations = newFoundations }))
  pure unit

resetStock :: GameM Unit
resetStock =
  withStock (map (Tuple unit) <<< Stock.reset)

advanceStock :: GameM Unit
advanceStock =
  withStock (map (Tuple unit) <<< Stock.advance)

-- | Attempt to take the top-most card from the waste pile.
takeWaste :: GameM Card
takeWaste =
  withStock \s ->
    map (\{ card, stock } -> Tuple card stock) (Stock.take s)

-- | Attempt to take a stack from a tableau.
takeTableauStack :: StackCursor -> GameM Stack
takeTableauStack { ix, size } =
  withTableau ix (Tableaux.takeStack size)

-- | Attempt to pick up the `Stack` pointed to by a `Cursor`.
takeCursor :: Cursor -> GameM Stack
takeCursor =
  case _ of
    WasteCursor ->
      map Stack.singleton takeWaste
    StackCursor csr  ->
      takeTableauStack csr

-- | Attempt to move the `Stack` pointed to by the given `Cursor` to the given
-- | `Destination`.
moveStack :: Cursor -> Destination -> GameM Unit
moveStack csr dest = do
  stack <- takeCursor csr
  case dest of
    ToTableau ix ->
      withTableau ix (map (Tuple unit) <<< Tableaux.addStack stack)
    ToFoundation -> do
      card <- lift $ Stack.unSingleton stack
      addToFoundations card

fromDeck :: Deck -> Game
fromDeck deck =
  case Tableaux.initial (Deck.run deck) of
    { tableaux, leftover } ->
      Game
        { stock: Stock.initial (Array.toUnfoldable leftover)
        , foundations: Foundations.initial
        , tableaux
        }

fromSeed :: Int -> Game
fromSeed = fromDeck <<< Deck.fromSeed

isWon :: Game -> Boolean
isWon (Game { foundations }) = Foundations.isComplete foundations
