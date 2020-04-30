-- | The web UI is entirely driven through keyboard bindings (for now).
-- |
-- | The UI has two states:
-- | 1) Selection. In this state there is a cursor which can be moved around
-- |    the tableaux
-- | 2) A stack has been selected; waiting for a command to see what to do with
-- |    it
-- |
-- | The UI starts in state 1. Choosing a selection (either a stack from a
-- | tableau or the topmost waste card) transitions from state 1 to state 2.
-- | After entering state 2, we can return to state 1 by either a) moving the
-- | selection, or b) discarding (allowing the user to choose a different
-- | selection).
-- |
-- | Therefore, in state 1, the possible actions are:
-- | * move cursor (up/down/left/right), 'hjkl'
-- | * select a stack from a tableau (spacebar)
-- | * select the topmost card in the waste pile 'g'
-- | * advance/reset stock 'a'
-- |
-- | And in state 2, the possible actions are:
-- | * Move to a tableau: 'q' 'w' 'e' 'r' 't' 'y' 'u'
-- | * Move to foundation pile: 'f'
-- | * Discard selection: 'd'
module Solitaire.Web where

import Solitaire.Prelude
import Data.Int as Int
import Data.Array as Array
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

import Solitaire.Card (Colour(..), cardSuit, suitColour, displayCard)
import Solitaire.Game (Game, MoveResult(..))
import Solitaire.Game as Game
import Solitaire.Deck (Deck, randomDeck)
import Solitaire.Tableaux (TableauIndex)
import Solitaire.Tableaux as Tableaux
import Solitaire.Web.Positioning
  (CardDisplay(..), CardPosition, evalPosition, displayGame, totalWidth,
   totalHeight, cardWidth, cardHeight, displayCursor, displayStackCursor)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import CSS as CSS
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  deck <- liftEffect randomDeck
  runUI (uiComponent deck) unit body

uiComponent ::
  forall query input output m .
  Deck ->
  H.Component HH.HTML query input output m
uiComponent deck =
  H.mkComponent
    { initialState: const (initialState deck)
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction })
    , render
    }
  where
  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction =
    case _ of
      MoveCursor dir -> do
        onlySelection \_ ->
          modify_ (moveCursor dir)
      SelectStack -> do
        onlySelection \csr ->
          modify_ \st -> st { ui = WaitingForAction (Game.StackCursor csr) }
      SelectWaste -> do
        onlySelection \_ ->
          modify_ \st -> st { ui = WaitingForAction Game.WasteCursor }
      AdvanceStock -> do
        onlySelection \_ ->
          modify_ \st -> st { game = advanceOrReset st.game }
      MoveToTableau ix -> do
        onlyWaiting \csr ->
          applyMove (Game.MoveStack csr (Game.ToTableau ix))
      MoveToFoundation -> do
        onlyWaiting \csr ->
          applyMove (Game.MoveStack csr Game.ToFoundation)
      DiscardSelection -> do
        discardSelection

    where
    -- Only do anything if we are in the `Selection` UI state.
    onlySelection act = do
      uiState <- gets _.ui
      case uiState of
        Selection csr ->
          act csr
        _ ->
          pure unit

    -- Only do anything if we are in the `WaitingForAction` UI state.
    onlyWaiting act = do
      uiState <- gets _.ui
      case uiState of
        WaitingForAction csr ->
          act csr
        _ ->
          pure unit

    -- Transition from state 2 back to state 1 without doing anything, i.e.
    -- after cancelling, or after having applied a move successfully.
    discardSelection = do
      game <- gets _.game
      onlyWaiting \csr' ->
        modify_ \st -> st { ui = Selection $ adjustWith succOrLoop game $
          case csr' of
            Game.StackCursor csr ->
              csr
            Game.WasteCursor ->
              { ix: bottom, size: 1 }
        }

    applyMove move = do
      game <- gets _.game
      case Game.applyMove move game of
        Game.GameWon ->
          -- todo: win state
          pure unit
        Game.IllegalMove ->
          -- todo: wiggle animation
          pure unit
        Game.MoveOk game' -> do
          modify_ \st -> st { game = game' }
          discardSelection

  render :: State -> HTML m
  render st =
    let
      props =
        [ HP.class_ (HH.ClassName "game-container")
        , style do CSS.width (px totalWidth)
                   CSS.height (px totalHeight)
        , HP.tabIndex 1 -- to allow receiving keyboard events
        , HE.onKeyDown interpretKey
        ]
      cards =
        map renderCard (displayGame st.game)
      selection =
        [ renderSelection st ]
      notFocusedReminder =
        [ HH.div
            [ HP.class_ (HH.ClassName "unfocused") ]
            [ HH.p_ [HH.text "Click here to play"]
            , HH.p_ [HH.text "Controls:"]
            , HH.p_ [HH.text "hjkl [move selection]"]
            , HH.p_ [HH.text "Space [fix selection]"]
            , HH.p_ [HH.text "g [select top card from waste pile]"]
            , HH.p_ [HH.text "a [draw into waste pile]"]
            , HH.p_ [HH.text "qwertyu [move selection to tableau]"]
            , HH.p_ [HH.text "f [move selected card to foundation pile]"]
            , HH.p_ [HH.text "d [discard selection]"]
            ]
        ]
    in
      HH.div
        props
        (cards <> selection <> notFocusedReminder)

    where
    interpretKey :: KeyboardEvent -> Maybe Action
    interpretKey ev =
      case KeyboardEvent.code ev of
        "KeyH" ->
          Just (MoveCursor DLeft)
        "KeyJ" ->
          Just (MoveCursor DDown)
        "KeyK" ->
          Just (MoveCursor DUp)
        "KeyL" ->
          Just (MoveCursor DRight)
        "Space" ->
          Just SelectStack
        "KeyG" ->
          Just SelectWaste
        "KeyA" ->
          Just AdvanceStock
        "KeyQ" ->
          Just (MoveToTableau (Tableaux.ixFromInt 0))
        "KeyW" ->
          Just (MoveToTableau (Tableaux.ixFromInt 1))
        "KeyE" ->
          Just (MoveToTableau (Tableaux.ixFromInt 2))
        "KeyR" ->
          Just (MoveToTableau (Tableaux.ixFromInt 3))
        "KeyT" ->
          Just (MoveToTableau (Tableaux.ixFromInt 4))
        "KeyY" ->
          Just (MoveToTableau (Tableaux.ixFromInt 5))
        "KeyU" ->
          Just (MoveToTableau (Tableaux.ixFromInt 6))
        "KeyF" ->
          Just MoveToFoundation
        "KeyD" ->
          Just DiscardSelection
        other ->
          unsafePerformEffect do
            log $ "unrecognised key code: " <> other
            pure Nothing

    renderCard :: Tuple CardDisplay CardPosition -> HTML m
    renderCard (Tuple display pos) =
      case evalPosition pos of
        Tuple left_ top_ ->
          HH.div [ HP.classes (classesFor display)
                 , style do CSS.top (px top_)
                            CSS.left (px left_)
                            CSS.width (px cardWidth)
                            CSS.height (px cardHeight)
                 ]
                 [ HH.text (textFor display)
                 ]

    renderSelection :: State -> HTML m
    renderSelection { ui, game } =
      let
        selectionClasses =
          ["selection"] <>
            case ui of
              WaitingForAction _ ->
                ["selection-fixed"]
              _ ->
                []

        go { pos, height } =
          case evalPosition pos of
            Tuple left_ top_ ->
              HH.div
                [ HP.classes $ map HH.ClassName selectionClasses
                 , style do CSS.top (px top_)
                            CSS.left (px left_)
                            CSS.width (px cardWidth)
                            CSS.height (px height)
                ]
                []
      in
        go $ case ui of
          Selection stackCsr ->
            displayStackCursor game stackCsr
          WaitingForAction csr ->
            displayCursor game csr

    px = CSS.px <<< Int.toNumber

    classesFor :: CardDisplay -> Array HH.ClassName
    classesFor display =
      map HH.ClassName $
        (Array.snoc ["card"]) $
          case display of
            FaceUp c ->
              case suitColour (cardSuit c) of
                Red -> "red"
                Black -> "black"
            FaceDown ->
              "face-down"
            EmptySpace ->
              "empty-space"

    textFor :: CardDisplay -> String
    textFor =
      case _ of
        FaceUp c ->
          displayCard c
        _ ->
          ""

data Action
  -- Move the stack cursor in a given direction
  = MoveCursor Direction
  -- Choose the stack pointed to by the current cursor as the selection
  | SelectStack
  -- Choose the topmost waste card as the selection
  | SelectWaste
  -- Advance/reset the stock pile
  | AdvanceStock
  -- Move the current selection to a tableau
  | MoveToTableau TableauIndex
  -- Move the current selection to the appropriate foundation pile
  | MoveToFoundation
  -- Discard the current selection
  | DiscardSelection

derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

type HTML m = H.ComponentHTML Action () m

data Direction
  = DUp
  | DDown
  | DLeft
  | DRight

derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show = genericShow

type State =
  { ui :: UIState
  , game :: Game
  }

data UIState
  = Selection Game.StackCursor
  | WaitingForAction Game.Cursor

derive instance genericUIState :: Generic UIState _

instance showUIState :: Show UIState where
  show = genericShow

moveCursor :: Direction -> State -> State
moveCursor dir st@{ ui, game } =
  case ui of
    Selection csr ->
      let
        newCsr = adjust (moveCursorInDirection dir csr)
      in
        st { ui = Selection newCsr }
    _ ->
      st

  where
  adjust csr =
    case dir of
      DLeft ->
        adjustWith predOrLoop game csr
      _ ->
        adjustWith succOrLoop game csr

-- This function ensures that the cursor points at something valid
adjustWith :: (TableauIndex -> TableauIndex) -> Game -> Game.StackCursor -> Game.StackCursor
adjustWith next game = adjustSize >>> adjustIndex
  where
  -- Ensure that the cursor doesn't ask for a stack which is too large or too
  -- small
  adjustSize { ix, size } =
    let
      tableau = Tableaux.get ix (unwrap game).tableaux
      actualSize = fromMaybe 1 (Tableaux.stackSize tableau)
      clampedSize = clamp 1 actualSize size
    in
      { ix, size: clampedSize }

  -- Skip over empty spaces
  adjustIndex csr@{ ix, size } =
    let
      tableau = Tableaux.get ix (unwrap game).tableaux
      nextCsr = { ix: next ix, size }
    in
      if tableau == Tableaux.EmptySpace
        then adjustIndex nextCsr
        else csr

moveCursorInDirection :: Direction -> Game.StackCursor -> Game.StackCursor
moveCursorInDirection dir { ix, size } =
  case dir of
    DUp ->
      { ix, size: size + 1 }
    DDown ->
      { ix, size: size - 1 }
    DLeft ->
      { ix: predOrLoop ix, size }
    DRight ->
      { ix: succOrLoop ix, size }

advanceOrReset :: Game -> Game
advanceOrReset g =
  case Game.applyMove Game.AdvanceStock g of
    MoveOk g' ->
      g'
    IllegalMove ->
      case Game.applyMove Game.ResetStock g of
        MoveOk g' ->
          g'
        _ ->
          unsafeCrashWith "must be able to either advance or reset stock"
    GameWon ->
      unsafeCrashWith "cannot win by advancing stock"

initialState :: Deck -> State
initialState deck =
  { ui: Selection { ix: Tableaux.ixFromInt 0, size: 1 }
  , game: Game.fromDeck deck
  }
