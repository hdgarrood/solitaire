module Solitaire.Repl where

import Solitaire.Prelude
import Data.String as String
import Data.Int as Int
import Effect.Ref as Ref
import Node.ReadLine as RL
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Random.LCG (mkSeed)
import Solitaire.Tableaux (TableauIndex, ixFromInt)
import Solitaire.Game (Move(..), MoveResult(..), Cursor(..), Destination(..))
import Solitaire.Game as Game
import Solitaire.Deck (randomDeck)
import Solitaire.Ansi as Ansi

data Command
  = MoveCommand Move
  | DumpToFile String

parseCommand :: String -> Maybe Command
parseCommand str =
  case String.split (String.Pattern " ") str of
    ["dump", filePath] ->
      Just $ DumpToFile filePath
    _ ->
      MoveCommand <$> parseMove str

parseMove :: String -> Maybe Move
parseMove =
  case _ of
    "r" -> Just ResetStock
    "a" -> Just AdvanceStock
    str ->
      case String.split (String.Pattern " ") str of
        ["m", csr, dest] ->
          MoveStack <$> parseCursor csr <*> parseDestination dest
        _ ->
          Nothing

  where
  parseTableauIndex :: String -> Maybe TableauIndex
  parseTableauIndex =
    map ixFromInt <<< Int.fromString

  parseCursor :: String -> Maybe Cursor
  parseCursor str =
    case String.split (String.Pattern "-") str of
      ["w"] ->
        Just WasteCursor
      [ix, size] ->
        map StackCursor $
          { ix: _, size: _ } <$> parseTableauIndex ix
                             <*> Int.fromString size
      _ ->
        Nothing

  parseDestination :: String -> Maybe Destination
  parseDestination =
    case _ of
      "f" ->
        Just ToFoundation
      str ->
        map ToTableau (parseTableauIndex str)

main :: Effect Unit
main = do
  ref <- Ref.new (Game.fromSeed (mkSeed 0))
  iface <- RL.createConsoleInterface RL.noCompletion
  welcome
  start ref iface

  where
  start ref iface = do
    newGame <- map Game.fromDeck randomDeck
    Ref.write newGame ref

    log "========"
    log "New Game"
    log "========"
    log (Ansi.game newGame)

    RL.setPrompt "> " 2 iface
    RL.setLineHandler iface normalLineHandler
    RL.prompt iface

    where
    normalLineHandler line = do
      maybe (log "Unable to parse move")
            handleCommand
            (parseCommand (String.trim line))
      RL.prompt iface

    victoryLineHandler line = do
      case String.toUpper (String.trim line) of
        "" ->
          start ref iface
        "Y" ->
          start ref iface
        "N" ->
          RL.close iface
        _ ->
          RL.prompt iface

    handleCommand =
      case _ of
        MoveCommand m ->
          move m
        DumpToFile file -> do
          g <- Ref.read ref
          FS.writeTextFile UTF8 file (stringify (encodeJson g))

    move m = do
      g <- Ref.read ref
      case Game.applyMove m g of
        IllegalMove ->
          log "Illegal move"
        MoveOk g' -> do
          Ref.write g' ref
          log $ Ansi.game g'
        GameWon -> do
          log "Congratulations, you won! Play again [Y/n]?"
          RL.setLineHandler iface victoryLineHandler

  welcome = do
    log "===================="
    log "PureScript Solitaire"
    log "===================="
    log "Controls:"
    log " a               - draw into the waste pile"
    log " r               - reset the waste pile"
    log " m <src> <dest>  - move a card or stack of cards"
    log ""
    log " <src> should be one of:"
    log "   w   - take a card from the waste pile"
    log "   N-M - take M cards from the Nth (0-based) tableau"
    log ""
    log " <dest> should be one of:"
    log "   f   - send the card to a foundation pile"
    log "   N   - send the card / stack of cards to the Nth (0-based) tableau"
