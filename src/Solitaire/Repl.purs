module Solitaire.Repl where

import Solitaire.Prelude
import Data.String as String
import Data.Int as Int
import Node.ReadLine (READLINE)
import Node.ReadLine as RL
import Node.FS (FS)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Test.QuickCheck.LCG (mkSeed)
import Solitaire.Tableaux (TableauIndex, ixFromInt)
import Solitaire.Game (Move(..), MoveResult(..), Cursor(..), Destination(..))
import Solitaire.Game as Game
import Solitaire.Deck (randomDeck)
import Solitaire.Ansi as Ansi

type EffR = Eff
  ( console :: CONSOLE
  , ref :: REF
  , readline :: READLINE
  , exception :: EXCEPTION
  , fs :: FS
  , random :: RANDOM
  )

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

main :: EffR Unit
main = do
  ref <- newRef (Game.fromSeed (mkSeed 0))
  iface <- RL.createConsoleInterface RL.noCompletion
  start ref iface

  where
  start ref iface = do
    writeRef ref =<< map Game.fromDeck randomDeck

    log "========"
    log "New Game"
    log "========"
    readRef ref >>= (log <<< Ansi.game)

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
        _ ->
          pure unit

    handleCommand =
      case _ of
        MoveCommand m ->
          move m
        DumpToFile file -> do
          g <- readRef ref
          FS.writeTextFile UTF8 file (stringify (encodeJson g))

    move m = do
      g <- readRef ref
      case Game.applyMove m g of
        IllegalMove ->
          log "Illegal move"
        MoveOk g' -> do
          writeRef ref g'
          log $ Ansi.game g'
        GameWon -> do
          log "Congratulations, you won! Play again [Y/n]?"
          RL.setLineHandler iface victoryLineHandler
