module Solitaire.Repl where

import Solitaire.Prelude
import Data.String as String
import Data.Int as Int
import Node.ReadLine (READLINE)
import Node.ReadLine as RL
import Node.FS (FS)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Solitaire.Tableaux (TableauIndex)
import Solitaire.Game (Game, Move(..), CardCursor(..), StackCursor, initialGame, applyMove)
import Solitaire.Deck as Deck
import Solitaire.Ansi as Ansi

type EffR = Eff (console :: CONSOLE, ref :: REF, readline :: READLINE, exception :: EXCEPTION, fs :: FS)

newGameRef :: Int -> EffR (Ref Game)
newGameRef = newRef <<< initialGame <<< Deck.fromSeed

move :: Ref Game -> Move -> EffR Unit
move gameRef m = do
  g <- readRef gameRef
  case execStateT (applyMove m) g of
    Nothing ->
      log $ "Invalid move"
    Just g' -> do
      writeRef gameRef g'
      log $ Ansi.game g'

ix :: Int -> TableauIndex
ix i =
  case toEnum i of
    Just r -> r
    Nothing
      | i <= 0    -> bottom
      | otherwise -> top

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
        ["w", ix] ->
          map WasteToTableau (parseTableauIndex ix)
        ["f", csr] ->
          map MoveToFoundations (parseCardCursor csr)
        ["s", csr, ix] ->
          MoveStack <$> parseStackCursor csr <*> parseTableauIndex ix
        _ ->
          Nothing

  where
  parseTableauIndex :: String -> Maybe TableauIndex
  parseTableauIndex =
    map ix <<< Int.fromString

  parseCardCursor :: String -> Maybe CardCursor
  parseCardCursor =
    case _ of
      "w" -> Just FromWaste
      other -> map FromTableau (parseTableauIndex other)

  parseStackCursor :: String -> Maybe StackCursor
  parseStackCursor str =
    case String.split (String.Pattern "-") str of
      [ix, size] ->
        { ix: _, size: _ } <$> parseTableauIndex ix
                           <*> Int.fromString size
      _ ->
        Nothing

main :: EffR Unit
main = do
  i <- RL.createConsoleInterface RL.noCompletion

  let seed = 0
  ref <- newGameRef seed

  log "New Game"
  log "========"
  readRef ref >>= (log <<< Ansi.game)

  RL.setPrompt "> " 2 i
  RL.setLineHandler i \line -> do
    maybe (log "Unable to parse move") (handleCommand ref) (parseCommand line)
    RL.prompt i

  RL.prompt i

  where
  handleCommand ref =
    case _ of
      MoveCommand m ->
        move ref m
      DumpToFile file -> do
        g <- readRef ref
        FS.writeTextFile UTF8 file (stringify (encodeJson g))
