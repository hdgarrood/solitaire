module Solitaire.Stack
  ( Stack
  , run
  , singleton
  , unSingleton
  , size
  , split
  , pop
  , push
  , highCard
  , lowCard
  , join
  , fromCards
  ) where

import Solitaire.Prelude
import Data.List as List
import Solitaire.Card (Card(..), suitColour)

-- | A `Stack` is a nonempty stack of cards of alternating suit colour and
-- | descending rank, usually fanned on the table. A stack is represented as
-- | a `List` with the lowest card first.
newtype Stack
  = Stack (List Card)

derive newtype instance eqStack :: Eq Stack
derive newtype instance encodeJsonStack :: EncodeJson Stack
derive newtype instance decodeJsonStack :: DecodeJson Stack

-- | Create a `Stack` from a single `Card`.
singleton :: Card -> Stack
singleton c = Stack (c : Nil)

-- | If a `Stack` consists of just one `Card`, get that card, otherwise
-- | `Nothing`.
unSingleton :: Stack -> Maybe Card
unSingleton =
  case _ of
    Stack (c:Nil) -> Just c
    _ -> Nothing

-- | Unwrap a `Stack` to access the underlying `List`.
run :: Stack -> List Card
run (Stack cs) = cs

size :: Stack -> Int
size = List.length <<< run

-- | Attempt to split a `Stack`. The `Int` argument specifies the number of
-- | cards which should be in the bottom stack after splitting.
split :: Int -> Stack -> Maybe { top :: Stack, bottom :: Stack }
split i (Stack cs) =
  let
    len = List.length cs
    isOk = between 1 (len - 1)
  in
    if isOk i
      then
        Just { top: Stack (List.drop i cs)
             , bottom: Stack (List.take i cs)
             }
      else
        Nothing

-- | Remove a single card from the bottom of a `Stack`. Returns the remaining
-- | `Stack`, or `Nothing` if the removed card was the last card in the
-- | `Stack`. 
pop :: Stack -> { card :: Card, remaining :: Maybe Stack }
pop =
  case _ of
    Stack (card:Nil) ->
      { card, remaining: Nothing }
    Stack (card:rest) ->
      { card, remaining: Just (Stack rest) }
    _ ->
      unsafeCrashWith "Stack invariant violated"

-- | Add a single card to the bottom of a `Stack`. Returns `Nothing` if the
-- | card could not be added (because it was incompatible).
push :: Card -> Stack -> Maybe Stack
push c stack =
  if c `canPlaceOn` lowCard stack
    then Just (Stack (c : run stack))
    else Nothing

-- | Get the highest card in the `Stack`.
highCard :: Stack -> Card
highCard (Stack cs) =
  case List.last cs of
    Just c -> c
    Nothing -> unsafeCrashWith "Stack invariant violated"

-- | Get the lowest card in the `Stack`.
lowCard :: Stack -> Card
lowCard (Stack cs) = 
  case List.head cs of
    Just c -> c
    Nothing -> unsafeCrashWith "Stack invariant violated"

-- | Attempt to join two stacks; returns `Nothing` if they are not compatible.
-- | The first argument should be the stack to go on the bottom.
join :: Stack -> Stack -> Maybe Stack
join below above =
  if highCard below `canPlaceOn` lowCard above
    then Just (unsafeJoin below above)
    else Nothing
  where
  unsafeJoin (Stack xs) (Stack ys) = Stack (xs <> ys)

fromCards :: List Card -> Maybe Stack
fromCards =
  case _ of
    c:cs ->
      List.foldM (flip push) (singleton c) cs
    Nil ->
      Nothing

canPlaceOn :: Card -> Card -> Boolean
canPlaceOn (Card suit1 rank1) (Card suit2 rank2) =
  suitColour suit1 /= suitColour suit2 && succ rank1 == Just rank2
