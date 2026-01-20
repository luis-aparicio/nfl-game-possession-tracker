module Game.Winners where

import Prelude

import Data.Maybe (Maybe(..))

type QuarterWinners = 
  { q1 :: Maybe String
  , q2 :: Maybe String
  , q3 :: Maybe String
  , q4 :: Maybe String
  , ot :: Maybe String
  }

setWinner :: String -> QuarterWinners -> QuarterWinners
setWinner winner winners = case winners of
  { q1: Nothing, q2: _, q3: _, q4: _, ot: _ } -> winners { q1 = Just winner }
  { q1: Just _, q2: Nothing, q3: _, q4: _, ot: _ } -> winners { q2 = Just winner }
  { q1: Just _, q2: Just _, q3: Nothing, q4: _, ot: _ } -> winners { q3 = Just winner }
  { q1: Just _, q2: Just _, q3: Just _, q4: Nothing, ot: _ } -> winners { q4 = Just winner }
  { q1: Just _, q2: Just _, q3: Just _, q4: Just _, ot: Nothing } -> winners { ot = Just winner }
  _ -> winners

checkQuarter :: Int -> QuarterWinners -> Boolean
checkQuarter quarter winners = case quarter of
  2 -> winners.q1 /= Nothing
  3 -> winners.q2 /= Nothing
  4 -> winners.q3 /= Nothing
  5 -> winners.q4 /= Nothing
  _ -> false
