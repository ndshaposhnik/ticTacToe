module Game where

import Data.Array

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | EndGame (Maybe Player)

type Field = Array (Int, Int) Cell

data Game = Game { field :: Field, state :: State, currentPlayer :: Player }

screenWidth :: Int
screenWidth = 600

screenHeight :: Int
screenHeight = 600

n :: Int
n = 3