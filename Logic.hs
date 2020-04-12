module Logic where

import Data.Array
import Graphics.Gloss.Interface.IO.Interact

import Game

initialGame :: Game
initialGame = Game { field = array rangePair listVal, 
                     state = Running, 
                     currentPlayer = PlayerX } where
    rangePair = ((0, 0), (n, n))
    listVal = [((i, j), Nothing) | i <- [0..(n - 1)], j <- [0..(n - 1)]]


getRow :: Float -> Int
getRow f | (floor f) == screenHeight = n - 1
         | otherwise = (floor f) `div` (screenHeight `div` n)

getColumn :: Float -> Int
getColumn f | floor f == screenWidth = n - 1
            | otherwise = (floor f) `div` (screenWidth `div` n)

posToCoord :: (Float, Float) -> (Int, Int)
posToCoord (a, b) = (getRow a, getColumn b)

changePlayer :: Player -> Player
changePlayer PlayerX = PlayerO
changePlayer PlayerO = PlayerX

updateGame :: Event -> Game -> Game
updateGame (EventKey (MouseButton LeftButton) Up _ position) (Game field state player) = case state of
    EndGame -> initialGame
    Running -> case field ! key of
        Nothing -> Game (field // [(key, Just player)]) state (changePlayer player)
        _ -> Game field state player
        where key = posToCoord position