module Logic where

import Data.Array
import Graphics.Gloss.Interface.IO.Interact

import Game

rangeI :: [Int]
rangeI = range (0, (n - 1))

rangePair :: [(Int, Int)]
rangePair = range ((0, 0), (n - 1, (n - 1)))

initialGame :: Game
initialGame = Game { field = array rangeField listVal, 
                     state = Running, 
                     currentPlayer = PlayerX } where
    rangeField = ((0,0), (n - 1, n - 1))
    listVal = [(key, Nothing) | key <- rangePair]


getRow :: Float -> Int
getRow f = ff `div` (screenSize `div` n)
         where ff = floor f + screenSize `div` 2

posToCoord :: (Float, Float) -> (Int, Int)
posToCoord (a, b) = (getRow a, getRow b)

changePlayer :: Player -> Player
changePlayer PlayerX = PlayerO
changePlayer PlayerO = PlayerX

sumAll :: Field -> Int
sumAll field = sum $ map (\x -> if x == Nothing then 0 else 1) [field ! key | key <- rangePair]

sumLine :: [Cell] -> Player -> Int
sumLine ps player = sum $ map (\p -> if p == Just player then 1 else 0) ps

countRow :: Field -> Int -> Player -> Int
countRow field row = sumLine [field ! (row, i) | i <- rangeI]

countColumn :: Field -> Int -> Player -> Int
countColumn field column = sumLine [field ! (i, column) | i <- rangeI]

countRows :: Field -> Player -> [Int]
countRows field player = [countRow field i player | i <- rangeI]

countColumns :: Field -> Player -> [Int]
countColumns field player = [countColumn field i player | i <- rangeI]

countMainDiag :: Field -> Player -> Int
countMainDiag field = sumLine [field ! (i, i) | i <- rangeI]

countSideDiag :: Field -> Player -> Int
countSideDiag field = sumLine [field ! (i, n - i - 1) | i <- rangeI]

isWinner :: Field -> Player -> Bool
isWinner field player = (maximum $ (countRows field player) ++ (countColumns field player) ++ 
                                     [countMainDiag field player, countSideDiag field player]) == n

nextTurn :: Game -> Game
nextTurn (Game field state player) = case isWinner field player of
    True -> Game field (EndGame (Just player)) player
    False -> case sumAll field == n * n of
        True -> Game field (EndGame Nothing) player
        _    -> Game field state (changePlayer player)

isValid :: (Int, Int) -> Bool
isValid (a, b) = 0 <= a && a < n && 0 <= b && b < n

updateGame :: Event -> Game -> Game
updateGame (EventKey (MouseButton LeftButton) Up _ position) (Game field state player) = case isValid key of
    True -> case state of
        EndGame _ -> initialGame
        Running   -> case field ! key of
            Nothing -> nextTurn $ Game (field // [(key, Just player)]) state player
            _ -> Game field state player
    False -> Game field state player
    where key = posToCoord position
updateGame _ game = game
