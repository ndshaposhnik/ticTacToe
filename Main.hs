module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Logic
import Rendering

window = InWindow "Tic Tac Toe" (screenWidth, screenHeight) (screenPosX, screenPosY)

main :: IO ()
main = play window backgroundColor fps initialGame drawWorld updateGame (const id)
