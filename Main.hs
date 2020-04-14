module Main where

import Graphics.Gloss

import Game (screenSize, Game)
import Logic (initialGame, updateGame)
import Rendering (backgroundColor, fps, drawWorld)

screenPosX :: Int
screenPosX = 100

screenPosY :: Int
screenPosY = 200

window = InWindow "Tic Tac Toe" (screenSize, screenSize) (screenPosY, screenPosX)

main :: IO ()
main = play window backgroundColor fps initialGame drawWorld updateGame (const id)
