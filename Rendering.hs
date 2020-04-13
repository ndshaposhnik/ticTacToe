module Rendering where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Game

fps :: Int
fps = 30

backgroundColor :: Color
backgroundColor = black

playerXColor :: Color
playerXColor = red

playerOColor :: Color
playerOColor = blue

drawColor :: Color
drawColor = greyN 0.5

screenPosX :: Int
screenPosX = 100

screenPosY :: Int
screenPosY = 100

drawGrid :: Picture
drawGrid = undefined

drawWorld :: Game -> Picture
drawWorld game = mconcat pictures where
    pictures = [drawGrid {- ADD PICTURES -}]