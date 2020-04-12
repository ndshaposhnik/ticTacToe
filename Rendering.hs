module Rendering where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Game

fps :: Int
fps = 30

backgroundColor :: Color
backgroundColor = black

screenPosX :: Int
screenPosX = 100

screenPosY :: Int
screenPosY = 100

drawWorld :: Game -> Picture
drawWorld = undefined