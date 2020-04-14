module Rendering where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Game

size :: Float
size = fromIntegral screenSize

fps :: Int
fps = 30

backgroundColor :: Color
backgroundColor = black

colorX :: Color
colorX = red

colorO :: Color
colorO = blue

colorGrid :: Color
colorGrid = greyN 0.5

drawGrid :: Picture
drawGrid = color colorGrid $ Pictures [line [(size / 6, size / 2), (size / 6, -size / 2)],
                                       line [(-size / 6, size / 2), (-size / 6, -size / 2)],
                                       line [(-size / 2, size / 6), (size / 2, size / 6)],
                                       line [(-size / 2, -size / 6), (size / 2, -size / 6)]]

getShift :: Int -> Float
getShift x = -size / 3 + (fromIntegral x) * (size / 3)

drawX :: (Int, Int) -> Color -> Picture
drawX (x, y) c = translate (getShift x) (getShift y) $ color c $ 
                    Pictures [line [(-sizen, -sizen), (sizen, sizen)],
                              line [(-sizen, sizen), (sizen, -sizen)]] where
                                sizen = 50


drawO :: (Int, Int) -> Color -> Picture
drawO (x, y) c = color c $ translate (getShift x) (getShift y) $ ThickCircle (sizen) (5) where
                                sizen = 45

getColor :: (Int, Int) -> Game -> Color
getColor key (Game field state _) = case state of
    EndGame winner -> case winner of 
        Nothing      -> colorGrid
        Just PlayerX -> colorX
        Just PlayerO -> colorO
    Running -> case field ! key of
        Just PlayerX -> colorX
        Just PlayerO -> colorO
        Nothing -> black

drawCell :: (Int, Int) -> Game -> Picture
drawCell key (Game field state _) = case field ! key of
    Just PlayerX -> drawX key $ getColor key game
    Just PlayerO -> drawO key $ getColor key game
    Nothing -> Blank
    where game = Game field state undefined


drawField :: Game -> Picture
drawField game = Pictures [drawCell key game | key <- range ((0, 0), (n - 1, n - 1))]

drawWorld :: Game -> Picture
drawWorld game = Pictures pictures where
    pictures = [drawGrid, drawField game]