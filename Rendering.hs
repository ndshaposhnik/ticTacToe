module Rendering where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Game

size :: Float
size = fromIntegral screenSize

cell :: Float
cell = size / fromIntegral n

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
drawGrid = color colorGrid $ Pictures $ map (\x -> line [(x, -size / 2), (x, size / 2)]) points
                                     ++ map (\x -> line [(-size / 2, x), (size / 2, x)]) points
                                     where points = [-size / 2 + (fromIntegral i) * cell | i <- [0..n]]

getShift :: Int -> Float
getShift x = -cell * (fromIntegral (n - 1)) / 2 + (fromIntegral x) * cell

drawX :: (Int, Int) -> Color -> Picture
drawX (x, y) c = translate (getShift x) (getShift y) $ color c $ Pictures [rotate 45 p, rotate (-45) p] where
    p = Polygon [(-w, -h), (-w, h), (w, h), (w, -h)]
    w = 0.5  * cell
    h = 0.05 * cell

drawO :: (Int, Int) -> Color -> Picture
drawO (x, y) c = color c $ translate (getShift x) (getShift y) $ ThickCircle rad thick where
    rad   = 0.4  * cell
    thick = 0.08 * cell

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
