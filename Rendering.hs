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

drawGrid :: Color -> Picture
drawGrid col = color col $ Pictures $ map (\x -> line [(x, -size / 2), (x, size / 2)]) points
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

drawCellRunning :: (Int, Int) -> Field -> Picture
drawCellRunning key field = case field ! key of
    Just PlayerX -> drawX key colorX
    Just PlayerO -> drawO key colorO
    Nothing -> Blank

drawCellEnd :: (Int, Int) -> Field -> Color -> Picture
drawCellEnd key field col = case field ! key of
    Just PlayerX -> drawX key col
    Just PlayerO -> drawO key col
    Nothing -> Blank

drawFieldRunning :: Field -> Picture
drawFieldRunning field = Pictures [drawCellRunning key field | key <- range ((0, 0), (n - 1, n - 1))]

drawFieldEnd :: Field -> Color -> Picture
drawFieldEnd field col = Pictures [drawCellEnd key field col | key <- range ((0, 0), (n - 1, n - 1))]

playerToColor :: Player -> Color
playerToColor PlayerX = colorX
playerToColor PlayerO = colorO

drawWorld :: Game -> Picture
drawWorld (Game field state _) = case state of
    Running               -> Pictures [drawGrid colorGrid, drawFieldRunning field]
    EndGame Nothing       -> Pictures [drawGrid colorGrid, drawFieldEnd field colorGrid]
    EndGame (Just player) -> Pictures [drawGrid color    , drawFieldEnd field color] 
        where color = playerToColor player