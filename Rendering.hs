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

drawX :: (Int, Int) -> Picture
drawX (x, y) = translate (getShift x) (getShift y) $ Pictures [rotate 45 p, rotate (-45) p] where
    p = Polygon [(-w, -h), (-w, h), (w, h), (w, -h)]
    w = 0.5  * cell
    h = 0.05 * cell

drawO :: (Int, Int) -> Picture
drawO (x, y) = translate (getShift x) (getShift y) $ ThickCircle rad thick where
    rad   = 0.4  * cell
    thick = 0.08 * cell

drawCellRunning :: (Int, Int) -> Field -> Picture
drawCellRunning key field = case field ! key of
    Just PlayerX -> color colorX $ drawX key
    Just PlayerO -> color colorO $ drawO key
    Nothing      -> Blank

drawCellEnd :: (Int, Int) -> Field -> Picture
drawCellEnd key field = case field ! key of
    Just PlayerX -> drawX key
    Just PlayerO -> drawO key
    Nothing -> Blank

drawFieldRunning :: Field -> Picture
drawFieldRunning field = Pictures [drawCellRunning key field | key <- range ((0, 0), (n - 1, n - 1))]

drawFieldEnd :: Field -> Color -> Picture
drawFieldEnd field col = color col $ Pictures [drawCellEnd key field | key <- range ((0, 0), (n - 1, n - 1))]

playerToColor :: Maybe Player -> Color
playerToColor (Just PlayerX) = colorX
playerToColor (Just PlayerO) = colorO
playerToColor Nothing = colorGrid

drawWorld :: Game -> Picture
drawWorld (Game field state _) = Pictures $ case state of
    Running        -> [drawGrid colorGrid, drawFieldRunning field]
    EndGame player -> [drawGrid colorP   , drawFieldEnd field colorP] 
        where colorP = playerToColor player