module PacMan.Helper where

import System.Random
import Codec.BMP
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Constants

data Direction = North | East | South | West deriving (Show, Eq)
data Cell = GhostHouse | Empty | Wall | CoinCell | PowerUpCell deriving (Show, Eq)

getVector :: Direction -> Vector
getVector North = (0, -1)
getVector East  = (1,  0)
getVector South = (0,  1)
getVector West  = (-1, 0)

pointToScreen, cellToPoint, cellToScreen :: Point -> Point
pointToScreen (x, y) = (x - 270, 290 - y)
cellToPoint (x, y)   = (fromIntegral tileWidth * x, fromIntegral tileHeight * y)
cellToScreen         = pointToScreen . cellToPoint

pointToCell, screenToPoint, screenToCell :: Point -> Point
pointToCell (x, y)   = (x / fromIntegral tileWidth, y / fromIntegral tileHeight)
screenToPoint (x, y) = (x + 270, 290 - y)
screenToCell         = pointToCell . screenToPoint

fromIntegralVec2 :: (Int, Int) -> Point
fromIntegralVec2 (x, y) = (fromIntegral x, fromIntegral y)

roundVec2 :: Point -> (Int, Int)
roundVec2 (x, y) = (round x, round y)

size :: [[a]] -> (Int, Int)
size y@(x:_) = (length x, length y)
size y       = (0,        length y)

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East  = West
oppositeDirection South = North
oppositeDirection West  = East

spriteSection :: (Int, Int) -> BitmapData -> Picture
spriteSection (x, y) = bitmapSection $ Rectangle (1 + x * (tileWidth + 1), 1 + y * (tileHeight + 1)) (tileWidth, tileHeight)

getGridElement :: [[Cell]] -> (Int, Int) -> Cell
getGridElement ((h : _ ) : _)  (0, 0) = h
getGridElement ((_ : hs) : _)  (x, 0) = getGridElement [hs] (x - 1, 0)
getGridElement (_        : vs) (x, y) = getGridElement vs (x, y - 1)
getGridElement _               _      = Empty

setGridElement :: [[Cell]] -> (Int, Int) -> Cell -> [[Cell]]
setGridElement ((_ : hs) : vs) (0, 0) e = (e : hs) : vs
setGridElement ((h : hs) : vs) (x, 0) e = case setGridElement [hs] (x - 1, 0) e of
  (v':_) -> (h:v'):vs
  []     -> [h]:vs
setGridElement (v        : vs) (x, y) e = v : setGridElement vs (x, y - 1) e
setGridElement grid            _      _ = grid

-- made my loadfunction because I want to use BitmapData instead of Picture
loadBitmapData :: FilePath -> IO BitmapData
loadBitmapData filePath = do
  ebmp <- readBMP filePath
  case ebmp of
    Left err  -> error $ show err
    Right bmp -> return $ bitmapDataOfBMP bmp

-- shuffles list
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle stdGen []  = ([], stdGen)
shuffle stdGen [x] = ([x], stdGen)
shuffle stdGenA xs = (c : shuffle', stdGenC)
  where
    (index, stdGenB) = randomR (0, length xs - 1) stdGenA
    (l, c:r) = splitAt index xs
    (shuffle', stdGenC) = shuffle stdGenB (l ++ r)

readGameMap :: IO [[Cell]]
readGameMap = map (map replace) . lines <$> readFile "data/gameMap.txt"
  where
    replace :: Char -> Cell
    replace 'o' = CoinCell
    replace 'O' = PowerUpCell
    replace '#' = Wall
    replace '_' = GhostHouse
    replace ' ' = Empty
    replace _   = Empty -- unknown

ghostIsHome :: [[Cell]] -> Point -> Bool
ghostIsHome gameMap position = getGridElement gameMap (roundVec2 (pointToCell position)) == GhostHouse
