{-# LANGUAGE NamedFieldPuns #-}

module PacMan.GameObject.GameMap where

import Graphics.Gloss.Data.Picture
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable

instance Renderable GameMap where
  render sprite _ GameMap { gameMap } = (pictures . zipWith (uncurry translate) coords) connectWalls
    where
      coords :: [Point]
      coords = [(cellToScreen . fromIntegralVec2) (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

      connectWalls :: [Picture]
      connectWalls = (loopY . pad) gameMap

      loopY :: [[Cell]] -> [Picture]
      loopY ((_ : t) : c : bs@(_ : b) : ys) = loopX t c b ++ loopY (c : bs : ys)
      loopY _                               = []

      loopX :: [Cell] -> [Cell] -> [Cell] -> [Picture]
      loopX (t : ts) (l : c : r : xs) (b : bs) = connectWall t l c r b : loopX ts (c : r : xs) bs
      loopX _        _                _        = []

      connectWall :: Cell -> Cell -> Cell -> Cell -> Cell -> Picture
      connectWall Wall Wall Wall Wall Wall = spriteSection (7, 2)  sprite
      connectWall _    Wall Wall Wall Wall = spriteSection (7, 3)  sprite
      connectWall Wall _    Wall Wall Wall = spriteSection (6, 2)  sprite
      connectWall Wall Wall Wall _    Wall = spriteSection (8, 2)  sprite
      connectWall Wall Wall Wall Wall _    = spriteSection (7, 1)  sprite
      connectWall _    _    Wall Wall Wall = spriteSection (6, 3)  sprite
      connectWall _    Wall Wall _    Wall = spriteSection (8, 3)  sprite
      connectWall Wall _    Wall Wall _    = spriteSection (6, 1)  sprite
      connectWall Wall Wall Wall _    _    = spriteSection (8, 1)  sprite
      connectWall _    Wall Wall Wall _    = spriteSection (7, 0)  sprite
      connectWall Wall _    Wall _    Wall = spriteSection (6, 0)  sprite
      connectWall _    _    Wall _    Wall = spriteSection (10, 1) sprite
      connectWall _    _    Wall Wall _    = spriteSection (9, 0)  sprite
      connectWall _    Wall Wall _    _    = spriteSection (10, 0) sprite
      connectWall Wall _    Wall _    _    = spriteSection (9, 1)  sprite
      connectWall _    _    Wall _    _    = spriteSection (8, 0)  sprite
      connectWall _    _    _    _    _    = Blank

      pad :: [[Cell]] -> [[Cell]]
      pad gameMap' = addToStartAndEnd (replicate (width + 2) Empty) (map (addToStartAndEnd Empty) gameMap')

      width, height :: Int
      (width, height) = size gameMap

-- coin has no update functions
-- fall back on default implementation of key down and update
instance Updateable GameMap where
  event _ _  a = a
  update _ _ a = a


-- helper functions
addToStartAndEnd :: a -> [a] -> [a]
addToStartAndEnd a list = a : list ++ [a]
