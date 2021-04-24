module Main where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Controller
import PacMan.Model
import PacMan.View
import PacMan.Constants
import PacMan.Helper

main :: IO ()
main = do
  sprite <- loadBitmapData "data/sprite.bmp"
  playIO
    (InWindow "Pac-Man" (28 * tileWidth, 36 * tileHeight) (0, 0))
    black           -- background color
    60              -- fps
    defaultMainMenu -- initial state (model)
    (view sprite)   -- render state, already pass sprite (view)
    input           -- event handler (controller)
    step            -- update handler for each frame (controller)
