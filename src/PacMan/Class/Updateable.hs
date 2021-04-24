module PacMan.Class.Updateable where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Model

-- class Updateable that implements update and event
class Updateable a where
  -- update is called every frame, with the gamestate, delta time and the gameobject
  update :: State -> Float -> a -> a
  -- update is called with every event
  event :: State -> Event -> a -> a
