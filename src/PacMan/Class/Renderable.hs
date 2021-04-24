module PacMan.Class.Renderable where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import PacMan.Model

class Renderable a where
  render :: BitmapData -> State -> a -> Picture
