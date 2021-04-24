{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module PacMan.GameObject.Coin where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable

instance Renderable Coin where
  render _ _ Coin { stateCoin = Eaten } = Blank
  render sprite Game { elapsedTime } coin = drawCoin sprite coin elapsedTime
  render sprite _                    coin = drawCoin sprite coin 0

drawCoin :: BitmapData -> Coin -> Float -> Picture
drawCoin sprite Coin { positionCoin, typeCoin } elapsedTime = uncurry translate (cellToScreen positionCoin) (spriteSection spritePosition sprite)
  where
    spritePosition :: (Int, Int)
    spritePosition = case typeCoin of
      Regular -> (8, 13)
      PowerUp -> case map (, 13) [0..7] of
        -- pick frame from animation bases on elapsedTime
        -- "!!" cannot fail because of mod length
        animation -> animation !! (round (elapsedTime * 5) `mod` length animation)

-- coin has no update functions
-- fall back on default implementation of key down and update
instance Updateable Coin where
  event _ _ a  = a
  update _ _ a = a
