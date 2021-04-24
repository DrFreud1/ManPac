-- Screen that lets users generate custom levels
-- The regular game map is shown, clickin on a cell changes said cell

{-# LANGUAGE NamedFieldPuns #-}

module PacMan.MapEditor where

import System.Random
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.GameObject.GameMap()
import PacMan.GameObject.Coin()

view :: BitmapData -> State -> Picture
view sprite gameState@MapEditor { grid = grid@GameMap { gameMap }, coins } = pictures $
  render' grid :
  (translate (-260) 320    . scale 0.2 0.2 . color white . Text) "Click on any cell to change it" :
  (translate (-260) (-340) . scale 0.2 0.2 . color white . Text) "Press RETURN to play" :
  (translate 110    (-340) . scale 0.1 0.1 . color white . Text) "Press ESC to go back" :
  map render' coins ++
  map (\x -> drawLine (fromIntegral x, 0) (fromIntegral x, fromIntegral height)) [0 .. width] ++
  map (\y -> drawLine (0, fromIntegral y) (fromIntegral width, fromIntegral y)) [0 .. height]
  where
    render' :: Renderable a => a -> Picture
    render' = render sprite gameState

    width :: Int
    height :: Int
    (width, height) = size gameMap

    drawLine :: Point -> Point -> Picture
    drawLine (sx, sy) (ex, ey) = (color (greyN 0.4) . line) [cellToScreen (sx - 0.5, sy - 0.5), cellToScreen (ex - 0.5, ey - 0.5)]
view _ _ = Blank

step :: Float -> State -> IO State
step _ = return

input :: Event -> State -> IO State
input (EventKey (MouseButton LeftButton) Down _ mousePosition) gameState@MapEditor {
  grid = GameMap { gameMap }
} = return gameState {
  grid = GameMap { gameMap = gameMap' },
  coins = defaultCoins gameMap'
}
  where
    cellPosition :: (Int, Int)
    cellPosition = (roundVec2 . screenToCell) mousePosition

    replaceCell :: Cell
    replaceCell = case getGridElement gameMap cellPosition of
      CoinCell    -> Wall
      Wall        -> Empty
      Empty       -> PowerUpCell
      PowerUpCell -> CoinCell
      GhostHouse  -> GhostHouse -- cannot replace ghost house

    gameMap' :: [[Cell]]
    gameMap' = setGridElement gameMap cellPosition replaceCell
input (EventKey (SpecialKey KeyEsc) Down _ _) _ = return defaultMainMenu
input (EventKey (SpecialKey KeyEnter) Down _ _) MapEditor { grid = GameMap { gameMap } } = defaultGame <$> newStdGen <*> return gameMap
input _ gameState = return gameState
