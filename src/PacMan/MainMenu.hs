{-# LANGUAGE NamedFieldPuns #-}

module PacMan.MainMenu where

import System.Random
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.HighscoreHelper
import PacMan.Helper

view :: BitmapData -> State -> Picture
view _ MainMenu { selected } = pictures $
  (translate (-130) 0      .                 colorStartGame  . Text) "Start" :
  (translate (-100) (-100) . scale 0.3 0.3 . colorHighscores . Text) "Highscores" :
  (translate (-180) (-200) . scale 0.3 0.3 . colorMapEditor  . Text) "Create Custom Map" :
  [Blank]
  where
    colorStartGame, colorHighscores, colorMapEditor :: Picture -> Picture
    (colorStartGame, colorHighscores, colorMapEditor) = case selected of
      MainMenuStart      -> (color cyan, color white, color white)
      MainMenuHighscores -> (color white, color cyan, color white)
      MainMenuMapEditor  -> (color white, color white, color cyan)
view _ _ = Blank

step :: Float -> State -> IO State
step _ = return

input :: Event -> State -> IO State
input (EventKey (SpecialKey KeyEnter) Down _ _) MainMenu { selected = MainMenuStart }                = defaultGame <$> newStdGen <*> readGameMap
input (EventKey (SpecialKey KeyEnter) Down _ _) MainMenu { selected = MainMenuHighscores }           = defaultHighscore Nothing <$> readHighscores
input (EventKey (SpecialKey KeyEnter) Down _ _) MainMenu { selected = MainMenuMapEditor }            = defaultMapEditor <$> readGameMap
input (EventKey (SpecialKey KeyDown)  Down _ _) gameState@MainMenu { selected = MainMenuStart }      = return gameState { selected = MainMenuHighscores }
input (EventKey (SpecialKey KeyDown)  Down _ _) gameState@MainMenu { selected = MainMenuHighscores } = return gameState { selected = MainMenuMapEditor }
input (EventKey (SpecialKey KeyDown)  Down _ _) gameState@MainMenu { selected = MainMenuMapEditor }  = return gameState { selected = MainMenuStart }
input (EventKey (SpecialKey KeyUp)    Down _ _) gameState@MainMenu { selected = MainMenuStart }      = return gameState { selected = MainMenuMapEditor }
input (EventKey (SpecialKey KeyUp)    Down _ _) gameState@MainMenu { selected = MainMenuHighscores } = return gameState { selected = MainMenuStart }
input (EventKey (SpecialKey KeyUp)    Down _ _) gameState@MainMenu { selected = MainMenuMapEditor }  = return gameState { selected = MainMenuHighscores }
input _ gameState = return gameState
