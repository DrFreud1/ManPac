-- View logic, calls the view function depending on the game state

{-# LANGUAGE NamedFieldPuns #-}

module PacMan.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Model
import qualified PacMan.Game           as Game
import qualified PacMan.MainMenu       as MainMenu
import qualified PacMan.EnterHighscore as EnterHighscore
import qualified PacMan.Highscores     as Highscores
import qualified PacMan.MapEditor      as MapEditor

view :: BitmapData -> State -> IO Picture
view sprite gameState@Game {}           = return (Game.view           sprite gameState)
view sprite gameState@MainMenu {}       = return (MainMenu.view       sprite gameState)
view sprite gameState@EnterHighscore {} = return (EnterHighscore.view sprite gameState)
view sprite gameState@Highscores {}     = return (Highscores.view     sprite gameState)
view sprite gameState@MapEditor {}      = return (MapEditor.view      sprite gameState)
