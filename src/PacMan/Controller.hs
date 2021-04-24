{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Controller where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import qualified PacMan.Game           as Game
import qualified PacMan.MainMenu       as MainMenu
import qualified PacMan.EnterHighscore as EnterHighscore
import qualified PacMan.Highscores     as Highscores
import qualified PacMan.MapEditor      as MapEditor

step :: Float -> State -> IO State
step dt gameState@Game {}           = Game.step           dt gameState
step dt gameState@MainMenu {}       = MainMenu.step       dt gameState
step dt gameState@EnterHighscore {} = EnterHighscore.step dt gameState
step dt gameState@Highscores {}     = Highscores.step     dt gameState
step dt gameState@MapEditor {}      = MapEditor.step      dt gameState

input :: Event -> State -> IO State
input event gameState@Game {}           = Game.input           event gameState
input event gameState@MainMenu {}       = MainMenu.input       event gameState
input event gameState@EnterHighscore {} = EnterHighscore.input event gameState
input event gameState@Highscores {}     = Highscores.input     event gameState
input event gameState@MapEditor {}      = MapEditor.input      event gameState
