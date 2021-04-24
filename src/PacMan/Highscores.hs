{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Highscores where

import Data.List
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.HighscoreHelper

totalScores :: Int
totalScores = 10

view :: BitmapData -> State -> Picture
view _ Highscores { highscores, selectedHighscore } = pictures $
  (translate (-150) 300    . scale 0.3 0.3 . color white . Text) "Highscores" :
  (translate (-250) (-300) . scale 0.3 0.3 . color white . Text) "Press ENTER to continue" :
  zipWith4 (\a b c d -> (a . b . c) d) colors (map (translate (-260)) ys) (repeat (scale 0.2 0.2)) rankings ++
  zipWith4 (\a b c d -> (a . b . c) d) colors (map (translate (-200)) ys) (repeat (scale 0.2 0.2)) names ++
  zipWith4 (\a b c d -> (a . b . c) d) colors (map (translate 170) ys)    (repeat (scale 0.2 0.2)) scores
  where
    indexes :: [Int]
    indexes
      | length highscores <= totalScores = [0 .. length highscores - 1]
      | otherwise               = case selectedHighscore of
        Just index | index > 9 -> [0 .. totalScores - 2] ++ [index]
        _                      -> [0 .. totalScores - 1]

    rankings, names, scores :: [Picture]
    rankings = map (Text . show . (+ 1)) indexes
    (names, scores) = unzip $ map (\index -> case highscores !! index of
      Score name score -> (Text name, (Text . show) score)) indexes

    colors = case selectedHighscore of
      Just selectedHighscore' -> map (\index -> if index == selectedHighscore' then color cyan else color white) indexes
      _                       -> repeat (color white)

    ys :: [Float]
    ys = map ((200 +) . (*) (-40)) [0 .. 9]
view _ _ = Blank

step :: Float -> State -> IO State
step _ = return

input :: Event -> State -> IO State
input (EventKey (SpecialKey KeyEnter) Down _ _) _ = return defaultMainMenu
input _ state = return state
