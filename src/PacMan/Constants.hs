-- Constants to be used by other modules

module PacMan.Constants where

import Graphics.Gloss.Data.Point

pacManStartPosition, pinkyStartPosition :: Point
pacManSpeed, pinkySpeed, frightenedGhostSpeed, homingGhostSpeed :: Float
pacManStartPosition = (13.5, 22)
pacManSpeed         = 10
pinkyStartPosition  = (13.5, 13)
pinkySpeed          = 8
frightenedGhostSpeed = 1
homingGhostSpeed     = 20

maxLives :: Int
maxLives = 1

tileWidth, tileHeight :: Int
tileWidth = 20
tileHeight = 20
