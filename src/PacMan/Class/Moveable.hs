{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Class.Moveable where

import Data.Fixed
import Data.List
import Data.Maybe
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import Graphics.Gloss.Data.Point
import PacMan.Helper
import PacMan.Model
import PacMan.Constants

class Moveable a where
  move :: State -> Float -> a -> a

-- general moving logic that can be used by both ghost and pacman
computeMove :: Point -> Direction -> Float -> Float -> [Direction] -> [[Cell]] -> [Cell] -> (Point, Direction, Float)
computeMove position direction speed dt rankedDirections gameMap moveableCells = (position', direction', elapsedPath)
  where
    position' :: Point
    position' = modPos (cellToPoint gridSize) $ position Pt.+
      (movementCurrentDirection Pt.* getVector direction) Pt.+
      (movementNextDirection Pt.* getVector direction')

    movement :: Float
    movement = speed * dt * fromIntegral tileWidth

    maxMovement :: Float
    maxMovement = case direction of
      North -> y
      East -> case x of
        0 -> 0
        _ -> width - x
      South -> case y of
        0 -> 0
        _ -> height - y
      West -> x
      where
        x, y, width, height :: Float
        width  = fromIntegral tileWidth
        height = fromIntegral tileHeight
        (x, y) = case position of (x', y') -> (x' `mod'` width, y' `mod'` height)

    nextDirection :: Maybe Direction
    nextDirection = find canMove rankedDirections

    movementCurrentDirection :: Float
    movementCurrentDirection = min movement maxMovement

    movementNextDirection :: Float
    movementNextDirection = case nextDirection of
      Just _  -> movement - movementCurrentDirection
      Nothing -> 0

    direction' :: Direction
    direction'
      | maxMovement - movement <= 0 = fromMaybe direction nextDirection
      | otherwise = direction

    gridSize :: Point
    gridSize = (fromIntegralVec2 . size) gameMap

    canMove :: Direction -> Bool
    canMove nextDirection' = getGridElement gameMap nextCell `elem` moveableCells
      where
        nextCell :: (Int, Int)
        nextCell = roundVec2 (modPos gridSize (pointToCell position Pt.+ getVector nextDirection'))

    elapsedPath :: Float
    elapsedPath = movementCurrentDirection + movementNextDirection

modPos :: Point -> Point -> Point
modPos gridSize@(gridWidth, gridHeight) position = case position Pt.+ gridSize of
  (x, y) -> (x `mod'` gridWidth, y `mod'` gridHeight)
