{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module PacMan.GameObject.PacMan where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model hiding (pacMan)
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.Class.Moveable

instance Renderable PacMan where
  render sprite Game { powerUpTimer } PacMan { directionPacMan, elapsedPath, positionPacMan } = uncurry translate (pointToScreen positionPacMan) (section sprite)
    where
      section :: BitmapData -> Picture
      section = case map (, y) xs of
        -- pick frame from animation based on elapesedPath
        -- "!!" cannot fail because of mod length
        animation -> spriteSection (animation !! (round (elapsedPath / 10) `mod` length animation))
        where
          y = case directionPacMan of
            North -> 12
            East  -> 12
            South -> 12
            West  -> 12
          xs = case powerUpTimer of
            0 -> [4, 5, 6, 7, 6, 5]
            _ -> [0, 1, 2, 3, 2, 1]
  render _ _ _ = Blank

instance Updateable PacMan where
  -- only move Pac-Man on update
  update = move

  event _ (EventKey (SpecialKey key) Down _ _) pacMan = case getDirection of
    -- Pac-Man can always move backwards
    Just nextDirection | oppositeDirection nextDirection == directionPacMan pacMan -> pacMan {
      nextDirectionPacMan = nextDirection,
      directionPacMan = nextDirection
    }
    -- set target direction of Pac-Man
    Just nextDirection -> pacMan {
      nextDirectionPacMan = nextDirection
    }
    -- if key is not up, right, down or left do nothing
    Nothing -> pacMan
    where
      getDirection :: Maybe Direction
      getDirection = case key of
        KeyUp    -> Just North
        KeyRight -> Just East
        KeyDown  -> Just South
        KeyLeft  -> Just West
        _        -> Nothing
  event _ _ pacMan = pacMan

instance Moveable PacMan where
  move Game { grid = GameMap { gameMap } } dt pacMan@PacMan { nextDirectionPacMan, speedPacMan, positionPacMan, directionPacMan, elapsedPath } = pacMan {
    positionPacMan = positionPacMan',
    directionPacMan = directionPacMan',
    elapsedPath = elapsedPath + elapsedPath'
  }
    where
      positionPacMan' :: Point
      directionPacMan' :: Direction
      elapsedPath' :: Float
      (positionPacMan', directionPacMan', elapsedPath') = computeMove
        positionPacMan
        directionPacMan
        speedPacMan
        dt
        rankedDirections
        gameMap
        [CoinCell, PowerUpCell, Empty]

      rankedDirections :: [Direction]
      rankedDirections = [nextDirectionPacMan, directionPacMan]
  move _ _ ghost = ghost
