{-# LANGUAGE NamedFieldPuns #-}

module PacMan.GameObject.Ghost where

import System.Random
import Data.List
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap()
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import Graphics.Gloss.Data.Vector
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.Class.Moveable
import PacMan.LevelProgress
import PacMan.Constants

-- Move to constants file
centerGhostHouse, entranceGhostHouse :: (Float, Float)
centerGhostHouse   = (13.5, 13)
entranceGhostHouse = (13.5, 10)

instance Renderable Ghost where
  render sprite Game { powerUpTimer } Ghost {
    directionGhost  = direction,
    positionGhost   = position,
    frightenedGhost = frightened,
    behaviourGhost  = behaviour
  } = uncurry translate (pointToScreen position) (spriteSection tilePosition sprite)
    where
      tilePosition = case (frightened, direction, behaviour) of
        (_,          East,  Pinky)  -> (4,8)
        (_,          South, Pinky)  -> (6,9)
        (_,          North, Pinky)  -> (7,9)
        (_,          West, Pinky)  -> (5,8)
        where

  render _ _ _ = Blank


instance Updateable Ghost where
  update Game { pacMan, score } _ ghost@Ghost { spawnMode = NotSpawned, behaviourGhost } = ghost { spawnMode = spawnMode' }
    where
      spawnMode' :: SpawnMode
      spawnMode' = case elapsedPath pacMan of
        0 -> NotSpawned
        _ -> case behaviourGhost of
          Pinky  -> if score >=0  then Spawned else NotSpawned
          -- _     -> Spawned
  update gameState dt ghost = move gameState dt ghost

  event _ _ a = a

instance Moveable Ghost where
  move Game {
    ghostMovementProgress,
    -- ghosts,
    pacMan = PacMan { positionPacMan},
    grid = GameMap { gameMap }
  } dt ghost@Ghost {
    frightenedGhost,
    speedGhost,
    positionGhost = position,
    directionGhost = direction,
    behaviourGhost,
    stdGen
  } = ghost {
    stdGen = stdGen',
    positionGhost = positionGhost',
    directionGhost = directionGhost'
  }
    where
      speed :: Float
      speed = case frightenedGhost of
        Frightened    -> speedGhost--frightenedGhostSpeed
        NotFrightened -> speedGhost
        Homing        -> homingGhostSpeed

      positionGhost' :: Point
      directionGhost' :: Direction
      (positionGhost', directionGhost', _) = computeMove
        position
        direction
        speed
        dt
        rankedDirections
        gameMap
        moveableCells

      moveableCells :: [Cell]
      moveableCells = case frightenedGhost of
        NotFrightened | getGridElement gameMap (roundVec2 (pointToCell position)) == GhostHouse
                      -> [CoinCell, PowerUpCell, Empty, GhostHouse]
        NotFrightened -> [CoinCell, PowerUpCell, Empty]
        _             -> [CoinCell, PowerUpCell, Empty, GhostHouse]

      rankedDirections :: [Direction]
      stdGen' :: StdGen
      (rankedDirections, stdGen') = case frightenedGhost of
        -- if ghost is frightend pick random position except for turning back
        Frightened -> shuffle stdGen (filter (/= oppositeDirection direction) [North, East, South, West])
        _          -> (sortBy sort' [North, East, South, West], stdGen)
        where
          sort' :: Direction -> Direction -> Ordering
          sort' a b
            -- Rank cells based on closest distance to Pac-Man
            -- Execept if that direction is the opposite of current direction
            -- This means that the ghost will always move
            -- to the cell next to it that is closest to Pac-Man and
            -- will only turn to the opposite direction if no other choice is possible
            | a == oppositeDirection direction = GT
            | b == oppositeDirection direction = LT
            | otherwise = compare (distanceToDirection a) (distanceToDirection b)

          distanceToDirection :: Direction -> Float
          distanceToDirection direction' = magV (pointToCell position Pt.+ getVector direction' Pt.- targetCell)

      targetCell :: Point
      targetCell = case frightenedGhost of
        -- if ghost is inside the home, go to the entrance tile to get out
        NotFrightened | ghostIsHome gameMap position -> entranceGhostHouse
        NotFrightened -> case movementMode of
          Scatter -> scatterModeTargetCell
          Chase -> case behaviourGhost of
            --Pinky -> pointToCell positionPacMan
            --Pinky  -> 3 Pt.* pointToCell positionPacMan Pt.+ getVector directionPacMan
            Pinky   | magV (pointToCell (position Pt.- positionPacMan)) < 8
                   -> scatterModeTargetCell
            Pinky  -> pointToCell positionPacMan
        _ -> centerGhostHouse
        where
          movementMode :: MovementMode
          movementMode = case ghostMovementProgress of
            StepMovement movementMode' _ _ -> movementMode'
            FinalMovement movementMode'    -> movementMode'

          scatterModeTargetCell :: Point
          scatterModeTargetCell = case behaviourGhost of
          
            Pinky  -> (width, 0)
       
            where
              width, height :: Float
              (width, height) = (fromIntegralVec2 . size) gameMap
  move _ _ ghost = ghost
