{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Game where

import System.Random
import Data.List
import Control.Monad
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper
import PacMan.LevelProgress
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.GameObject.Coin()
import PacMan.GameObject.GameMap()
import PacMan.GameObject.PacMan() --- Duplicar y editar ** OJOJ
import PacMan.GameObject.Ghost()

view :: BitmapData -> State -> Picture
view sprite gameState@Game { gameMode, lives, level, score, coins, pacMan, grid, ghosts = (pinky) } = pictures $
  render' grid :
  map render' coins ++
  [render' pacMan, render' pinky] ++
  pauseText: levelHeader : levelValue : scoreHeader : scoreValue : livesLeft -- UI elements
  where
    render' :: Renderable a => a -> Picture
    render' = render sprite gameState

    scoreHeader, scoreValue, levelHeader, levelValue :: Picture
    scoreHeader = drawText (-260) 340 "Score"
    scoreValue  = drawText (-260) 320 (show score)
    levelHeader = drawText (-10)  340 "Level"
    levelValue  = drawText (-10)  320 (show (level + 1))
    -- show different text based on
    pauseText   = drawText 120    340 (case gameMode of
      Playing -> "Press ESC to pause"
      Paused  -> "Press ESC to continue")

    drawText :: Float -> Float -> String -> Picture
    drawText x y = translate x y . scale 0.1 0.1 . color white . Text

    livesLeft :: [Picture]
    livesLeft = fmap (\x -> (translate (fromIntegral x * 40 - 260) (-330) . spriteSection (4, 9)) sprite) [0 .. (lives - 1)]
view _ _ = Blank

step :: Float -> State -> IO State
-- if gameState = Playing update every GameObject
step dt gameState@Game {
  gameMode = Playing,
  score,
  elapsedTime,
  levelProgress,
  lives,
  level,
  coins,
  pacMan,
  grid = grid@GameMap { gameMap },
  ghosts = (pinky)
}
  -- If all coins are eaten, advance to next level
  | all coinIsEaten coins = do
    stdGen <- newStdGen
    return gameState {
      levelProgress = nextLevelProgress,                -- update level progress
      ghostMovementProgress = nextMovementModeProgress, -- update movement mode progress
      powerUpDuration = nextPowerUpDuration,            -- update power up duration
      level = level + 1,                                -- increase level
      elapsedTime = 0,                                  -- reset elapsed time
      powerUpTimer = 0,                                 -- reset power up timer
      pacMan = defaultPacMan,                           -- reset Pac-Man
      ghosts = defaultGhosts stdGen,                    -- reset Ghosts
      coins = defaultCoins gameMap                      -- reset coins
    }
  -- Check if Pac-Man died
  |  die pinky = case lives - 1 of
    0      -> return (defaultEnterHighscore score)
    lives' -> do
      stdGen <- newStdGen
      return gameState {
        lives = lives',                                      -- decrease lives
        ghostMovementProgress = currentMovementModeProgress, -- reset movement progress
        powerUpTimer = 0,                                    -- reset power up timer
        elapsedTime = 0,                                     -- reset elapsed time
        ghosts = defaultGhosts stdGen,                       -- reset ghosts
        pacMan = defaultPacMan                               -- reset Pac-Man
      }
  -- Update game
  | otherwise = return $
    updateGhostMovementProgress dt $
    updateCoins dt $
    updateGhosts dt $
    updatePowerUpTimer dt $
    gameState {
      elapsedTime = elapsedTime + dt,                                       -- increase elapsedTime
      coins = fmap update' coins,
      pacMan = update' pacMan,
      ghosts = (update' pinky),
      grid = update' grid
    }
    where
      update' :: Updateable a => a -> a
      update' = update gameState dt

      coinIsEaten :: Coin -> Bool
      coinIsEaten Coin { stateCoin = Eaten } = True
      coinIsEaten _                          = False

      die :: Ghost -> Bool
      die Ghost {
        frightenedGhost = NotFrightened,
        positionGhost
      } = (roundVec2 . pointToCell . positionPacMan) pacMan == roundVec2 (pointToCell positionGhost)
      die _ = False

      currentMovementModeProgress :: MovementModeProgress
      currentMovementModeProgress = case levelProgress of
        StepLevel movementMode _ _ -> movementMode
        FinalLevel movementMode _  -> movementMode

      nextLevelProgress :: LevelProgress
      nextMovementModeProgress :: MovementModeProgress
      nextPowerUpDuration :: Float
      (nextLevelProgress, nextMovementModeProgress, nextPowerUpDuration) = case levelProgress of
        levelProgress'@(StepLevel movementMode powerUpDuration _) -> (levelProgress', movementMode, powerUpDuration)
        levelProgress'@(FinalLevel movementMode powerUpDuration)  -> (levelProgress', movementMode, powerUpDuration)

step _ gameState = return gameState

updateCoins2 :: (Floating a, Ord a) => [a] -> [a] -> [a] -> [a]
updateCoins2 la lb lc = do
  a <-  la
  b <-  lb
  c <-  lc

  if (a == 0)
    then mzero
    else do
      direction <- pure (b * b - 4 * a * c) -- >>= \d ->
      distance <- pure (2 * a) -- >>= \de ->
      location <- pure ((-b) / distance) -- >>= \su ->
      coord <- pure(sqrt direction / distance)

      case (signum direction) of
        (-1)  -> mzero
      --   0 -> return loc
      --   1 -> return (loc + co) `mplus` return (co - loc)

updateCoins :: Float -> State -> State
updateCoins _ gameState@Game {
  powerUpTimer,
  coins,
  score,
  powerUpDuration,
  ghosts = (pinky)
} = case partition isEaten2 coins of
  (left, right) -> case any isPowerUp left of
    atePowerUp -> gameState {
      score = score + countScore left,
      coins = fmap (\coin -> coin { stateCoin = Eaten }) left ++ right,
      powerUpTimer = if atePowerUp
        then powerUpDuration
        else powerUpTimer,
      ghosts = if atePowerUp
        then (frightenGhost pinky)
        else (frightenGhost pinky)
    }
  where

    isEaten2 :: Coin -> Bool
    isEaten2 Coin { stateCoin = Eaten } = False
    isEaten2 coin = (roundVec2 . pointToCell . positionGhost) pinky == (roundVec2 . positionCoin) coin

    isPowerUp :: Coin -> Bool
    isPowerUp Coin { typeCoin = PowerUp } = True
    isPowerUp _ = False

    frightenGhost :: Ghost -> Ghost
    frightenGhost ghost@Ghost { frightenedGhost = Frightened } = ghost {
      frightenedGhost = Frightened,
      directionGhost = oppositeDirection $ directionGhost ghost
    }
    frightenGhost ghost = ghost

    countScore :: [Coin] -> Int
    countScore [] = 0
    countScore (Coin { typeCoin = PowerUp } : xs) = 50 + countScore xs
    countScore (Coin { typeCoin = Regular } : xs) = 10 + countScore xs
updateCoins _ gameState = gameState

updateGhostMovementProgress :: Float -> State -> State
updateGhostMovementProgress _  gameState@Game { ghostMovementProgress = (FinalMovement _) } = gameState
updateGhostMovementProgress dt gameState@Game { ghostMovementProgress = (StepMovement mode time nextStep) } = gameState {
  ghostMovementProgress = if newTime < 0
    then nextStep
    else StepMovement mode newTime nextStep
}
  where
    newTime :: Float
    newTime = time - dt
updateGhostMovementProgress _  gameState = gameState

updateGhosts :: Float -> State -> State
updateGhosts _ gameState@Game {
  pacMan = PacMan { positionPacMan },
  grid = GameMap { gameMap },
  ghosts = (pinky)
} = gameState {
  ghosts = (updateGhost pinky)
}
  where
    updateGhost :: Ghost -> Ghost
    updateGhost ghost@Ghost {
      positionGhost = position,
      frightenedGhost
    } = ghost {
      frightenedGhost = case frightenedGhost of
        Homing | ghostIsHome gameMap position -> NotFrightened
        -- if ghost is frightened and hit Pac-Man, go back to home to respawn
        Frightened | roundVec2 (pointToCell positionPacMan) == roundVec2 (pointToCell position) -> Homing
        _ -> frightenedGhost
    }
updateGhosts _ gameState = gameState

updatePowerUpTimer :: Float -> State -> State
updatePowerUpTimer _ gameState@Game { powerUpTimer = 0, ghosts = (pinky) } = gameState {
  ghosts = (unFrighten pinky)
}
  where
    unFrighten :: Ghost -> Ghost
    unFrighten ghost@Ghost { frightenedGhost = Homing }     = ghost { frightenedGhost = NotFrightened }
    unFrighten ghost@Ghost { frightenedGhost = Frightened } = ghost { frightenedGhost = NotFrightened }
    unFrighten ghost = ghost
updatePowerUpTimer dt gameState@Game { powerUpTimer } = gameState {
  -- count down powerup timer
  powerUpTimer = max 0 (powerUpTimer - dt)
}

updatePowerUpTimer _ gameState = gameState

input :: Event -> State -> IO State
-- play pause logic
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@Game { gameMode = Playing } = return gameState { gameMode = Paused }
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@Game { gameMode = Paused } = return gameState { gameMode = Playing }

-- emit special keys to all game objects
input key gameState@Game {
  gameMode = Playing,
  coins,
  pacMan,
  ghosts = (pinky),
  grid
} = return gameState {
  coins = fmap event' coins,
  pacMan = event' pacMan,
  ghosts = (event' pinky),
  grid = event' grid
}
  where
    event' :: Updateable a => a -> a
    event' = event gameState key
input _ gameState = return gameState
