module PacMan.LevelProgress where

-- data type to store alternating of movement (scatter and chase mode) of ghosts
-- during the first level the movement of the ghosts progresses as the following
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then switch to Chase mode permanently.

-- Movement modes of Ghosts
data MovementMode = Scatter | Chase

data MovementModeProgress = StepMovement MovementMode Float MovementModeProgress | FinalMovement MovementMode

-- Describe the movement mode progress and power up duration for each level
data LevelProgress = StepLevel MovementModeProgress Float LevelProgress | FinalLevel MovementModeProgress Float

-- Level progress for Pac-Man
defaultLevelProgress :: LevelProgress
defaultLevelProgress = StepLevel
  (
    StepMovement Scatter 1 $ -- Scatter for 7 sec
    --StepMovement Chase 20$ -- Chase for 20 sec etc
    --StepMovement Scatter 7 $
    --StepMovement Chase 20 $
    --StepMovement Scatter 5 $
    --StepMovement Chase 20 $
    --StepMovement Scatter 5 $
    FinalMovement Chase -- Chase for the remainder of the level
  )
  10 -- power up duration of 10s for level 1
  $ flip (foldr id) (replicate 3 $ StepLevel ( -- repeat level settings 3 times
      StepMovement Scatter 7 $
      StepMovement Chase 20 $
      StepMovement Scatter 7 $
      StepMovement Chase 20 $
      StepMovement Scatter 5 $
      StepMovement Chase 1033 $ -- chase for 1033 seconds!
      StepMovement Scatter 1 $ -- scatter for 1 second
      FinalMovement Chase -- chase for the remainder of the level
    )
    5 -- power up duration of 5 sec for level 2 to 5
  )
  $ FinalLevel ( -- level settings for level 5 and up
    StepMovement Scatter 5 $
    StepMovement Chase 20 $
    StepMovement Scatter 5 $
    StepMovement Chase 20 $
    StepMovement Scatter 5 $
    StepMovement Chase 1037 $
    StepMovement Scatter 1 $
    FinalMovement Chase
  )
  3 -- power up for 3 seconds

defaultMovementModeProgress :: MovementModeProgress
defaultPowerUpDuration :: Float
(defaultMovementModeProgress, defaultPowerUpDuration) = case defaultLevelProgress of
  StepLevel movementMode powerUpDuration _ -> (movementMode, powerUpDuration)
  FinalLevel movementMode powerUpDuration  -> (movementMode, powerUpDuration)
