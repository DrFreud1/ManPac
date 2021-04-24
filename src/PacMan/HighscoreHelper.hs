{-# LANGUAGE OverloadedStrings #-}

module PacMan.HighscoreHelper where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Aeson
import Data.List

data Score = Score {
  playerName :: String,
  playerScore :: Int
} deriving (Eq)

-- Eq and Ord instances of score
instance Ord Score where
  -- order is reverse because the highest scores are ranked higher
  (Score _ a) `compare` (Score _ b) = b `compare` a
  (Score _ a) < (Score _ b)         = b < a
  (Score _ a) <= (Score _ b)        = b <= a
  (Score _ a) > (Score _ b)         = b > a
  (Score _ a) >= (Score _ b)        = b >= a

-- Read and write scores with JSON format
instance FromJSON Score where
  parseJSON = withObject "Score" (\v -> Score <$> v .: "name" <*> v .: "score")

instance ToJSON Score where
  toJSON (Score name score)     = object ["name" .= name, "score" .= score]
  toEncoding (Score name score) = pairs ("name" .= name <> "score" .= score)

readHighscores :: IO [Score]
readHighscores = do
 jsonData <- readFile "data/highscores.json"
 case decode (pack jsonData) of
   (Just highscores') -> return highscores'
   _                  -> error "could not read highscores highscores"

writeHighscore :: [Score] -> IO ()
writeHighscore = writeFile "data/highscores.json" . unpack . encode

-- add score to a list of scores
-- returns sorted highscores and the index of the new score
addScore :: Score -> [Score] -> (Int, [Score])
addScore score highscores = (index, highscores')
  where
    highscores' :: [Score]
    highscores' = sort (score : highscores)

    index :: Int
    index = case elemIndex score highscores' of
      Just index' -> index'
      -- cannot fail because score is added to highscores'
      _           -> error "cannot find index of score"
