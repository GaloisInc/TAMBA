module Experiment where

import GainFunctions
import Utils
import Matricize
import QueryCore
import Dist
import Data.List (genericLength)
import System.Random

-- TODO: Make this more robust (i.e. don't rely on hardcoded 10000)
randomDist :: Channel -> IO Prior
randomDist (is, _, _) = do
  probs <- randDivideInto 10000 n
  return $ fromList $ zip is $ map (\n -> fromIntegral n / 10000) probs
  where
    n = genericLength is

randomPieceWise :: Channel -> IO Prior
randomPieceWise (is, _, _) = do
  bckts <- randomRIO (1,n)              -- Random number of buckets
  bWidths <- randDivideInto n bckts     -- The width of each bucket (can be 0)
  probs1 <- randDivideInto 10000 bckts
  let probs = map (\n -> fromIntegral n / 10000) probs1
  return $ fromList $ zip is $ mkPieces bWidths probs
  where
    n = genericLength is

-- This function takes the widths of each bucket, and the probability of each
-- bucket and makes the assoc list representing the Prior
mkPieces :: [Int] -> [Double] -> [Double]
mkPieces ws ps = concat $ zipWith f ws ps
  where
    f w p = take w $ repeat (p / fromIntegral w)
