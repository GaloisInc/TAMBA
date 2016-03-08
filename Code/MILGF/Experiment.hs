module Experiment where

import GainFunctions
import Utils
import Matricize
import QueryCore
import Dist
import Data.List (genericLength, intersperse, zipWith5)
import System.Random

data Result = R { priorVuln  :: Probability
                , postTrad   :: Probability
                , postMax    :: Probability
                , leakage    :: Entropy
                , leakageMax :: Entropy
                }
  deriving (Show)

data Result2 = R2 { randDistRes   :: [Result]
                  , pieceWieseRes :: [Result]
                  }
  deriving (Show)

-- TODO: Make this more robust (i.e. don't rely on hardcoded 10000)
randomDist :: X -> IO Prior
randomDist is = do
  probs <- randDivideInto 10000 n
  return $ fromList $ zip is $ map (\n -> fromIntegral n / 10000) probs
  where
    n = genericLength is

randomPieceWise :: X -> IO Prior
randomPieceWise is = do
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

experiment :: Int -> [Statement] -> IO [Result2]
experiment n qs = do
    let channels   = map (flip chanelize (0,100)) qs
    let (x,_,_)    = head channels
    rndPriors <- sequence $ take n $ repeat $ randomDist x
    piePriors <- sequence $ take n $ repeat $ randomPieceWise x
    let fs = map runMetrics channels
    let rndRes = mapAll fs rndPriors
    let pieRes = mapAll fs piePriors
    return $ zipWith R2 rndRes pieRes

runMetrics :: Channel -> Prior -> Result
runMetrics chan pi = R a b c d e
  where
    a = vuln pi chan
    b = pVuln pi chan
    c = pVulnMax pi chan
    d = minLeakage pi chan
    e = minLeakageMax pi chan

pprResult :: String -> Result -> String
pprResult qName (R a b c d e) = qName
                                ++ "," ++ show a
                                ++ "," ++ show b
                                ++ "," ++ show c
                                ++ "," ++ show d
                                ++ "," ++ show e

pprExperiment :: String -> Result2 -> String
pprExperiment qName (R2 rs ps)
                           = headerRPriors ++ "\n" ++
                             unlines (map (pprResult qName) rs) ++ "\n" ++

                             headerPWPriors ++ "\n" ++
                             unlines (map (pprResult qName) ps) ++ "\n"

headerRPriors :: String
headerRPriors = concat $ intersperse "," $
                      map show [ "query name"
                               , "prior Vuln for rand prior"
                               , "Traditional post-vuln for random Prior"
                               , "Max-based post-vuln for random Prior"
                               , "Leakage using traditional pvuln for rand prior"
                               , "Leakage using max-based pvuln for rand prior"
                               ]

headerPWPriors :: String
headerPWPriors = concat $ intersperse "," $
                      map show [ "query name"
                               , "prior Vuln for piece-wise random prior"
                               , "Traditional post-vuln for piece-wise rand Prior"
                               , "Max-based post-vuln for piece-wise rand Prior"
                               , "Leakage using trad. pvuln for p-wise rand prior"
                               , "Leakage using max-based pvuln for p-wise prior"
                               ]
{- 
    let randRes = zipWith5 R
              (zipRes vuln rndPriors)     -- Prior vulnerability with random priors
              (zipRes pVuln rndPriors)    -- Posterior vuln with rand-prior
              (zipRes pVulnMax rndPriors) -- Same as ^ but with max post-vuln
              (zipRes minLeakage rndPriors)
              (zipRes minLeakageMax rndPriors)
    let pwRes = zipWith5 R
              (zipRes vuln piePriors)     -- Prior vulnerability with piece-wise uni
              (zipRes pVuln piePriors)    -- Posterior vuln with piece-wise priors
              (zipRes pVulnMax piePriors) -- Same as ^ but with max post-vuln
              (zipRes minLeakage piePriors)
              (zipRes minLeakageMax piePriors)
    return $ R2 randRes pwRes
-}
