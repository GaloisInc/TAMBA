import Text.Printf (printf)
import System.Process (readProcess)
import Control.Monad (forM_)
import Criterion.Measurement (getCPUTime)
import Data.List (isPrefixOf)
import Statistics.Distribution.Beta
import Statistics.Distribution (quantile)
import Control.Parallel
import Control.DeepSeq
import Debug.Trace

time :: IO String -> IO  (String, Double)
time action = do
  t0 <- getCPUTime
  x <- action
  rnf x `pseq` return ()
  t1 <- getCPUTime
  return (x, t1 - t0)

policies :: [String]
policies = 
  [ 
  --  "bday_large.pol"  
  --, "pizza.pol"
  --, "bday.pol"        
  --, "pizza_small.pol"
  --, "close.pol"       
  "support.pol"
  --, "closer.pol"      
  --, "synth.pol"
  --, "photo.pol"       
  --, "travel.pol"
  ]


precisions :: [Int]
precisions = 0:[2..10]

samples :: [Int]
samples = 0 : map (10^) [1..5]


-- Parsing is currently baked into the command line, using unix tricks.
-- VERY brittle.
makeCommand pol prec samp = 
  printf "/vagrant/Code/prob/prob /vagrant/Code/prob/examples/bench/%s --domain box --precision %d --samples %d" pol prec samp

data Result = Result
  { size    :: Double
  , smin    :: Integer
  , smax    :: Integer
  , trues   :: Integer
  , falses  :: Integer
  }

bounds :: Result -> (Double, Double)
bounds result = (lo, hi)
  where
  a = fromIntegral $ 1 + trues result
  b = fromIntegral $ 1 + falses result
  lo = max (fromIntegral $ smin result) (size result * betaLo a b)
  hi = min (fromIntegral $ smax result) (size result * betaHi a b)

eps = 0.001

betaLo a b = quantile d eps
  where
  d = betaDistr a b

betaHi a b = quantile d (1 - eps)
  where
  d = betaDistr a b

makeResult :: String -> Result
makeResult s = Result (read a) (read b) (read c) (read d) (read e)
  where
  vals = map ((!! 2) . words) . dropWhile (not . isPrefixOf "size_z") $ lines s
  [a,b,c,d,e] = vals

-- byFile = do
--   putStr "policy"
--   forM_ precisions $ \pr -> do
--     putStr ", "
--     putStr $ printf "precision=%s" pr
--   putStr "\n"
--   forM_ policies $ \pol -> do
--     putStr pol
--     forM_ precisions $ \pr -> do
--       putStr ", "
--       result <- readProcess "bash" ["-c", makeCommand pol pr n] ""
--       putStr $ takeWhile (/= '\n') result
--     putChar '\n'

byPrec = do
  putStrLn "precision, samples, time, lo, hi"
  forM_ precisions $ \pr -> do
    forM_ samples $ \n -> do
      (output, t) <- time $ readProcess "bash" ["-c", makeCommand "support.pol" pr n] ""
      --putStrLn output
      let result = makeResult output
      let (lo, hi) = bounds result
      printf "%d, %d, %.2f, %.1f, %.1f\n" pr n t lo hi

main = byPrec
