import Text.Printf (printf)
import System.Process (readProcess)
import Control.Monad (forM_)
import Criterion.Measurement (getCPUTime)
import Data.List (isPrefixOf)
import Statistics.Distribution.Beta
import Statistics.Distribution (quantile)
import Debug.Trace

time :: IO a -> IO  (a, Double)
time action = do
  t0 <- getCPUTime
  x <- action
  t1 <- getCPUTime
  return (x, t1 - t0)

policies :: [String]
policies = do
  s <- [1..5]::[Int]
  return $ printf "demo-%ds-%dq.prob" s s

precisions :: [Int]
precisions = [0]

samples :: [Int]
samples = 0 : map (10^) [1..5]


-- Parsing is currently baked into the command line, using unix tricks.
-- VERY brittle.
makeCommand pol prec = 
  printf "/vagrant/Code/prob/prob /vagrant/Code/prob/examples/bench/%s --domain box --precision %d | tail -n 5 | cut -f 3 -d \" \"" pol prec


data Result = Result
  { size    :: Double
  , smin    :: Int
  , smax    :: Int
  , trues   :: Int
  , falses  :: Int
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

traceString :: String -> String 
traceString s = trace s s

traceStrings s = trace (unlines s) s


makeResult :: String -> Result
makeResult s = Result (read a) (read b) (read c) (read d) (read e)
  where
  [a,b,c,d,e] = lines s

byFile = do
  putStr "policy"
  forM_ precisions $ \pr -> do
    putStr ", "
    putStr $ printf "precision=%s" pr
  putStr "\n"
  forM_ policies $ \pol -> do
    putStr pol
    forM_ precisions $ \pr -> do
      putStr ", "
      result <- readProcess "bash" ["-c", makeCommand pol pr] ""
      putStr $ takeWhile (/= '\n') result
    putChar '\n'

-- byPrec = do
--   putStrLn "precision, samples, time, smin, smax"
--   forM_ precisions $ \pr -> do
--     forM_ samples $ \n -> do
--       (output, t) <- time $ readProcess "bash" ["-c", makeCommand "support.pol" pr] ""
--       putStrLn output
--       let result = makeResult output
--       printf "%d, %d, %.2f, %d, %d" pr n t (smin result) (smax result)

main = byFile