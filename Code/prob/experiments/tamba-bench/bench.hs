import Text.Printf (printf)
import System.Process (readProcess)
import Control.Monad (forM_)
import System.Clock
import System.Environment
import System.IO
import Data.List (isPrefixOf)
import Statistics.Distribution.Beta
import Statistics.Distribution (quantile)
import Control.Parallel
import Control.DeepSeq
import Debug.Trace

{-
time :: IO String -> IO  (String, Double)
time action = do
  t0 <- getCPUTime
  x <- action
  rnf x `pseq` return ()
  t1 <- getCPUTime
  let t = t1 - t0
  t `pseq` return (x, t)
-}


leave :: Int -> [a] -> [a]
leave n xs = leave' xs (drop n xs)
  where
    leave' xs     []     = xs
    leave' (_:xs) (y:ys) = leave' xs ys

precisions :: [Int]
precisions = [1..5]

samples :: [Int]
samples = 0 : 0 : map (10^) [1..7]


-- Parsing is currently baked into the command line, using unix tricks.
-- VERY brittle.
makeCommand pol prec samp = 
  printf "/vagrant/Code/prob/prob %s --domain box --precision %d --samples %d" pol prec samp

data Result = Result
  { size    :: Double
  , pmin    :: Double
  , pmax    :: Double
  , smin    :: Integer
  , smax    :: Integer
  , mmin    :: Double
  , mmax    :: Double
  , trues   :: Integer
  , falses  :: Integer
  }

bounds :: Result -> Int -> (Double, Double)
bounds result 0 = (lo, hi)
  where
  lo = fromIntegral $ smin result
  hi = fromIntegral $ smax result
bounds result _ = (lo, hi)
  where
  a = fromIntegral $ 1 + trues result
  b = fromIntegral $ 1 + falses result
  -- Compute the updated bounds, subject to some constraints:
  -- the new lower bound should be >= the deterministic lower bound
  -- the new lower bound should be <= the deterministic upper bound
  -- ... and similar for the upper bound
  lo = min (fromIntegral $ smax result) $ max (fromIntegral $ smin result) $ (size result * betaLo a b)
  hi = max (fromIntegral $ smin result) $ min (fromIntegral $ smax result) $ (size result * betaHi a b)

eps = 0.001

betaLo a b = quantile d eps
  where
  d = betaDistr a b

betaHi a b = quantile d (1 - eps)
  where
  d = betaDistr a b

makeResult :: String -> Result
makeResult s = Result (read a) (read b) (read c) (read d) (read e) (read f) (read g) (read h) (read i)
  where
  vals = map ((!! 2) . words) . leave 9 $ lines s
  [a,b,c,d,e,f,g,h,i] = vals

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

time action = do
  t0 <- getTime Monotonic
  x <- action
  rnf x `pseq` return ()
  t1 <- getTime Monotonic
  let dt = (fromIntegral $ toNanoSecs $ diffTimeSpec t1 t0 :: Double) / (10^9)
  return (x, dt)

byPrec policies = do
  putStrLn "precision, samples, time, lo, hi, size"
  forM_ policies $ \pol -> do
    forM_ precisions $ \pr -> do
      forM_ samples $ \n -> do
        (output, t) <- time $ readProcess "bash" ["-c", makeCommand pol pr n] ""
        --putStrLn output
        let result = makeResult output
        let (lo, hi) = bounds result n
        printf "%d, %.3e, %.5f, %.3e, %.3e, %.3e\n" pr (fromIntegral n :: Double) t lo hi (size result)

main = hSetBuffering stdout LineBuffering >> getArgs >>= byPrec
