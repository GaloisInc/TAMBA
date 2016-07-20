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
import System.Timeout

leave :: Int -> [a] -> [a]
leave n xs = leave' xs (drop n xs)
  where
    leave' xs     []     = xs
    leave' (_:xs) (y:ys) = leave' xs ys

-- Parsing is currently baked into the command line, using unix tricks.
-- VERY brittle.
makeCommand pol dom inl = 
  printf "/vagrant/Code/prob/prob %s --domain %s --precision 0 %s" pol dom inl

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

domains = ["box", "octalatte", "poly"]

inlinings = ["", "--inline"]

eps = 0.001

makeResult :: String -> Result
makeResult s = Result (read a) (read b) (read c) (read d) (read e) (read f) (read g) (read h) (read i)
  where
  vals = map ((!! 2) . words) . leave 9 $ lines s
  [a,b,c,d,e,f,g,h,i] = vals


-- time :: Int -> IO a -> IO (Maybe a, 
time tmax action = do
  t0 <- getTime Monotonic
  x <- timeout tmax action
  rnf x `pseq` return ()
  t1 <- getTime Monotonic
  let dt = (fromIntegral $ toNanoSecs $ diffTimeSpec t1 t0 :: Double) / (10^9)
  return (x, dt)

byPrec policies = do
  putStrLn "file, domain, inline, time"
  forM_ policies $ \pol -> do
    forM_ domains $ \dom -> do
      forM_ inlinings $ \inl -> do
        (mOutput, t) <- time (30*60*1000000) $ readProcess "bash" ["-c", makeCommand pol dom inl] ""
        case mOutput of 
          Nothing -> return () -- timed out
          Just output -> do
            let inl' = case inl of
                          "" -> " no"
                          _  -> "yes"
            printf "%s, %s, %s, %.5f\n" pol dom inl' t

main = hSetBuffering stdout LineBuffering >> getArgs >>= byPrec
