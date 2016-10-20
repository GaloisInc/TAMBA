{-# LANGUAGE BangPatterns #-}
module Utils where

import Data.Array.IO
import Control.Monad (forM)
import System.Random

randDivideInto :: Int -> Int -> IO [Int]
randDivideInto n m = do
    gen <- getStdGen
    arr <- newListArray (1,n') $ ones ++ zeros :: IO (IOArray Int Int)
    xs <- forM [1..n'] $ \i -> do
             j <- randomRIO (i,n')
             vi <- readArray arr i
             vj <- readArray arr j
             writeArray arr j vi
             return vj
    return $ countBins xs
  where
    n'    = n + (m - 1)
    ones  = take (m - 1) $ repeat 1
    zeros = take n $ repeat 0

countBins :: [Int] -> [Int]
countBins = go 0
  where
    go !n []     = [n]
    go !n (0:xs) = go (n + 1) xs
    go !n (1:xs) = n : go 0 xs

zipWith8 :: (a->b->c->d->e->f->g->h->i) ->
            [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]
zipWith8 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) (g:gs) (h:hs) (i:is)
                           = f a b c d e g h i : zipWith8 f as bs cs ds es gs hs is
zipWith8 _ _ _ _ _ _ _ _ _ = []

mapAll :: [a -> b] -> [a] -> [[b]]
mapAll []     _  = []
mapAll (f:fs) xs = map f xs : mapAll fs xs
