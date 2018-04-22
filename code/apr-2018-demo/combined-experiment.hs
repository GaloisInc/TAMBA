import System.IO
import System.Process
import Control.Monad
import Text.Printf
import Data.Char (isSpace)
import Data.Ratio

ships = ["us", "hmas", "plan"]
ports = ["cebu", "boho", "siqu"]

main = do
    res <- runAll
    putStrLn "from,to,n-queries,max-belief,leakage"
    forM_ res $ \(f,t,nq,mb,l) -> do
        putStrLn $ printf "%s,%s,%d,%f,%f" f t nq mb l


runAll = forM allPairs $ \(f,t) -> do
    out <- readProcess "bash" ["-c", mkFileCommand f t] ""
    let numQueries = dumbParseFileCreation out
    putStrLn $ printf "From %s to %s: %d queries" f t numQueries
    ran <- readProcess "bash" ["-c", mkRunCommand f t] ""
    let [mbStr, leakStr] = take 2 $ (tailN 4 (lines ran))
    let (mb, leak)       = (dumbParseMB mbStr, dumbParseLeak leakStr)
    putStrLn $ "Max-belief: " ++ show mb
    putStrLn $ "Cumulative Leakge: " ++ show leak
    return (f,t,numQueries,mb,leak)

allPairs = sToP ++ map swap sToP
  where
    sToP = [ (s,p) | s <- ships, p <-ports]
    swap (x,y) = (y,x)

mkFileCommand from to =
    printf "python mk_queries.py %s %s man 30000 query-files/%s-%s.prob" from to from to

mkRunCommand from to =
    printf "./batch.sh query-files/%s-%s.prob" from to

dumbParseFileCreation :: String -> Int
dumbParseFileCreation str = read $ takeWhile (not . isSpace) $ drop 2 str'
  where
    str' = dropWhile (/= ':') str

tailN n xs = f (take n xs) (drop n xs)
  where
    f xs [] = xs
    f (x:xs) (y:ys) = f (xs ++ [y]) ys

dumbParseMB :: String -> Double
dumbParseMB str = fromRational $ read (takeWhile (/= '/') str') % read (drop 1 $ dropWhile (/= '/') str')
  where
    str' = drop 2 (dropWhile (/= ':') str)

dumbParseLeak :: String -> Double
dumbParseLeak = read . drop 2 . dropWhile (/= ':')
