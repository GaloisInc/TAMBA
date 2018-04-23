import System.IO
import System.Process
import Control.Monad
import Control.Exception
import Text.Printf
import Data.Char (isSpace)
import Data.Ratio
import Data.Maybe
import Data.List (maximumBy)

ships = ["plan"] --["us", "hmas", "plan"]
ports = ["boho"] --["cebu", "boho", "siqu"]

main = do
    res <- runAll
    putStrLn "from,to,n-queries,max-belief,leakage"
    nqs <- forM res $ \(f,t,nq,mb,l) -> do
             putStrLn $ printf "%s,%s,%d,%f,%f" f t nq mb l
             return nq
    print $ sum nqs `div` length nqs


runAll :: IO [(String, String, Int, Double, Double)]
runAll = forM allPairs $ \(f,t) -> do
    out <- readProcess "bash" ["-c", mkFileCommand f t] ""
    let numQueries = dumbParseFileCreation out
    let numPerms = 2 ^ numQueries :: Int
    ran <- sequence $ mkAllCommands f t numPerms
    let sucesses = catMaybes ran
    putStrLn $ "Number of sucesses: " ++ show (length sucesses)
    let (mb,leak) = processSuccesses sucesses
    putStrLn $ printf "From %s to %s: %d queries" f t numQueries
    putStrLn $ "Max-belief: " ++ show mb
    putStrLn $ "Cumulative Leakge: " ++ show leak
    return (f,t,numQueries,mb,leak)
{-
    let [mbStr, leakStr] = take 2 $ (tailN 4 (lines ran))
    let (mb, leak)       = (dumbParseMB mbStr, dumbParseLeak leakStr)
    putStrLn $ "Max-belief: " ++ show mb
    putStrLn $ "Cumulative Leakge: " ++ show leak
    return (f,t,numQueries,mb,leak)
-}

allPairs = sToP ++ map swap sToP
  where
    sToP = [ (s,p) | s <- ships, p <-ports]
    swap (x,y) = (y,x)

mkFileCommand from to =
    printf "python mk_static_queries.py %s %s man 30000 query-files/static-%s-%s" from to from to

mkRunCommand from to i =
    printf "./batch-static.sh query-files/static-%s-%s%d.prob" from to i

mkAllCommands f t n =
    map (\i -> either (const Nothing) Just <$> ((try $ readProcess "bash" ["-c", mkRunCommand f t i] ""):: IO (Either IOException String))) [0..(n-1)]

processSuccesses xs = maximumBy g (map f xs')
  where
    xs' = map (take 2 . tailN 4 . lines) xs
    f [mbStr, leakStr] = (dumbParseMB mbStr, dumbParseLeak leakStr)

    g (_,x) (_,y)      = compare x y

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
