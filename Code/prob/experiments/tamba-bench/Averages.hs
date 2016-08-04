import qualified Data.ByteString.Lazy as BS
import Data.Csv
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment

type Row = (String, Int, Int, Double, Double)

calcAvg :: Vector Row -> Double
calcAvg rs = (V.sum ts) / (fromIntegral (V.length rs))
  where
    ts = V.map (\(_,_,_,_,t) -> t) rs

calcFileAvg :: FilePath -> IO ()
calcFileAvg f = do
  conts <- BS.readFile f
  let decoded = decode NoHeader conts :: Either String (Vector Row)
  case decoded of
    Left _ -> putStrLn $ "Error in calculating average for " ++ f
    Right rs -> putStrLn $ f ++ ": " ++ (show $ calcAvg rs)

main = do
  fs <- getArgs
  mapM_ calcFileAvg fs
