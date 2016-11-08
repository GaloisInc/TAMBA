module Dist
  ( Dist, lookup, insert
  , fromList, fromListWithBounds
  ) where

{- In this module we define a Distribution (Dist), which is a mapping from some
 - index value (which has to be a member of the Ord class) to the 'reals'
 - (Double).
 -
 - Ideally we could make Dist a member of the class 'Bounded', but
 - Distributions
 - will have different bounds depending on the nature of the query under test.
 - Therefore we carry around the bounds as part of the data structure.
 -}


import Prelude hiding (lookup)
import Data.Map (Map)
import Data.List (sort)
import qualified Data.Map as M
import System.Random

data Dist a = Dist { dist :: Map a Double
                   , bounds :: Bound a
                   }

type Bound a = (a, a)

inRange :: Ord a => Bound a -> a -> Bool
inRange (l,h) v = l <= v && h >= v

lookup :: Ord a => a -> Dist a -> Maybe Double
lookup k (Dist m _) = M.lookup k m

insert :: Ord a => a -> Double -> Dist a -> Dist a
insert k v (Dist m bs)
  | inRange bs k = Dist (M.insert k v m) bs
  | otherwise    = error "Trying to insert a key that is out of range of the dist"

fromList :: Ord a => [(a, Double)] -> Dist a
fromList xs = Dist (M.fromList xs) bound
  where
    xs'   = sort $ map fst xs
    bound = (minimum xs', maximum xs')

fromListWithBounds :: Ord a => Bound a -> [(a, Double)] -> Dist a
fromListWithBounds bs xs = Dist (M.fromList xs) bs

empty :: Ord a => Bound a -> Dist a
empty b = Dist (M.empty) b
