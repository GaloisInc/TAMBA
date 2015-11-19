module Data.List.Matrix where

import Data.List

type Vector a = [a]
type Matrix a = [[a]]

numRows :: Matrix a -> Int
numRows = length

numColumns :: Matrix a -> Int
numColumns = length . head

zeroVector :: Num a => Int -> Vector a
zeroVector n = replicate n 0

index :: Num a => Int -> Int -> Matrix a -> a
index i j m = (m !! i) !! j

-- | Makes a zero vector with a single 1 element at index `i`.
-- | If `i` is greater than `len - 1` you get a vector longer than `len`
unitVector :: Num a => Int -> Int -> Vector a
unitVector len i = pre ++ [1] ++ post
  where
    pre  = zeroVector i
    post = zeroVector (len - (i + 1))

vecSProd :: Num a => a -> Vector a -> Vector a
vecSProd n vec = [n * x | x <- vec]

matSProd :: Num a => a -> Matrix a -> Matrix a
matSProd n mat = [vecSProd n row | row <- mat]

vecSum :: Num a => Vector a -> Vector a -> Vector a
vecSum = zipWith (+)

matSum :: Num a => Matrix a -> Matrix a -> Matrix a
matSum = zipWith vecSum

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct v w = sum $ zipWith (*) v w

matProduct :: Num a => Matrix a -> Matrix a -> Matrix a
matProduct m n = [map (dotProduct row) (transpose n) | row <- m]

vmatProduct :: Num a => Vector a -> Matrix a -> Vector a
vmatProduct v n = map (dotProduct v) (transpose n)
