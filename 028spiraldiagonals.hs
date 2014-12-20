module SpiralDiagonal where

matrixSizes :: [Integer]
matrixSizes = takeWhile (< 1001) [1,3..]

jumps = map (+1) matrixSizes

diagnumbers = scanl (+) 1 . concatMap (replicate 4) $ jumps

answer = sum diagnumbers
