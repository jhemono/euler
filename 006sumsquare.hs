module SumSquare where

sumSquareDifference :: Integral a => [a] -> a
sumSquareDifference r = square (sum r) - (sum . map square $ r)
  where
    square n = n * n
