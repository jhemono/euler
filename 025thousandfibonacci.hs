module ThousandFibonacci where
import Data.List (find)

fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)

digit1000 :: Integer -> Bool
digit1000 = (>= 10 ^(999 :: Integer))

answer :: Maybe (Int, Integer)
answer = find (digit1000 . snd) $ zip [1..] fibonacci
