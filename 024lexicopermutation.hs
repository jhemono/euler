module LexicoPermutation where
import Data.List (delete)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

permutation :: Eq a => [a] -> Int -> [a]
permutation [] _ = []
permutation xs n = x : permutation xr r
  where (i, r) = divMod n (factorial (length xs - 1))
        x = xs !! i
        xr = delete x xs
