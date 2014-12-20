module Collatz where
import Data.List (elemIndex, maximumBy)
import Data.Maybe (fromMaybe)

collatz :: Integral a => a -> [a]
collatz = iterate (\n -> if even n then n `div` 2 else (3 * n) + 1)

longestCollatz :: Integral a => a -> a
longestCollatz n = fst . maximumBy (\(_,ci) (_,cj) -> compare ci cj) . zip range . map (fromMaybe 0 . elemIndex 1 . collatz) $ range
  where range = [1..n]
