module Champernowne where

champer = aux 1
  where aux d i
          | i < d * lnb d = aux' d i nb1s []
          | otherwise = aux (d+1) (i - d * lnb d)
        lnb d = 9 * 10 ^ (d-1)
        --aux' :: Int -> Int -> [Int] -> [Int] -> Int
        aux' 0 i (di:_) acc = reverse (di:acc) !! i
        aux' d i (di:dis) acc
          | i <= d * knb d = aux' (d-1) i nb0s (di:acc)
          | otherwise = aux' d (i - d * knb d) dis acc
        knb d = 10 ^ (d - 1)
        nb0s = 0 : nb1s
        nb1s = [1..9]
