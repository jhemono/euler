module Champernowne where

champer i = aux i 1
  where aux j d
          | j < lnb d = (j, d)
          | otherwise = aux (j - d * lnb d) (d+1)
        lnb d = 9 * 10 ^ (d-1)

