module PandigitalMultiples where
import Data.List ((\\), nub, find, delete, unfoldr)
import Control.Monad (mplus, mzero)

ads0 :: [Int]
ads0 = [1,2,3,4,5,6,7,8,9]

type Set = [Int]
type Pandigital = [Int]

succdigital :: Set -> Pandigital -> Maybe (Set,Pandigital)
succdigital ads [] = firstpandigital ads
succdigital ads (d:ds) = first `mplus` second
  where first = do
          let ads' = ads \\ ds
          d' <- find (> d) ads'
          return (delete d' ads', d':ds)
        second = do
          (ads' @ (d':_),ds') <- succdigital ads ds
          return (delete d' ads', d':ds')

firstpandigital [] = mzero
firstpandigital (d:ads) = return (ads, [d])

n9 :: Pandigital
n9 = [5,4,6,3,7,2,8,1,9]

range :: [Pandigital]
range = unfoldr wrap n9
  where wrap n =
          do (_,a) <- succdigital ads0 n
             return (a,a)

nb = length range

order :: Int -> Int
order n = aux n 0
  where aux n' i
          | n' < 10 = i
          | otherwise = aux (n' `div` 10) (i+1)
rrange = reverse range
toint = foldl (\acc d -> acc * 10 + d) 0
taken i xss = (n,xs)
  where n = toint p
        (p,xs) = splitAt i xss
try i pd = aux n0 pds
  where (n0,pds) = taken i pd
        aux _ [] = True
        aux n xs = let n' = n + n0
                       o = order n' + 1
                       (n'', xs') = taken o xs
                       in n' == n'' && aux n' xs'

truc pd = any (`try` reverse pd) [1..4]

answer = find truc rrange
