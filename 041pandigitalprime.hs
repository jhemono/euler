module PandigitalPrime where
import IntPrimes (isPrime)
import Data.List ((\\), nub, find, delete, unfoldr,inits)
import Control.Monad (mplus, mzero)
import Control.Applicative ((<*>))

ads0 :: Set
ads0 = [1,2,3,4,5,6,7,8,9]

type Set = [Int]
type Pandigital = [Int]

succdigital' :: Set -> Pandigital -> Maybe (Set,Pandigital,Int)
succdigital' ads [] = firstpandigital ads
succdigital' ads (d:ds) = first `mplus` second
  where first = do
          let ads' = ads \\ ds
          d' <- find (> d) ads'
          let delta = d' - d
          return (delete d' ads', d':ds,delta)
        second = do
          (ads' @ (d':_),ds',delta) <- succdigital' ads ds
          let delta' = (delta * 10) - (d - d')
          return (delete d' ads', d':ds', delta')

firstpandigital [] = mzero
firstpandigital (d:ads) = return (ads, [d], d)

succdigital :: Set -> (Pandigital,Int) -> Maybe (Pandigital,Int)
succdigital s (pd,n) =
  do (_,pd',delta) <- succdigital' s pd
     return (pd',n+delta)

firstnpandigital :: Set -> Pandigital
firstnpandigital s = reverse s

toint :: Pandigital -> Int
toint p = sum $ zipWith (\d i -> d * (10 ^ i)) p ([0..] :: [Int])

range :: Set -> Pandigital -> [Int]
range s pd0 = n0 : unfoldr wrap (pd0,n0)
  where
    n0 = toint pd0
    wrap (pd,n) =
      do (pd',n') <- succdigital s (pd,n)
         return (n',(pd',n'))

answer = head . concatMap (filter isPrime . reverse . (range <*> firstnpandigital)) . reverse . tail . inits $ ads0
