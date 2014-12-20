module PandigitalProducts where
import Data.List ((\\), nub, find, delete, unfoldr)
import Control.Monad (mplus, mzero)

ads0 :: [Int]
ads0 = [1,2,3,4,5,6,7,8,9]

type Set = [Int]
type Pandigital = [Int]

succdigital :: Set -> Pandigital -> Maybe (Set,Pandigital,Int)
succdigital ads [] = firstpandigital ads
succdigital ads (d:ds) = first `mplus` second
  where first = do
          let ads' = ads \\ ds
          d' <- find (> d) ads'
          let delta = d' - d
          return (delete d' ads', d':ds,delta)
        second = do
          (ads' @ (d':_),ds',delta) <- succdigital ads ds
          let delta' = (delta * 10) - (d - d')
          return (delete d' ads', d':ds', delta')

firstpandigital [] = mzero
firstpandigital (d:ads) = return (ads, [d], d)

--enumpandigitals ads =
  --do (f @ (_,first)) <- firstpandigital ads
     --f : unfoldr aux first
       --where aux n = do (res @ (_,n')) <- succdigital ads n
                        --return (res, n')
--pantoint :: Pandigital -> Int
--pantoint p = sum $ zipWith (\d i -> d * (10 ^ i)) p ([1..] :: [Int])
--answer = do
  --(ads1,a) <- enumpandigitals ads0
  --(ads2,b) <- enumpandigitals ads1
  --return (a,b)
----answer = [ ((a,b),c) | a <- [1..1963], pandigital a, b <- [1..48], alterdigital a b, let c = a * b, alterdigital2 a b c]
--
--extra ans = sum . nub . map snd $ ans
