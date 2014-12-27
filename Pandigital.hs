module Pandigital where
import Data.List ((\\), find, delete, unfoldr)
import Control.Monad (mplus, mzero, MonadPlus)

ads1 :: Set
ads1 = [1,2,3,4,5,6,7,8,9]

ads0 :: Set
ads0 = 0 : ads1

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

firstpandigital :: (MonadPlus m) => Set -> m (Set, Pandigital, Int)
firstpandigital [] = mzero
firstpandigital (0:ads) = do
  (ads', pd, d) <- firstpandigital ads
  return (0:ads', pd, d)
firstpandigital (d:ads) = return (ads, [d], d)

succdigital :: Set -> (Pandigital,Int) -> Maybe (Pandigital,Int)
succdigital s (pd,n) =
  do (_,pd',delta) <- succdigital' s pd
     return (pd',n+delta)

firstnpandigital :: Set -> Pandigital
firstnpandigital s =
  case reverse s of
    (0:x:xs) -> x:0:xs
    pd -> pd

toint :: Pandigital -> Int
toint p = sum $ zipWith (\d i -> d * (10 ^ i)) p ([0..] :: [Int])

range' :: Set -> Pandigital -> [(Pandigital,Int)]
range' s pd0 = (pd0,n0) : unfoldr wrap (pd0,n0)
  where
    n0 = toint pd0
    wrap (pd,n) =
      do (pd',n') <- succdigital s (pd,n)
         return ((pd',n'),(pd',n'))

range :: Set -> Pandigital -> [Int]
range s pd0 = fmap snd (range' s pd0)

rangepd :: Set -> Pandigital -> [Pandigital]
rangepd s pd0 = fmap fst (range' s pd0)
