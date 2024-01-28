import Pandigital
import IntPrimes

substrings :: Pandigital -> [Pandigital]
substrings [x,y,z,_] = [[x,y,z]]
substrings (x:y:z:zs) = [x,y,z] : substrings (y:z:zs)
substrings _ = []

answer :: Int
answer = sum . fmap toint . filter substringDivisible $ pds where
  substringDivisible = and . zipWith divides primes . reverse . fmap toint . substrings
  divides p n        = n `mod` p == 0
  pds                = rangepd ads0 (firstnpandigital ads0)

main :: IO ()
main = print answer
