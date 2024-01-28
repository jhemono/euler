import IntPrimes
import Data.List

greaterPermutations p = dropWhile (< p) . sort . nub . map read . permutations . show $ p

arithmeticSubsequences (p0:ps) = filter ((== 3) . length) ars
  where
    ars = map (\p -> p0 : p : if ((p + p - p0) `elem` ps) then [p + p - p0] else []) ps

answer = filter ((>= 3) . length) . concatMap (arithmeticSubsequences . filter isPrime . greaterPermutations) $ primes4d
  where
    primes4d = takeWhile (< 10000) . dropWhile (<= 1000) $ primes

main = print answer
