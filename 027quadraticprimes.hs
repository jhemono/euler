module QuadraticPrimes where
import Primes (primes, isPrime)
import Data.List (maximumBy)
import Data.Function (on)

coefficients = [(a,b) | b <- takeWhile (< 1000) primes, a <- [(1-b)..999], isPrime (1+a+b)]

function (a,b) n = (n * n) + (a * n) + b

primeSerie coef = takeWhile (\p -> p > 1 && isPrime p) . map (function coef) $ [0..]

compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength (_:_) [] = GT
compareLength [] (_:_) = LT
compareLength [] [] = EQ

answer = (a * b, (a,b))
  where (a,b) = fst . maximumBy (compareLength `on` snd) . map (\coef -> (coef, primeSerie coef)) $ coefficients
