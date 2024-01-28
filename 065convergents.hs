import Data.Ratio
import Data.List

e = 2 : concatMap (\n -> [1, n, 1]) [2,4..]

approx [d] = d % 1
approx (d:ds) = (d % 1) + (1 / approx ds)

sumDigits :: Integer -> Int
sumDigits = sum . map (read . (:[])) . show

answer = sumDigits . numerator . approx . take 100 $ e

main = print answer
