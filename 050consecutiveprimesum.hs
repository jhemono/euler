import IntPrimes
import Control.Monad

sumWindow l xs = scanl (\s (b,a) -> s - b + a) (sum win0) $ zip xs as
  where
    (win0,as) = splitAt l xs

isEmpty [] = True
isEmpty _ = False

answer l0
  | isEmpty truc = Nothing
  | otherwise = (answer (l0 + 2)) `mplus` (return (head truc))
  where
    truc = filter isPrime . takeWhile (<1000000) . sumWindow l0 $ primes

main = print $ answer 21
