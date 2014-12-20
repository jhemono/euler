module PowerDigitSum where
import Data.List (unfoldr)

power :: (Num a, Integral b) => a -> b -> a
power _ 0 = 1
power x 1 = x
power x n
  | even n = power x (n `div` 2) * power x (n `div` 2)
  | otherwise = x * power x (n-1)

sumDigits :: Integral a => a -> a
sumDigits n = sum $ unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 10, x `div` 10)) n
