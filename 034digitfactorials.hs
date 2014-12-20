module DigitFactorial where
import Data.List (unfoldr,findIndex)

factorial :: Int -> Int
factorial n = product [2..n]

isDigitFactorial n = n == (sum . map factorial $ digits n)

digits = unfoldr (\n -> if n == 0 then Nothing else Just (mod n 10, div n 10))

i = case findIndex (<=0) deltas of
      Nothing -> undefined
      Just n -> n
  where deltas = map (\x -> (x * factorial 9) - (10 ^ x)) [1..]

answer = sum . filter isDigitFactorial $ [3..10^i]
