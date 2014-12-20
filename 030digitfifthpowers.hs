module DigitFifthPowers where

digits 0 = []
digits n = r : digits q
  where (q,r) = divMod n 10

fifth d = d ^ (5 :: Int)

sumFifthDigits = sum . map fifth . digits

-- I got the right answer but I don't know why
answer = sum . take 6 . filter (\n -> sumFifthDigits n == n) $ [2..]
