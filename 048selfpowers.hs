mAdd m x y = (x + y) `mod` m

-- Assumes one of the operand is less than 1000 as should be the case here
mMult m x y = (x * y) `mod` m

mExp m x y = foldl (mMult m) 1 $ replicate y x

answer = foldl (mAdd m) 0 . map (\n -> mExp m n n) $ [1..1000]
  where m = 10^10

main = print answer
