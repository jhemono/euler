import Control.Applicative
data Numeral = I | V | X | L | C | D | M deriving (Eq, Show)

reduce (V:V:ns) = X : reduce ns
reduce (L:L:ns) = C : reduce ns
reduce (D:D:ns) = M : reduce ns
reduce (I:I:I:I:ns) = I : V : reduce ns
reduce (V:I:I:I:I:ns) = I : X : reduce ns
reduce (X:X:X:X:ns) = X : L : reduce ns
reduce (L:X:X:X:X:ns) = X : C : reduce ns
reduce (C:C:C:C:ns) = C : D : reduce ns
reduce (D:C:C:C:C:ns) = C : M : reduce ns
reduce (n:ns) = n : reduce ns
reduce [] = []

ex = X:X:X:X:V:I:I:I:I:[]

charToNumeral c = case c of
  'I' -> I
  'V' -> V
  'X' -> X
  'L' -> L
  'C' -> C
  'D' -> D
  'M' -> M

toNumerals = map charToNumeral

fix f a
  | a == b = a
  | otherwise = fix f b
  where b = f a

main = do
  ns <- map toNumerals . words <$> readFile "p089_roman.txt"
  let ns' = map (fix reduce) ns
      l = sum $ map length ns
      l' = sum $ map length ns'
  print (l - l')
