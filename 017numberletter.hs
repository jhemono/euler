module NumberLetter where

lc 0 = "zero"
lc 1 = "one"
lc 2 = "two"
lc 3 = "three"
lc 4 = "four"
lc 5 = "five"
lc 6 = "six"
lc 7 = "seven"
lc 8 = "eight"
lc 9 = "nine"
lc 10 = "ten"
lc 11 = "eleven"
lc 12 = "twelve"
lc 13 = "thirteen"
lc 14 = "fourteen"
lc 15 = "fifteen"
lc 16 = "sixteen"
lc 17 = "seventeen"
lc 18 = "eighteen"
lc 19 = "nineteen"
lc 20 = "twenty"
lc 30 = "thirty"
lc 40 = "forty"
lc 50 = "fifty"
lc 60 = "sixty"
lc 70 = "seventy"
lc 80 = "eighty"
lc 90 = "ninety"
lc 100 = "hundred"
lc 1000 = "thousand"

letterCount 0 = ""
letterCount n
  | 1 <= n && n <= 20 = lc n
  | 21 <= n && n <= 99 = let unit = n `mod` 10 in lc (n - unit) ++ letterCount unit
  | 100 <= n && n <= 999 = let tens = n `mod` 100 in lc (n `div` 100) ++ lc 100 ++ (if tens == 0 then "" else "and" ++ letterCount tens)
  | 1000 <= n && n <= 1999 = "onethousand" ++ letterCount (n `mod` 1000)
