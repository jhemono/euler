module PalindromeProduct where

isPalindromic :: Integer -> Bool
isPalindromic n = representation == reverse representation
  where
    representation = show n

largestPalindromic :: Integer
largestPalindromic =
  maximum [a * b | a <- threeDigit, b <- threeDigit, isPalindromic (a * b)]
  where
    threeDigit = [100..999]
