module Main where
import Data.Word
import Data.List
triangularNumbers :: [Integer]
triangularNumbers = scanl1 (+) [1..]

racine :: Integer -> Integer
racine = floor . sqrt . fromIntegral

divisors :: Integer -> [Integer]
divisors n = (1 :) $ nub $ concat [ [x, n `div` x] | x <- [2..limit], n `rem` x == 0]
  where limit = racine n

highlyDivisibleTriangular :: Int -> Integer
highlyDivisibleTriangular nd = head $ filter (\n -> length (divisors n) >= nd) triangularNumbers

-- not optimised using the square root
fastHDT :: Int -> Word64
fastHDT nd =
  let aux n i = if divisorCount n >= nd then n else aux (n+i) (i+1)
      divisorCount n =
        let half = quot n 2
            aux d count = if d > half
                          then count
                          else (if (n `mod` d) == 0
                                then aux (d+1) (count+1)
                                else aux (d+1) count)
        in aux 1 1
  in aux 1 2

main :: IO ()
main = interact (show . fastHDT . read)
