module Main where
import Data.List (foldl')

solutions :: Int -> [(Int,Int,Int)]
solutions p = [ (a,b,c) | a <- [1..l], b <- [a..l+l], let c = p - a - b, c*c == a*a + b*b]
  where l = p `div` 3

answer :: Int
answer = fst $ foldl' (\(pmax,smax) p -> let s = length $ solutions p in if s > smax then (p,s) else (pmax,smax)) (0,0) [12..1000]

main :: IO ()
main = print answer
