import IntPrimes
import Data.List

answer = find (\(_,(n,d)) -> n * 10 <= d) . tail $ zip matrixSizes bidule
  where
    matrixSizes = [1,3..]
    jumps = map (+1) matrixSizes
    truc = map (replicate 4) $ jumps
    machin = ([1] :) . snd $ mapAccumL (\acc js -> (sum js + acc, tail $ scanl (+) acc js)) 1 truc
    bidule = tail $ scanl (\(np, nn) ns -> (np + length (filter isPrime ns), nn + length ns)) (0,0) machin

main = putStrLn $ maybe "Can't find" (\(n,_) -> show n) answer

