import IntPrimes
import Data.List

intRoot :: Int -> Int
intRoot n = floor . (sqrt :: Double -> Double) . fromIntegral $ n

goldbach n p = let n' = (n - p) `div` 2
                   r = intRoot n'
                in r * r == n'

oddComposites :: [Int]
oddComposites = aux primes []
  where aux (p1:(ps @ (p2:_))) acc = ans ++ aux ps acc' where
          acc' = p1 : acc
          ocs = [p1+2,p1+4..p2-2]
          ans = filter predicate ocs
          predicate n = not . any (goldbach n) $ acc'

main :: IO ()
main = do
  print $ take 2 oddComposites
