import Data.Maybe
import Control.Monad

pentagonals :: [Int]
pentagonals = fmap pentagonal [1..]
pentagonal :: Int -> Int
pentagonal n = (n * (3 * n - 1)) `div` 2

deltas = zipWith (-) (tail pentagonals) pentagonals

answer = do
  d <- pentagonals
  (pk,_) <- takeWhile (\(_,delta) -> delta <= d) $ zip pentagonals (0 : deltas)
  let pj = pk - d
  guard $ isJust $ sidePentagonal pj
  let s = pk + pj
  guard $ isJust $ sidePentagonal s
  return d

sidePentagonal :: Int -> Maybe Int
sidePentagonal x = do
  let a = 24 * x + 1
  b <- integralRoot a
  let c = 1 + b
  c `integralDiv` 6
  where
    integralDiv m d
      | r == 0 = Just q
      | otherwise = Nothing
        where (q,r) = m `divMod` d
    integralRoot m
      | m == root * root = Just root
      | otherwise = Nothing
        where root = floor . (sqrt :: Double -> Double) . fromIntegral $ m

main = print (head answer)
