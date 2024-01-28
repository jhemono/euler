import Data.List
import Data.Maybe
coefficient n
  | root * root == n = [root]
  | otherwise = root : aux root 1
  where
    root = floor . (sqrt :: Double -> Double) . fromIntegral $ n
    aux m d = s' : aux m'' d''
      where
        d' = n - m * m
        d'' = d' `div` (gcd d' d)
        (m'',s') = truc m 0
        truc m' s
          | root + m' >= d'' = truc (m' - d'') (s+1)
          | otherwise = ((- m'),s)

period n = fmap (+1) $ elemIndex (2 * c0) cs
  where (c0:cs) = coefficient n

oddPeriod n = maybe False (odd) $ period n

main = print $ length . filter oddPeriod $ [1..10000]
