triangulars :: [Int]
triangulars = scanl1 (+) [1..]

pentagonals :: [Int]
pentagonals = fmap pentagonal [1..]
  where pentagonal n = (n * (3 * n - 1)) `div` 2

hexagonals :: [Int]
hexagonals = fmap hexagonal [1..]
  where hexagonal n = n * (2 * n - 1)

start = [(triangulars, 285), (pentagonals, 165), (hexagonals, 143)]

search (h:_) (p:_) (t:_)
  | h == p && h == t = h
search (_:(hs @ (h':_))) ps ts =
  search hs (dropWhile (< h') ps) (dropWhile (< h') ts)

main = do
  print $ search (drop 143 hexagonals) (drop 165 pentagonals) (drop 285 triangulars)
