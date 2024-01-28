import Data.List

mapAccumL1_ f ((_,x):xs) = let (_,xs') = mapAccumL f x xs in x : xs'
mapAccumR1_ f ((_,x):xs) = let (_,xs') = mapAccumR f x xs in x : xs'

miniPath :: [[Int]] -> [Int]
miniPath xss = foldl1 aux xss
  where
    aux paths xs = mapAccumR1_ truc . zip xs . mapAccumL1_ truc . zip xs . zipWith (+) xs $ paths
    truc p' (x,p) = let m = min (p'+x) p in (m,m)

main = do
  cs <- readFile "p082_matrix.txt"
  let mat = map (map read . words) . lines $ cs
      tran = transpose mat
      paths = miniPath tran
      min = minimum paths
  print min
