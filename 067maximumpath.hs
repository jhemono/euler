pyramidMatrix :: [String] -> [[Integer]]
pyramidMatrix pyramid = map (map read . words) pyramid

maximumPath :: [[Integer]] -> Integer
maximumPath = maximum . foldl1 (\acc ns -> zipWith3 (\l r n -> max l r + n) (head acc : acc) (acc ++ [last acc]) ns)

main =
  do mat <- fmap (pyramidMatrix . lines) $ readFile "p067_triangle.txt"
     print $ maximumPath mat
