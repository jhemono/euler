diagonals :: [[a]] -> [[a]]
diagonals ((x:xs):xss) = [x] : zipWith' (:) (:[]) (id) xs (diagonals xss)
  where
    zipWith' fab fa fb = aux
      where
        aux (a:as) (b:bs) = fab a b : aux as bs
        aux as [] = map fa as
        aux [] bs = map fb bs
diagonals ([]:xss) = diagonals xss
diagonals [] = []

miniPath :: [[Int]] -> [Int]
miniPath xss = foldl1 (\acc xs -> zipWith3 (\l r x -> min (l+x) (r+x)) (head acc : acc) (acc ++ [last acc]) xs) xss

miniPathSqu :: Int -> [[Int]] -> [Int]
miniPathSqu n xss = miniPath (reverse xssb ++ [miniPath xsst])
  where
    (xsst,xssb) = splitAt n xss

main = do
  cs <- readFile "p081_matrix.txt"
  let ls = lines cs
      mat = map (map read . words) $ ls
      dia = diagonals mat
      minis = miniPathSqu 80 dia
      answer = minimum minis
  print answer
