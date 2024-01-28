import Data.List

main = (mapM_ putStrLn) . nub . words =<< readFile "p079_keylog.txt"
-- Solved by hand on my way to coding a solution.
