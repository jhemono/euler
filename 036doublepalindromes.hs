module DoublePalindromes where
import Data.List (foldl1',unfoldr)

palindromes :: Int -> [[Int]]
palindromes 1 = map (:[]) [1,2,3,4,5,6,7,8,9]
palindromes i
  | odd i =
    do m <- range0
       half <- aux (i `div` 2)
       return $ half ++ [m] ++ reverse half
  | otherwise =
    do half <- aux (i `div` 2)
       return $ half ++ reverse half
  where range0 = 0 : range1
        range1 = [1,2,3,4,5,6,7,8,9]
        aux 0 = []
        aux j = sequence $ range1 : replicate (j-1) range0

paltoInt :: [Int] -> Int
paltoInt = foldl1' (\x y -> x * 10 + y)

isbinPalindrome :: Int -> Bool
isbinPalindrome n = binrep n == reverse (binrep n)

binrep :: Int -> [Int]
binrep = unfoldr aux
  where aux 0 = Nothing
        aux i = let (q,r) = i `divMod` 2 in Just (r,q)

answer :: Int
answer = sum . map (sum . filter isbinPalindrome . map paltoInt . palindromes) $ [1..9]
