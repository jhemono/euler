import qualified Data.IntSet as S

next :: Int -> Int
next = aux 0
  where
    aux sum n
      | n < 10 = sum + n * n
      | otherwise = aux (sum + r * r) q
      where (q,r) = n `divMod` 10

arrives89 :: Int -> Bool
arrives89 89 = True
arrives89 1 = False
arrives89 n = arrives89 (next n)

main = do
  let set = foldl (\s n -> if arrives89 n then S.insert n s else s) S.empty [1..567]
      n = length . filter (flip S.member set . next) $ [568..10000000]
      n' = S.size set
  print (n+n')
