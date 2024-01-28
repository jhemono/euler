module Champernowne where
import Data.Monoid
import Data.Foldable

champer' :: Int -> Int -> [[Int]] -> Int
champer' l i ((c:cs):css) =
  if i < nbleafs css
  then (if i `mod` l == 0
        then c
        else champer' l (i-1) css)
  else champer' l (i-(nbleafs css)) (cs:css)
  where nbleafs xs = l * getProduct (foldMap (Product .length) xs)

champer :: Int -> Int
champer = aux 1
 where nb1s = [1..9]
       nb0s = 0 : nb1s
       css l = nb1s : replicate (l-1) nb0s
       aux d i =
         if i < lengthddigits
         then champer' d i (css d)
         else aux (d+1) (i-lengthddigits)
         where lengthddigits = d * 9 * 10 ^ (d-1)

answer :: Int
answer = getProduct . foldMap (Product . champer) $ indices
  where
    indices = fmap (\x -> x - 1) . take 7 . iterate (*10) $ 1
