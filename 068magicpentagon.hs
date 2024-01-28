import Control.Monad
import Data.List

chose :: Int -> [a] -> [[a]]
chose 0 _ = [[]]
chose 1 xs = map (:[]) xs
chose n xs
  | length xs < n = []
chose n (x:xs) = map (x:) (chose (n-1) xs) ++ chose n xs

pick :: [Int] -> [(Int,[Int])]
pick xs = do
  x <- xs
  return (x,delete x xs)

answer :: [(String,Int)]
answer = do
  e0 <- reverse [1..6] -- There remains enough greater numbers for the other external nodes
  rest <- chose (6 - e0) . reverse $ [e0+1..10]
  let es = [e0+1..10] \\ rest
  let inner = rest ++ reverse [1..e0-1]
  (i0,inner0) <- pick inner
  (i1,inner1) <- pick inner0
  guard $ i0 > i1
  let sum = e0+i0+i1
  guard $ sum >= 13 && sum <= 20 -- Minimum sum where number 10 and 1 will be
  (e1,es1) <- pick es
  (i2,inner2) <- pick inner1
  guard $ e1+i1+i2 == sum
  (e2,es2) <- pick es1
  (i3,inner3) <- pick inner2
  guard $ e2+i2+i3 == sum
  (e3,es3) <- pick es2
  (i4,inner4) <- pick inner3
  guard $ e3+i3+i4 == sum
  (e4,es4) <- pick es3
  guard $ e4+i4+i0 == sum
  return $ (concatMap show [e0,i0,i1,e1,i1,i2,e2,i2,i3,e3,i3,i4,e4,i4,i0], sum)

main :: IO ()
main = mapM_ print answer
