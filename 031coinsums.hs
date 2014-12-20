module CoinSums where
import Control.Monad.State

coins :: [Int]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

type Memo a b = State [(a,b)] b

memoize :: Eq a => a -> Memo a b -> Memo a b
memoize x f =
  do memo <- gets (lookup x)
     case memo of
       Just y -> return y
       Nothing -> do y <- f
                     modify ((x,y):)
                     return y

count' :: [Int] -> Int -> Memo (Int, [Int]) Int
count' _ 0 = return 1
count' [] _ = return 0
count' [c] n = if n `mod` c == 0 then return 1 else return 0
count' (ccs @ (c:cs)) n =
  memoize (n,ccs) (if n >= c
                   then liftM2 (+) (count' (c:cs) (n-c)) (count' cs n)
                   else count' cs n)

count :: [Int] -> Int -> Int
count cs n = evalState (count' cs n) []
