module DigitCancelling where
import Control.Monad (guard)

answer :: Int
answer = den `div` gcd num den
  where pairs =
          do d <- [1..9]
             d' <- [d+1..9]
             dc <- [1..9]
             n <- [d*10+dc, dc*10+d]
             n' <- [d'*10+dc, dc*10+d']
             guard $ d * n' == d' * n
             return (d,d')
        (num, den) = foldl1 (\(x,y) (x',y') -> (x*x',y*y')) pairs
