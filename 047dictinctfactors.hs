module Main (main) where
import IntPrimes
import Data.List
import qualified Data.IntMap.Strict as Map

type PF = [(Int,Int)]

expo x y =
  case x `div` y of
    0 -> 1
    q -> 1 + expo q y

myprimes = take 20 primes

nextQuad :: PF -> PF
nextQuad ((p,i):pfs) = (p,i+1) : ys
  where
    ys = map (\pt -> (pt, expo (p * i) pt)) $ ts
    greaterPrimes = dropWhile (<= p) myprimes
    ts = case pfs of
      [] -> take 1 greaterPrimes
      (pn,_):_ -> take 1 $ takeWhile (< pn) greaterPrimes

next :: PF -> [PF]
next (pfs @ (pf:pfss)) = (map (:pfss) . nextQuad $ pfs) ++ map (pf:) (next pfss)
next [] = []

first :: PF
first = [(2,1),(3,1),(5,1)]

list :: PF -> [(Int,PF)]
list f = aux (Map.singleton (toint f) f)
  where
    aux m = (n0,pf0) : aux (ns `Map.union` m')
      where
        ((n0,pf0), m') = Map.deleteFindMin m
        ns = Map.fromList . filter ((> n0) . fst) . map (\pf -> (toint pf, pf)) $ next pf0

toint :: PF -> Int
toint = product . map (\(p,i) -> p ^ i)

truc = list first

machin = groupBy (\(n,_) (n',_) -> succ n == n') truc

bidule :: [(Int,Int)]
bidule = map (\((n,_):(n',_):_) -> (n,n')) . filter ((>= 2) . length) $ machin

criterion1 n = length (primeDecomposition n) == 4
criterion n n' = criterion1 n && criterion1 n'

chose :: [(Int,Int)]
chose = filter (\(n,n') -> criterion1 (n'+n') && let n2 = n + n in criterion n2 (n2+1)) bidule

deco (n,n') = map primeDecomposition [n*2,n*2+1,n'*2]

main :: IO ()
main = print $ head chose
