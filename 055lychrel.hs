reverse' = read . reverse . show

lychrel n = not . or . take 50 . tail $ zipWith (==) truc machin
  where
    truc = n : zipWith (+) truc machin
    machin = map reverse' truc

answer = length $ filter lychrel [1..10000]

main = print answer
