import Data.Ratio
sqrt2 = 1 : repeat 2

approx = aux id
  where
    aux k (d:ds) =
      k (d%1) : aux (\r -> k (d%1 + (1 / r))) ds

main = print . length . filter aux . take 1001 . approx $ sqrt2
  where
    aux r = nbdigit n > nbdigit d
      where
        nbdigit = length . show
        n = numerator r
        d = denominator r

