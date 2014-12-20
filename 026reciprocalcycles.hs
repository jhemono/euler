module ReciprocalCycles where
import Data.List (elemIndex, maximumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

restsReciprocal n = tail $ iterate (\r -> (10 * r) `mod` n) 1

answer n = fst . maximumBy (compare `on` snd) . map cycleLength $ [3..n]
  where cycleLength d = (d, aux . take d . restsReciprocal $ d)
        aux (x:xs) = fromMaybe (aux xs) (elemIndex x xs)
