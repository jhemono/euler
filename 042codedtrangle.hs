module CodedTriangle where
import Data.Char (ord,toLower)
import Control.Applicative ((<$>))
import Data.Foldable
import Data.Monoid
import Data.List hiding (elem)
import Prelude hiding (elem, mapM_,break)

parser :: String -> [String]
parser [] = []
parser ('"':cs) = let (name,'"':cs') = break (== '"') cs in name : parser cs'
parser (_:cs) = parser cs

alphabeticalValue :: String -> Int
alphabeticalValue = getSum . foldMap (Sum . charValue)
  where
    charValue c = (ord . toLower $ c) - ord 'a' + 1

triangularNumbers :: [Int]
triangularNumbers = scanl1 (+) [1..]

isTriangularNumber :: Int -> Bool
isTriangularNumber n = n `elem` (takeWhile (<= n) triangularNumbers)

answer :: String -> IO ()
answer file = 
  do
    dictionary <- parser <$> readFile file
    let triangularWords = filter (isTriangularNumber . alphabeticalValue) $ dictionary
    mapM_ putStrLn triangularWords
    print $ length triangularWords
