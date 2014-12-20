module NameScores where
import Data.Char (ord, toLower)
import Data.List (sort)
import Control.Applicative ((<$>))

parser :: String -> [String]
parser [] = []
parser ('"':cs) = let (name,'"':cs') = break (== '"') cs in name : parser cs'
parser (_:cs) = parser cs

alphabeticalValue :: String -> Int
alphabeticalValue = sum . map charValue
  where charValue c = (ord . toLower $ c) - ord 'a' + 1

sumNameScores :: [String] -> Int
sumNameScores = sum . zipWith (*) [1..] . map alphabeticalValue . sort

sumNameScoresFile :: String -> IO Int
sumNameScoresFile file = sumNameScores . parser <$> readFile file
