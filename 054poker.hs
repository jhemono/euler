import Data.List
import Control.Arrow ((&&&), (***))
import Data.Monoid
import Data.Ord
import Data.Maybe

data Rank = HighCard Value | OnePair Value | TwoPairs Value Value
          | ThreeOfKind Value | Straight Value | Flush Value
          | FullHouse Value Value | FourOfKind Value | StraightFlush Value
          | RoyalFlush
  deriving (Eq, Ord)

data Value = Deuce | Trey | Four | Five | Six | Seven | Eight | Nine | Ten
           | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq)

data Card = Card {value :: Value, suit :: Suit}

newtype Hand = Hand {hand :: [Card]}
instance Eq Hand

-- Requires a list of value sorted in descending order
allConsecutive :: [Value] -> Maybe Value
allConsecutive vs =
  do
    let (v0:_) = vs
    if aux vs
       then return v0
       else Nothing
  where
    aux (v0' : (vs' @ (v1':_))) = v1' /= v0' && succ v1' == v0' && aux vs'
    aux _ = True

allSame :: Eq a => [a] -> Bool
allSame (x:xs) = all (== x) xs
allSame [] = True

groupV :: [Value] -> [(Value,Int)]
groupV = sortBy (flip (comparing snd)) . map (head &&& length) . group

rank :: Hand -> Rank
rank (Hand h) =
  case (allSame suits, allConsecutive values, groupV values) of
       (_, Just Ace, _) -> RoyalFlush
       (True, Just v, _) -> StraightFlush v
       (_, _, (v,4):_)-> FourOfKind v
       (_, _, (v0,3):(v1,2):_) -> FullHouse (max v0 v1) (min v0 v1)
       (True, _, _) -> Flush (head values)
       (_, Just v, _) -> Straight v
       (_, _, (v,3):_) -> ThreeOfKind v
       (_, _, (v0,2):(v1,2):_) -> TwoPairs (max v0 v1) (min v0 v1)
       (_, _, (v,2):_) -> OnePair v
       (_, _, _) -> HighCard (head values)
  where
    suits = map suit h
    values = sortBy (flip compare) . map value $ h

readTwoHand :: (Hand -> Hand -> r) -> String -> r
readTwoHand k s =
  uncurry k . (Hand *** Hand) . splitAt 5 . map readCard . words $ s where
  readCard [vc,sc] = Card (readValue vc) (readSuit sc)
  readSuit = lookup' "HCSD" [Hearts, Clubs, Spades, Diamonds]
  readValue = lookup' "23456789TJQKA" [Deuce .. Ace]
  lookup' keys values k = fromJust $ lookup k $ zip keys values

instance Ord Hand where
  compare h1 h2 =
    rank h1 `compare` rank h2 <> aux h1 `compare` aux h2
    where
      aux = sortBy (flip compare) . map (value) . hand

main :: IO ()
main =
  do ls <- fmap lines $ readFile "p054_poker.txt"
     let answer = length . filter (readTwoHand (>)) $ ls
     print answer
