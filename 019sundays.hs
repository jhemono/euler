module Sundays where
type Month = (Months, Int)
data Months = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Show,Eq,Ord,Enum,Bounded)
data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq,Show)

isLeap :: Int -> Bool
isLeap y = if y `mod` 100 == 0
           then y `mod` 400 == 0
           else y `mod` 4 == 0

daysInMonth :: Month -> Int
daysInMonth (m, y)
  | m `elem` [Jan , Mar , May , Jul , Aug , Oct , Dec] = 31
  | m `elem` [Apr , Jun , Sep , Nov] = 30
  | m == Feb = if isLeap y then 29 else 28

daysInYear :: Int -> Int
daysInYear y = daysIn [(m,y) | m <- months]
daysIn :: [Month] -> Int
daysIn = sum . map daysInMonth

truc :: Int
truc = snd machin
machin :: ([Days],Int)
machin = foldl (\(ds',n) m -> (drop (daysInMonth m) ds', if head ds' == Sun then 1 + n else n)) (ds,0) range
ds :: [Days]
ds = drop (daysIn [(m, 1900) | m <- months]) $ cycle days
days :: [Days]
days = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]
range :: [(Months, Int)]
range = [(m,y) | y <- [1901..2000], m <- months]
months :: [Months]
months = [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]


