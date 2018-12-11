import Control.Applicative
import Data.List

countLetterOccurrences :: String -> [Int]
countLetterOccurrences = (map length) . group . sort

hasTwoOccurrences   :: [Int] -> Bool
hasTwoOccurrences   = elem 2
hasThreeOccurrences :: [Int] -> Bool
hasThreeOccurrences = elem 3

countOccurrencesOfTwoAndThree :: String -> (Bool,Bool)
countOccurrencesOfTwoAndThree str = (hasTwoOccurrences counts, hasThreeOccurrences counts)
  where counts = countLetterOccurrences str

sumOccurrences :: [String] -> (Int,Int)
sumOccurrences boxIds = foldl' sumfunc (0,0) $ map countOccurrencesOfTwoAndThree boxIds
  where sumfunc (a,b) (c,d) = (if c then succ a else a, if d then succ b else b)

getAllBoxIds = lines <$> readFile "day2_data.txt"

solution = do
  allBoxIds <- getAllBoxIds
  let (twos, threes) = sumOccurrences allBoxIds
  print $ twos * threes
