import Control.Applicative
import Data.List

inputFile = "day1_data.txt"

-- assumes that there is no empty input
readSignedInteger :: String -> Integer
readSignedInteger s = case head s of
  '+' -> read (tail s)
  _   -> read s

stringToListOfIntegers :: String -> [Integer]
stringToListOfIntegers str = map readSignedInteger $ lines str

main = do
  calibrationsList <- stringToListOfIntegers <$> readFile inputFile
  print $ foldl' (+) 0 calibrationsList 