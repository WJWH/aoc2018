import Control.Applicative
import Data.List
import qualified Data.Set as S

inputFile = "day1_data.txt"

-- assumes that there is no empty input
readSignedInteger :: String -> Integer
readSignedInteger s = case head s of
  '+' -> read (tail s)
  _   -> read s

stringToListOfIntegers :: String -> [Integer]
stringToListOfIntegers str = map readSignedInteger $ lines str

getCalibrationsList :: IO [Integer]
getCalibrationsList = stringToListOfIntegers <$> readFile inputFile

-- Run this on an infinite list, obtained by `cycle` from the list of calibrations
scanForDoubleFrequencies :: S.Set Integer -> Integer -> [Integer] -> Integer
scanForDoubleFrequencies alreadySeen acc (f:fs)
  | S.member newFrequency alreadySeen = newFrequency
  | otherwise = scanForDoubleFrequencies (S.insert newFrequency alreadySeen) newFrequency fs
    where newFrequency = acc + f

solution = do
  calibrations <- getCalibrationsList
  print $ scanForDoubleFrequencies S.empty 0 $ cycle calibrations
