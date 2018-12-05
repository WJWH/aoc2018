import Data.Char

main = do
  input <- readFile "input_day5.txt"
  print . length $ reduce "" input

reduce :: String -> String -> String
reduce left "" = left -- We're at the right side of the map 
reduce ""  (r:rights) = reduce "r" rights --shift up one to the right because we're falling off the left side
reduce (l:lefts) (r:rights)
  | reduceable = reduce lefts rights -- remove the elements and look again
  | otherwise = reduce (r:l:lefts) rights -- look at the next pair
    where reduceable = sameLetter && differentCase
          sameLetter = (toUpper l) == (toUpper r)
          differentCase = (isUpper l) /= (isUpper r)
