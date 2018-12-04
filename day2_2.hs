import Control.Applicative
import Data.List

getAllBoxIds = lines <$> readFile "day2_data.txt"

findNumberOfDifferingCharacters :: String -> String -> Int
findNumberOfDifferingCharacters a b = foldl' foldfunc 0 $ zip a b
  where foldfunc acc (x,y) = if x /= y then succ acc else acc

-- You don't have to scan previous boxes, since they will already have scanned YOU
scanAllNextBoxes :: String -> [String] -> Maybe String
scanAllNextBoxes str [] = Nothing
scanAllNextBoxes str (x:xs)
  | difs == 1 = Just $ intersect str x
  | otherwise = scanAllNextBoxes str xs
    where difs = findNumberOfDifferingCharacters str x

-- No maybe here, since we know there is at least (exactly?) one pair
scanAllBoxes :: [String] -> String
scanAllBoxes (b:boxids) = case scanAllNextBoxes b boxids of
  Just answer -> answer
  Nothing -> scanAllBoxes boxids

solution = do
  allBoxIds <- getAllBoxIds
  print $ scanAllBoxes allBoxIds
