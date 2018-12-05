import Data.Char
import qualified Data.IntMap.Strict as M
import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.Set as S

main = do
  dataTree <- M.fromList . (zip [1..]) <$> readFile "input_day5.txt"
  let allElements = S.toList $ foldl' (flip S.insert) S.empty $ map toUpper (M.elems dataTree)
  print . minimum $ map (reducedSizeWithoutElement dataTree) allElements

-- Some utility functions to make it the subsequent code shorter
compareKeys :: M.IntMap Char -> Int -> Int -> Bool
compareKeys map l r = sameLetter && differentCase
  where leftValue  = M.lookup l map
        rightValue = M.lookup r map
        sameLetter = (toUpper <$> leftValue) == (toUpper <$> rightValue)
        differentCase = (isUpper <$> leftValue) /= (isUpper <$> rightValue)

reduce :: M.IntMap Char -> Maybe Int -> Maybe Int -> M.IntMap Char
reduce m left Nothing = m -- We're at the right side of the map 
reduce m Nothing (Just right) = reduce m (Just right) (fst <$> M.lookupGT right m) --shift up one to the right because we're falling off the left side
reduce m (Just left) (Just right)
  | compareKeys m left right = reduce newMap nextSmallestKey nextBiggestKey
  | otherwise = reduce m (Just right) (fst <$> M.lookupGT right m)
    where nextBiggestKey  = fst <$> M.lookupGT right m
          nextSmallestKey = fst <$> M.lookupLT left  m
          newMap = M.delete right (M.delete left m) -- map with the deleteable elements 'reduced's

reducedSizeWithoutElement :: M.IntMap Char -> Char -> Int
reducedSizeWithoutElement m charToRemove = M.size $ reduce filteredMap (Just 1) (Just 2)
  where filteredMap = M.filter (\x -> (toUpper x) /= charToRemove) m
