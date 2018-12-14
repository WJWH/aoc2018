{-# LANGUAGE OverloadedLists #-}
import Data.Char
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as S

type RecipeList = S.Seq Int

numToDigitList :: Int -> [Int]
numToDigitList x = map digitToInt $ show x

step :: (RecipeList,Int,Int) -> (RecipeList,Int,Int)
step (rl, firstElfPosition, secondElfPosition) = (newRecipeList,newFirstElfPosition,newSecondElfPosition)
  where firstElfRecipe  = S.index rl firstElfPosition
        secondElfRecipe = S.index rl secondElfPosition
        newRecipes = S.fromList $ numToDigitList (firstElfRecipe + secondElfRecipe)
        newRecipeList = rl S.>< newRecipes
        newFirstElfPosition = (firstElfPosition + firstElfRecipe + 1) `mod` (S.length newRecipeList)
        newSecondElfPosition = (secondElfPosition + secondElfRecipe + 1) `mod` (S.length newRecipeList)

stopCondition :: Int -> (RecipeList, Int, Int) -> Bool
stopCondition n (rl,_,_) = (S.length rl) <= (n+10)

solution puzzleInput = map (intToDigit) . toList . S.take 10 . S.drop puzzleInput . (\(x,y,z) -> x) . head . dropWhile (stopCondition puzzleInput) $ iterate step ([3,7],0,1)

main = print "not relevant"
