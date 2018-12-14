{-# LANGUAGE OverloadedLists #-}
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.List
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

-- stopCondition :: Int -> RecipeList -> Bool
-- stopCondition n rl = isSuffixOf 

isApproxSuffixOf :: Eq a => [a] -> S.Seq a -> Bool
isApproxSuffixOf sfx seq =  not (sfx `isInfixOf` (toList b))
  where b = snd $ S.splitAt (seqLength - sfxLength - 2) seq
        seqLength = S.length seq
        sfxLength = length sfx

-- Actual answer is the one given by this, since this is one of the cases where the searched for string is not RIGHT at the end of the sequence
solution puzzleInput = (\x -> x - (length suffix)) . S.length . head . dropWhile (isApproxSuffixOf suffix) . map (\(x,_,_) -> x) $ iterate step ([3,7],0,1)
  where suffix = numToDigitList puzzleInput

main = print "not relevant"
