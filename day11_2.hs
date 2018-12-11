-- Warning: even with all the strictness forcing to prevent from running out of stack space,
-- this still runs slow enough to waarant compiling with -O2 instead of just running it in
-- ghci as usual. I have some ideas to make it faster but probably won't get around to those.

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord

gridSerialNumber = 9005

powercellLevel :: Int -> (Int,Int) -> Int
powercellLevel serial (x,y) = hundredsDigit - 5
  where rackId = x + 10
        initialPower = (rackId * y) + serial
        secondPower = initialPower * rackId
        hundredsDigit = (secondPower `div` 100) `mod` 10

wholeGrid :: M.Map (Int,Int) Int
wholeGrid = foldl' foldfunc M.empty $ [(x,y) | x <- [1..300], y <- [1..300]]
  where foldfunc m coords = M.insert coords (powercellLevel gridSerialNumber coords) m

type DynMap = M.Map (Int,Int,Int) Int

sumLookup :: [(Int,Int)] -> Int
sumLookup tuples = sum $ map (\t -> fromJust $ M.lookup t wholeGrid) tuples

-- Doing it naively was fine for section one, but gets a little bit out of hand for doing it for all
-- possible grid sizes. So, since there are clearly overlapping subproblems here, we turn to dynamic
-- programming to solve this faster.
areaValue :: (Int,Int,Int) -> State DynMap Int
areaValue (x,y,gridSize)
  | gridSize == 1 = return . fromJust $ M.lookup (x,y) wholeGrid
  | otherwise = do
    s <- get
    case M.lookup (x,y,gridSize) s of
      Just v -> return v
      Nothing -> do
        inner <- areaValue (x,y,gridSize - 1)
        let rest = sumLookup [(a,b) | a <- [x..x+gridSize-1], b <- [y+gridSize-1]] + sumLookup [(a,b) | a <- [x+gridSize-1], b <- [y..y+gridSize-1]] - (fromJust $ M.lookup (x+gridSize-1, y+gridSize-1) wholeGrid)
        let answer = force $ inner + rest
        modify' (M.insert (x,y,gridSize) answer)
        return answer

calculateNewAreas :: Int -> State DynMap [((Int,Int,Int),Int)]
calculateNewAreas gs = forM [(x,y,gs) | x <- [1..(300 - (gs - 1))], y <- [1..(300 - (gs - 1))]] $ \coords -> do
  area <- areaValue coords
  return (coords, area)

solveOneGridSize :: DynMap -> Int -> IO DynMap
solveOneGridSize dm size = do
  let newDynmap = force $ execState (calculateNewAreas size) dm
  print $ "Finished iteration " ++ (show size)
  print $ M.size newDynmap
  return newDynmap

main = do
  endState <- foldM solveOneGridSize M.empty [1..300]
  print $ maximumBy (comparing snd) $ M.toList endState
