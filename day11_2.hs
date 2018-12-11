import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord

gridSerialNumber = 18

powercellLevel :: Int -> (Int,Int) -> Int
powercellLevel serial (x,y) = hundredsDigit - 5
  where rackId = x + 10
        initialPower = (rackId * y) + serial
        secondPower = initialPower * rackId
        hundredsDigit = (secondPower `div` 100) `mod` 10

wholeGrid :: M.Map (Int,Int) Int
wholeGrid = foldl' foldfunc M.empty $ [(x,y) | x <- [1..300], y <- [1..300]]
  where foldfunc m coords = M.insert coords (powercellLevel gridSerialNumber coords) m

areaValue :: M.Map (Int,Int) Int -> (Int,Int,Int) -> Int
areaValue map (x,y,gridSize) = foldl' foldfunc 0 $ [(x,y) | x <- [x..(x+gridSize-1)], y <- [y..(y+gridSize-1)]]
  where foldfunc acc coords = acc + (fromJust $ M.lookup coords map)

main = do
  let values = map (\coords -> (coords, areaValue wholeGrid coords)) $ [(x,y,gs) | gs <- [1..300], x <- [1..(300 - (gs - 1))], y <- [1..(300 - (gs - 1))]]
  print $ maximumBy (comparing snd) values
