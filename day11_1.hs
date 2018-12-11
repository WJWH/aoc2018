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

areaValue :: M.Map (Int,Int) Int -> (Int,Int) -> Int
areaValue map (x,y) = foldl' foldfunc 0 $ [(x,y) | x <- [x..(x+2)], y <- [y..(y+2)]]
  where foldfunc acc coords = acc + (fromJust $ M.lookup coords map)

main = do
  let values = map (\coords -> (coords, areaValue wholeGrid coords)) $ [(x,y) | x <- [1..298], y <- [1..298]]
  print $ maximumBy (comparing snd) values
