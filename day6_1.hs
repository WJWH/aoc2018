import Control.Monad.Identity
import Data.List
import Data.Ord
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec

type Coordinate = (Int,Int)
-- reuse boilerplate for parser:
type Parser a = ParsecT T.Text () Identity a

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
integer :: Parser Int
integer = read <$> many1 digit

coordinate :: Parser Coordinate
coordinate = (,) <$> (integer <* string ", ") <*> integer

coordinateFileParser :: Parser [Coordinate]
coordinateFileParser = do
  coordinates <- sepBy coordinate endOfLine
  eof
  return $ coordinates

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

main = do
  Right coordinates <- parse coordinateFileParser "" <$> TIO.readFile "input_day6.txt"
  let filledGrid = fillGrid coordinates
  mapM print $ (sortBy (comparing snd)) . M.toList $ countClosestPoints filledGrid
  -- actually we need the second largest because the largest is actually infinite, I can't be bothered
  -- to find out how to detect this edge case :p

-- Takes a list of coordinates and returns all the tuples within the "bounding square"
relevantGrid :: [Coordinate] -> [Coordinate]
relevantGrid coords = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]
  where xMin = fst $ minimumBy (comparing fst) coords
        xMax = fst $ maximumBy (comparing fst) coords
        yMin = snd $ minimumBy (comparing snd) coords
        yMax = snd $ maximumBy (comparing snd) coords

-- For EACH of the relevant grid points, compute the distance to each of the coordinates?
fillGrid :: [Coordinate] -> M.Map Coordinate (Maybe Coordinate)
fillGrid coords = foldl' foldfunc M.empty $ relevantGrid coords
  where foldfunc m gridpoint = M.insert gridpoint (closestPoint coords gridpoint) m

-- it may be closest to multiple points, in which case we return Nothing
closestPoint :: [Coordinate] -> Coordinate -> Maybe Coordinate
closestPoint coords gridPoint = if (firstTwoEqual $ map snd $ take 2 distances) then Nothing else Just . fst . head $ distances
  where distances = sortBy (comparing snd) $ map (\coord -> (coord, manhattanDistance coord gridPoint)) coords
        firstTwoEqual [a,b] = a == b

countClosestPoints :: M.Map Coordinate (Maybe Coordinate) -> M.Map Coordinate Int
countClosestPoints filledMap = foldl' foldfunc M.empty $ M.toList filledMap
  where foldfunc m (k,v) = if isJust v then M.insertWith (+) (fromJust v) 1 m else m

filterOutsidePoints :: [Coordinate] -> [(Coordinate, Int)] -> [(Coordinate, Int)]
filterOutsidePoints coords counts = filter (\((x,y),_) -> x > xMin && x < xMax && y > yMin && y < yMax) counts 
  where xMin = fst $ minimumBy (comparing fst) coords
        xMax = fst $ maximumBy (comparing fst) coords
        yMin = snd $ minimumBy (comparing snd) coords
        yMax = snd $ maximumBy (comparing snd) coords
