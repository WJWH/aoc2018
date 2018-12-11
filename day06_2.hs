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
  print . length . filter (\coord -> (sumOfDistances coordinates coord) < 10000) $ relevantGrid coordinates


-- Takes a list of coordinates and returns all the tuples within the "bounding square"
relevantGrid :: [Coordinate] -> [Coordinate]
relevantGrid coords = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]
  where xMin = fst $ minimumBy (comparing fst) coords
        xMax = fst $ maximumBy (comparing fst) coords
        yMin = snd $ minimumBy (comparing snd) coords
        yMax = snd $ maximumBy (comparing snd) coords

sumOfDistances :: [Coordinate] -> Coordinate -> Int
sumOfDistances coords gridpoint = sum $ map (manhattanDistance gridpoint) coords