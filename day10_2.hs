import Control.Monad.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Data.List

type Parser a = ParsecT T.Text () Identity a

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
-- Little bit more sophisticated to also parse negative numbers
integer :: Parser Int
integer = read <$> (plus <|> minus <|> number)
  where plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit

-- A single point is defined like this in the input file:
-- position=< 9,  1> velocity=< 0,  2>
data Point = Point { x :: Int 
                   , y :: Int 
                   , dx :: Int
                   , dy :: Int
                   } deriving (Show)

point :: Parser Point
point = do
  string "position=<"
  x <- spaces *> integer
  char ','
  y <- spaces *> integer
  string "> velocity=<"
  dx <- spaces *> integer
  char ','
  dy <- spaces *> integer
  char '>'
  return $ Point x y dx dy

pointsFile :: Parser [Point]
pointsFile = do
  points <- point `sepBy` endOfLine
  eof
  return points

pointAtTime :: Int -> Point -> Point
pointAtTime t (Point x y dx dy) = Point (x + (t*dx)) (y + (t*dy)) dx dy

yHeight :: [Point] -> Int
yHeight points = (maximum yCoords) - (minimum yCoords)
  where yCoords = map y points

heightAtTime :: [Point] -> Int -> Int
heightAtTime points t = yHeight $ map (pointAtTime t) points

findTimeOfMinimumHeight :: [Point] -> Int -> Int
findTimeOfMinimumHeight points startTime
  | (heightAtTime points (startTime + 1)) > (heightAtTime points startTime) = startTime
  | otherwise = findTimeOfMinimumHeight points (startTime + 1)

prettyPrintPoints :: [Point] -> [String]
prettyPrintPoints points = map printOneLine [(maxY-minY),((maxY-minY-1))..0]
  where xCoords = map x points
        yCoords = map y points
        maxX = maximum xCoords
        minX = minimum xCoords
        maxY = maximum yCoords
        minY = minimum yCoords
        pointCoords = map (\(Point x y _ _) -> (x,y)) correctedPoints
        correctedPoints = map (\(Point x y dx dy) -> Point (x - minX) (y - minY) dx dy) points
        printOneLine :: Int -> String
        printOneLine ycoord = map (\xcoord -> if elem (xcoord,ycoord) pointCoords then 'X' else ' ') [0..(maxX-minX)]


main = do
  Right points <- parse pointsFile "" <$> TIO.readFile "input_day10.txt"
  print $ findTimeOfMinimumHeight points 1
  
