import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

gridsize = 50

testInput :: [String]
testInput =  [".#.#...|#.",
              ".....#|##|",
              ".|..|...#.",
              "..|#.....#",
              "#.#|||#|#|",
              "...#.||...",
              ".|....|...",
              "||...#|.#|",
              "|.||||..|.",
              "...#.|..|."]

input :: [String]
input = 
 ["............|#.|..|..|#.#.....#.#||.#.#...#.|#....",
  "|...|.##...#.|..||#.##..###...|..#||#|.....|..|...",
  "#.|.#..||#..|.|..|..|.....##....#.##||..#|.|.....|",
  ".##||.|..#.......###||.#.#.|..##.#|..#...#.....#.|",
  "....#|##|.|.|...|||..|...#.......|#..|#|.|..|#|...",
  "|....|...#.....|#||...#.....#....#|....|.....|....",
  ".#||..|##.##|..||.#.....#...|##..|#...#.#.|.#.|##.",
  "..#|##..|......#|..#.|#.##....|.....|..#.#...|...#",
  "#....#||.|..|..#..#.|........|.|.....||||.|..|....",
  "|.#|.|...#..|||#.##|#.........|...#..#.|#.....||.|",
  ".#.##.........|...|.|.#..|.|##...|.|#.||....||.#..",
  "#.#.....##|.#|..###..|........|.....|.....|#.||.#|",
  "|..||....#..###.|..#..#.#.......##.#.|.#..|.|.#|.#",
  "|...##.#..#|..|..|......#..#......|..|..#.....|.||",
  ".......||#|.|...##.#.##..|#.|.|#|....##||...#.#.|#",
  "..|###..|.....#|.|.|.|....|....#.#..##|...||......",
  "|.|#|.#|||#.|#|#..|...#.|..|..#.#...#..#.#.#.#..#.",
  "...#.###...#............#..|#...||#|..#..|###...|.",
  ".|......|...#....#.......|...##.|.|..##.#...#||||.",
  "#...|.#|.|.......|##.#...|#.|.........#.|.|##.||..",
  "|.|#..#.||#|.|..|..|#........#..#.....#.#..#.#.|..",
  "#..|.|...#|#..#|....|...|..|#|....|..##|#|......||",
  "....||...|....|...#|..|.#|.....#.|..||..|...|.....",
  "#......#.#..##.|#.|||.|#||||.|...#...|#.|#...||...",
  ".|...|...|.#..|#.|####|.......|.#||.|..|..|..#.#..",
  ".#.|##.......|||...#..#.#....#..|..#.###........#.",
  "#.#..|.##..|...#....#.#.|...|.#.|.....|.|......|#.",
  "#.||.#......#.....##.|...|#..#.#|..#..|..#..#.#.|.",
  "|...#|#...|....#|##..|....||....#.||||.|#...|.....",
  ".|...|.|...|...|..|..#|...||||#.#...|.#....##...|.",
  ".|.#...#|...#||.#....|##.#|..#|||......|.|..||...|",
  "|#...|.|..|.#...#..||......|.|.##.....||#|#.#|..|.",
  "..#.|.###......|.|.###|...#.#|#||....#..|....|#..|",
  "|....|.|.|#.|.#..|.|.#.#.||..##|..|.||#|..|...||..",
  "#.|#.|....|#|#.|..#...|...|.#.#.....#...#|###.#..#",
  "...###|.#....#..#..|.|....#|.|.###.|...|..|.##.|#.",
  "...#....#|.#|#.|#.|#|.|...|.....#..|......|.||###.",
  ".||#|.|.#.|..|||.#.|..#.....#..|..###||#|||...#..#",
  "###||#........#|####.|..|#....#|.|.|.|#|.|.#|.##.|",
  "|..#.#|....#....|.#.|..|...|..#..|....#.###.|.#|#.",
  ".#..##|##...#.....#||||#.#...||..|........|#......",
  ".#.|||#.|...#.#.|.|..|......#.#....##|..|#||....##",
  "...##..|....|#.||......##...#..||##.##.#..#.|##|..",
  "..#.||...|#..||#....#|.##.#|.........#...||.|#|#|#",
  "#.......|.|#...##.#..#|#.|.|.|.##|...#...|...#.#..",
  ".....||......|...#..|......||.|#..#....##|...#...#",
  "..#|.#....|.......#.#..##......|.#.|..|..#..#.#.||",
  ".|.....#...#......#...#..||||#|.....||..|....#|...",
  "#...#.||..#......|.##|#.....|..|..|..|.......|.||.",
  "..|#.|......||.|.||.|##|||.|..#.......#|||.#.|.|#|"]

data PlotType = Lumberyard | Trees | OpenLand deriving (Show,Eq)
type LandMap = M.Map (Int,Int) PlotType

charToPlot :: Char -> PlotType
charToPlot c = case c of
  '.' -> OpenLand
  '|' -> Trees
  '#' -> Lumberyard

readLineToNumberedPlots :: String -> [(Int,PlotType)]
readLineToNumberedPlots str = zip [1..] $ map charToPlot str

initialLandmap :: LandMap
initialLandmap = M.fromList . concat $ map tupleConverter $ zip [1..] $ (map readLineToNumberedPlots input)
  where tupleConverter (x, plots) = [((x,y),plot) | (y,plot) <- plots]

step :: LandMap -> LandMap
step oldstate = M.fromList $ map singleCell allCoordinates 
  where singleCell coords = case fromJust $ M.lookup coords oldstate of
          Lumberyard -> (coords, if (((length . filter (== Lumberyard) $ surroundingCoordinatesCellContents coords) >= 1) && ((length . filter (== Trees) $ surroundingCoordinatesCellContents coords) >= 1)) then Lumberyard else OpenLand)
          Trees -> (coords, if (length . filter (== Lumberyard) $ surroundingCoordinatesCellContents coords) >= 3 then Lumberyard else Trees)
          OpenLand -> (coords, if (length . filter (== Trees) $ surroundingCoordinatesCellContents coords) >= 3 then Trees else OpenLand)
        surroundingCoordinatesCellContents :: (Int,Int) -> [PlotType]
        surroundingCoordinatesCellContents (x,y) = catMaybes [
          M.lookup (x-1,y-1) oldstate,
          M.lookup (x  ,y-1) oldstate,
          M.lookup (x+1,y-1) oldstate,
          M.lookup (x-1,y  ) oldstate,
          M.lookup (x+1,y  ) oldstate,
          M.lookup (x-1,y+1) oldstate,
          M.lookup (x  ,y+1) oldstate,
          M.lookup (x+1,y+1) oldstate]
        allCoordinates = [(x,y) | x <- [1..gridsize], y <- [1..gridsize]]

tenthIteration :: LandMap
tenthIteration = (iterate step initialLandmap) !! 10

score :: LandMap -> Int
score lm = ((length $ filter (== Trees) elements) * (length $ filter (== Lumberyard) elements) )
  where elements = M.elems lm

printLandmap :: LandMap -> IO ()
printLandmap lm = forM_ [1..gridsize] $ \x -> do
  forM_ [1..gridsize] $ \y -> do
    putChar $ case fromJust $ M.lookup (x,y) lm of
      Trees -> '|'
      Lumberyard -> '#'
      OpenLand -> '.'
  putChar '\n'
  
firstProblem = score tenthIteration

allScores = map score $ iterate step initialLandmap
allScoreDifferences = zipWith (-) allScores (tail allScores)
-- it eventually starts cycling, the start of the cycle seems to be difference -3810 and the first time it happens is after 552, then every next one is after 855? 
-- length $ takeWhile (/= -3810) allScoreDifferences == 513
-- length $ takeWhile (/= -3810) $ drop 514 allScoreDifferences == 27
-- So, 513 to stabilize and then cycles of length 27?
--(1000000000 - 513) `div` 27 -- == 37037018
--The cycle is: 
observedCycle = [-3810,1858,-4615,4530,3533,4680,703,1921,1913,4971,-3378,318,-2091,-243,-1380,61,-5626,9719,3230,1662,-1264,1149,-4605,-3116,-3436,-4116,-3453,885]
-- (37037018 * 27) + 513 == 999999999, but 1858 is too low?
-- wait, cycle has length 28 instead of 27
-- (35714267 * 28) + 513 = 999999989, dwz je moet de 11e hebben
-- Oh Doi, en ook niet de difference maar de absolute value
secondProblem = allScores !! (513 + 11)
