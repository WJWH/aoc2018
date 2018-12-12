import Control.Monad.Identity
import qualified Data.IntMap.Strict as M
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec

type Parser a = ParsecT T.Text () Identity a

data Rule = Rule { conditions :: [Bool]
                 , outcome :: Bool 
                 } deriving Show

type CaveState = M.IntMap Bool
type Position = Int

rule :: Parser Rule
rule = do
  otherpots <- count 5 (oneOf "#.")
  string " => "
  outcome <- (oneOf "#.")
  return $ Rule (map (== '#') otherpots) (outcome == '#')

cave :: Parser CaveState
cave = do
  string "initial state: "
  initialPlants <- many (oneOf "#.")
  endOfLine
  return . M.fromList $ zip [0..] (map (== '#') initialPlants)

inputFile :: Parser (CaveState, [Rule])
inputFile = do
  cavestate <- cave
  endOfLine -- empty line between state and rules
  rules <- rule `sepBy` endOfLine
  eof
  return $ (cavestate, rules)

applyRule :: CaveState -> Position -> Rule -> Bool
applyRule prevState pos rule = and $ zipWith (==) (conditions rule) posAndSurroundings
  where posAndSurroundings = map (\k -> M.findWithDefault False k prevState) [pos-2,pos-1,pos,pos+1,pos+2]

applyAllRules :: CaveState -> Position -> [Rule] -> Bool
applyAllRules prevstate pos rules = or $ map (applyRule prevstate pos) rules

stepCave :: [Rule] -> CaveState -> CaveState
stepCave rules prevstate = M.fromList $ map (\pos -> (pos,applyAllRules prevstate pos rules)) [minKey..maxKey]
  where minKey = (fst $ M.findMin prevstate) - 2
        maxKey = (fst $ M.findMax prevstate) + 2

showCave :: CaveState -> String
showCave cs = map (\(_,b) -> if b then '#' else '.') $ M.toList cs

sumCave :: CaveState -> Int
sumCave cave = sum . map fst . filter snd $ M.toList cave

-- Prints differences between generations
debugMain = do
  Right (initialCave, rules) <-  parse inputFile "" <$> TIO.readFile "input_day12.txt"
  let filteredRules = filter (\r -> outcome r) rules
  let caveSums = map sumCave $ iterate (stepCave filteredRules) initialCave
  mapM_ print . (take 150) $ zipWith (-) caveSums (tail caveSums)

-- Looking at the outputs over time it becomes clear that after a while, the sum increases by 46 every generation
-- so we can just multiply 50 billion by 46 to get a closed form solution (with some corrections for the first few
-- generations where the pattern is not yet established). Looking at the output of debugMain by generation 150
-- the pattern has stabilized.
main = do
  Right (initialCave, rules) <-  parse inputFile "" <$> TIO.readFile "input_day12.txt"
  let filteredRules = filter (\r -> outcome r) rules
  let caveSums = map sumCave $ iterate (stepCave filteredRules) initialCave
  let sum150thGeneration = head . (drop 150) $ caveSums
  print $ sum150thGeneration + (50000000000 - 150) * 46
