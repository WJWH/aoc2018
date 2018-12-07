import Control.Monad.Identity
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec

type Parser a = ParsecT T.Text () Identity a

type DependencyGraph = M.Map Char [Char]
type AvailableSteps = H.MinHeap Char
type RunningTasks = [(Char,Int)] -- (task, end time)
emptyHeap :: AvailableSteps
emptyHeap = H.empty

type Dependency = (Char,Char)

dependency :: Parser Dependency
dependency = do
  string "Step "
  dependsOn <- anyChar
  string " must be finished before step "
  action <- anyChar
  string " can begin."
  return $ (dependsOn, action)

dependencyFileParser :: Parser [Dependency]
dependencyFileParser = do
  dependencies <- sepBy dependency endOfLine
  eof
  return $ dependencies

createDependencies :: [Dependency] -> DependencyGraph
createDependencies deps = foldr (\(d,a) m -> M.insertWith (++) d [a] m) M.empty deps

numDependencies :: [Dependency] -> M.Map Char Int
numDependencies = foldr (\(k,v) m -> M.insertWith (+) v 1 m) M.empty

filterSteps :: M.Map Char Int -> [Char] -> (M.Map Char Int,[Char])
filterSteps map options = (newMap, filter (\option -> M.lookup option newMap == Just 0) options)
  where newMap = foldr (M.adjust pred) map options

resolveGraph :: DependencyGraph -> M.Map Char Int -> AvailableSteps -> [Char] -> [Char]
resolveGraph deps depmap stepsAvailable stepsSoFar
  | H.null stepsAvailable = reverse stepsSoFar
  | otherwise = resolveGraph (M.delete nextStep deps) newDepCount (foldr H.insert newStepsAvailable newOptions) (nextStep : stepsSoFar)
    where Just (nextStep, newStepsAvailable) = H.view stepsAvailable
          possibleNewOptions = fromMaybe [] $ M.lookup nextStep deps
          (newDepCount, newOptions) = filterSteps depmap possibleNewOptions

stepResolveGraph :: DependencyGraph -> M.Map Char Int -> AvailableSteps -> RunningTasks -> Int -> Int
stepResolveGraph deps depmap stepsAvailable runningTasks secondsSoFar
  | null runningTasks = secondsSoFar -- we're done here
  | (length runningTasks) < 5 && (not $ H.null stepsAvailable) = stepResolveGraph deps depmap newStepsAvailable (newRunningTask : runningTasks) secondsSoFar
  | any (\(_,y) -> y == 0) runningTasks = stepResolveGraph deps newDepCount (foldr H.insert stepsAvailable newOptions) (filter (\z -> snd z /= 0) runningTasks) secondsSoFar
  | otherwise = stepResolveGraph deps depmap stepsAvailable (map (\(x,y) -> (x,y - 1)) runningTasks) (secondsSoFar + 1)
    where Just (nextStep, newStepsAvailable) = H.view stepsAvailable
          newRunningTask = (nextStep, taskDuration nextStep)
          finishedSteps = map fst $ filter (\z -> snd z == 0) runningTasks
          possibleNewOptions finishedStep = fromMaybe [] $ M.lookup finishedStep deps
          (newDepCount, newOptions) = filterSteps depmap $ concatMap possibleNewOptions finishedSteps

-- ?????

taskDuration :: Char -> Int
taskDuration c = (fromEnum c) - 64 -- actually `- 64 + 60`

main = do
  Right dependencies <- parse dependencyFileParser "" <$> TIO.readFile "input_day7.txt"
  let depMap = createDependencies dependencies
  let initialOptions = H.fromList . S.toList $ S.difference (S.fromList $ M.keys depMap) (S.fromList . concat $ M.elems depMap)
  let (prime, initialOptions') = fromJust $ H.view initialOptions
  print $ stepResolveGraph depMap (numDependencies dependencies) initialOptions' [(prime, taskDuration prime)] 0
