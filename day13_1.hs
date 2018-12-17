import qualified Data.Map.Strict as M
import Data.Maybe
import Prelude hiding (Left,Right)

type Position = (Int,Int)
data Direction = Up | Down | Left | Right deriving Show
data DirectionChoice = Port | Straight | Starboard deriving Show
data Cart = Cart { position :: Position
                 , currentDirection :: Direction
                 , directions :: [DirectionChoice]
                 } deriving Show
type CaveMap = M.Map Position Char

type CaveState = [Cart]

generateMap :: IO () -- (CaveMap, [Cart])
generateMap = do
  lines <- zip [0..] . lines <$> readFile "input_day13.txt"
  print "biep"

moveCart :: CaveMap -> Cart -> Cart
moveCart cm (Cart (x,y) direction dirChoices) = Cart (newPos direction) nextDir nextChoices
  where newPos Up    = (x,y+1)
        newPos Down  = (x,y-1)
        newPos Left  = (x-1,y)
        newPos Right = (x+1,y)
        tileType = fromJust $ M.lookup (newPos direction) cm
        (nextDir, nextChoices) = determineNextDir tileType direction dirChoices

-- Yes this could be shorter
determineNextDir :: Char -> Direction -> [DirectionChoice] -> (Direction,[DirectionChoice])
determineNextDir '-' x ds = (x,ds)
determineNextDir '|' x ds = (x,ds)
determineNextDir '/' Up ds = (Right,ds)
determineNextDir '/' Down ds = (Left,ds)
determineNextDir '/' Left ds = (Down,ds)
determineNextDir '/' Right ds = (Up,ds)
determineNextDir '\\' Up ds = (Left,ds)
determineNextDir '\\' Down ds = (Right,ds)
determineNextDir '\\' Left ds = (Up,ds)
determineNextDir '\\' Right ds = (Down,ds)
determineNextDir '+' x (Straight:ds) = (x,ds)
determineNextDir '+' Up (Port:ds) = (Left,ds)
determineNextDir '+' Up (Starboard:ds) = (Right,ds)
determineNextDir '+' Left (Port:ds) = (Down,ds)
determineNextDir '+' Left (Starboard:ds) = (Up,ds)
determineNextDir '+' Down (Port:ds) = (Right,ds)
determineNextDir '+' Down (Starboard:ds) = (Left,ds)
determineNextDir '+' Right (Port:ds) = (Up,ds)
determineNextDir '+' Right (Starboard:ds) = (Down,ds)

stepCarts :: CaveMap -> CaveState -> CaveState
stepCarts cm = undefined

main = undefined

