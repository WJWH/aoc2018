import qualified Data.Map.Strict as M

type Position = (Int,Int)
data Direction = Up | Down | Left | Right
data DirectionChoice = Port | Straight | Starboard deriving Show
data Cart = Cart { position :: Position
                 , directions :: [DirectionChoice]
                 } deriving Show
type CaveMap = M.Map Position Char

type CaveState = [Cart]

generateMap :: IO (CaveMap, [Cart])
generateMap = undefined

moveCart :: CaveMap -> Cart -> Cart
moveCart = undefined

stepCarts :: CaveMap -> CaveState -> CaveState
stepCarts = undefined

main = undefined

