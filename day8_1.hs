import Control.Monad.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec

type Parser a = ParsecT T.Text () Identity a

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
integer :: Parser Int
integer = read <$> many1 digit

node :: Parser Node
node = do
  numNodes <- integer <* space
  numMetadata <- integer <* space
  nodes <- count numNodes node
  metadata <- count numMetadata (integer <* (space <|> (eof *> pure ' ')))
  return $ Node (Header numNodes numMetadata) nodes metadata

data Node = Node { header :: Header
                 , subNodes :: [Node]
                 , metadata :: [Int]
                 } deriving (Show,Eq)
                 
data Header = Header { numNodes :: Int
                     , numMetadata :: Int
                     } deriving (Show,Eq)

sumMetadata :: Node -> Int
sumMetadata n = sum (map sumMetadata $ subNodes n) + sum (metadata n)

main = do
  Right node <- parse node "" <$> TIO.readFile "input_day8.txt"
  print $ sumMetadata node