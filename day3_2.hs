{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Control.Applicative ((<$>))
import Control.Monad.Identity
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.IO as TIO
import Text.Parsec

-- Let's make those type signatures a bit shorter
-- For our purposes: a parser of type a is a ParsecT that take Texts, has no state, the Identity
-- monad (which does nothing) as underlying monad and returns values of type a if succesful
type Parser a = ParsecT T.Text () Identity a

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
integer :: Stream s m Char => ParsecT s u m Integer
integer = read <$> many1 digit

-- Claims are shaped like this: "#14 @ 953,65: 29x23". The first number does not mean anything.
data Claim = Claim Integer Integer Integer Integer Integer deriving (Show,Eq)

-- Let's make some parsers!
claim :: Parser Claim
claim = do
  char '#'
  claimId <- integer
  string " @ "
  x <- integer
  char ','
  y <- integer
  string ": "
  width <- integer
  char 'x'
  height <- integer
  return $ Claim claimId x y width height

claimsFileParser :: Parser [Claim] --Stream s m Char => ParsecT s u m [Claim]
claimsFileParser = do
  claims <- endBy claim endOfLine
  eof
  return claims

-- For the solution we "expand" the claims into the grid they represent with a list comprehension
-- and then increment all those values into a Map representing the sheet of cloth.
tilesTouched :: Claim -> [(Integer,Integer)]
tilesTouched (Claim _ x y width height) = [(a,b) | a <- [(x+1)..(x+width)], b <- [(y+1)..(y+height)]]

emptySheet = M.empty
fillSheet :: [Claim] -> M.Map (Integer,Integer) Integer
fillSheet claims = foldl' foldfunc emptySheet $ concatMap tilesTouched claims
  where foldfunc sheet key = M.insertWith (+) key 1 sheet

-- According to the problem specification, the claim we need to find is the only claim that has not had
-- a single one of its tiles also claimed by another claim. Said in a different way, all of its tiles have
-- only been claimed one time.
findGoodClaim :: M.Map (Integer,Integer) Integer -> [Claim] -> Claim
findGoodClaim filledSheet [] = error "No claim found"
findGoodClaim filledSheet (c:claims)
  | all testTile tiles = c
  | otherwise = findGoodClaim filledSheet claims
    where tiles = tilesTouched c
          testTile tile = (M.findWithDefault 0 tile filledSheet) == 1

claimId :: Claim -> Integer
claimId (Claim id _ _ _ _) = id

main = do
  Right claims <- parse claimsFileParser "input" <$> TIO.readFile "input_day3.txt"
  let filledSheet = fillSheet claims
  print . claimId $ findGoodClaim filledSheet claims
