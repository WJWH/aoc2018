{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Control.Applicative ((<$>))
import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.IO as TIO
import Text.Parsec

bigSheet = M.empty

-- Let's make those type signatures a bit shorter
-- For our purposes: a parser of type a is a ParsecT that take Texts, has no state, the Identity
-- monad (which does nothing) as underlying monad and returns values of type a if succesful
type Parser a = ParsecT T.Text () Identity a

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
integer :: Stream s m Char => ParsecT s u m Integer
integer = read <$> many1 digit

-- Claims are shaped like this: "#14 @ 953,65: 29x23". The first number does not mean anything.
data Claim = Claim Integer Integer Integer Integer deriving (Show,Eq)

-- Let's make some parsers!
claim :: Parser Claim
claim = do
  manyTill anyChar (string "@ ") -- ie: skip characters until you find an '@ ' since we don't care yet about the claim number
  x <- integer
  char ','
  y <- integer
  string ": "
  width <- integer
  char 'x'
  height <- integer
  return $ Claim x y width height

claimsFile :: Parser [Claim] --Stream s m Char => ParsecT s u m [Claim]
claimsFile = do
  claims <- endBy claim endOfLine
  eof
  return claims


main = do
  Right claims <- parse claimsFile "input" <$> TIO.readFile "input_day3.txt"
  print $ take 3 claims
