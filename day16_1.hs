{-# LANGUAGE OverloadedLists #-}
import Control.Monad.Identity
import Data.Bits
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec

-- type definitions
type Parser a = ParsecT T.Text () Identity a
data Registers = Registers Int Int Int Int deriving (Show,Eq)
data Programline = Programline Int Int Int Int deriving (Show,Eq)
data Example = Example Registers Programline Registers deriving (Show,Eq)

-- parsing the input file

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
-- Little bit more sophisticated to also parse negative numbers
integer :: Parser Int
integer = read <$> (plus <|> minus <|> number)
  where plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit

registers :: Parser Registers
registers = do
  string "["
  [registerA, registerB, registerC, registerD] <- integer `sepBy` string ", "
  string "]"
  return $ Registers registerA registerB registerC registerD

programline :: Parser Programline
programline = do
  opcode <- integer <* spaces
  a <- integer <* spaces
  b <- integer <* spaces
  c <- integer
  return $ Programline opcode a b c

example :: Parser Example
example = do
  beforeState <- string "Before: " *> registers <* endOfLine
  line <- programline <* endOfLine
  afterState <- string "After:  " *> registers <* endOfLine
  return $ Example beforeState line afterState

inputFile :: Parser [Example]
inputFile = do
  examples <- example `sepBy` endOfLine
  eof
  return examples


-- opcodes implementations
getRegisterValue :: Registers -> Int -> Int
getRegisterValue (Registers r0 r1 r2 r3) n = case n of
  0 -> r0
  1 -> r1
  2 -> r2
  3 -> r3
  
setRegister :: Registers -> Int -> Int -> Registers
setRegister (Registers r0 r1 r2 r3) n value = case n of
  0 -> Registers value r1 r2 r3
  1 -> Registers r0 value r2 r3
  2 -> Registers r0 r1 value r3
  3 -> Registers r0 r1 r2 value

addr :: Registers -> Programline -> Registers
addr s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) + (getRegisterValue s b)) 
addi :: Registers -> Programline -> Registers
addi s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) + b) 
mulr :: Registers -> Programline -> Registers
mulr s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) * (getRegisterValue s b)) 
muli :: Registers -> Programline -> Registers
muli s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) * b) 
banr :: Registers -> Programline -> Registers
banr s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) .&. (getRegisterValue s b)) 
bani :: Registers -> Programline -> Registers
bani s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) .&. b) 
borr :: Registers -> Programline -> Registers
borr s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) .|. (getRegisterValue s b)) 
bori :: Registers -> Programline -> Registers
bori s (Programline _ a b c) = setRegister s c ((getRegisterValue s a) .|. b) 
setr :: Registers -> Programline -> Registers
setr s (Programline _ a b c) = setRegister s c (getRegisterValue s a) 
seti :: Registers -> Programline -> Registers
seti s (Programline _ a b c) = setRegister s c a
gtir :: Registers -> Programline -> Registers
gtir s (Programline _ a b c) = setRegister s c $ if (a > (getRegisterValue s b)) then 1 else 0 
gtri :: Registers -> Programline -> Registers
gtri s (Programline _ a b c) = setRegister s c $ if ((getRegisterValue s a) > b) then 1 else 0
gtrr :: Registers -> Programline -> Registers
gtrr s (Programline _ a b c) = setRegister s c $ if ((getRegisterValue s a) > (getRegisterValue s b)) then 1 else 0
eqir :: Registers -> Programline -> Registers
eqir s (Programline _ a b c) = setRegister s c $ if (a == (getRegisterValue s b)) then 1 else 0
eqri :: Registers -> Programline -> Registers
eqri s (Programline _ a b c) = setRegister s c $ if ((getRegisterValue s a) == b) then 1 else 0
eqrr :: Registers -> Programline -> Registers
eqrr s (Programline _ a b c) = setRegister s c $ if ((getRegisterValue s a) == (getRegisterValue s b)) then 1 else 0

allOpcodes :: [Registers -> Programline -> Registers]
allOpcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

-- actual question solving
exampleBehavesLikeNumberOfOpcodes :: Example -> Int
exampleBehavesLikeNumberOfOpcodes example = length $ filter (opcodeMatchesExample example) allOpcodes
  where opcodeMatchesExample (Example bs progline as) opcode = (opcode bs progline) == as

main = do
  Right examples <- parse inputFile "" <$> TIO.readFile "input_day16_examples.txt"
  print . length $ filter (>= 3) $ map exampleBehavesLikeNumberOfOpcodes examples
