import Control.Monad.Identity
import Control.Monad
import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Debug.Trace

-- type definitions
type Parser a = ParsecT T.Text () Identity a
data Registers = Registers Int Int Int Int Int Int deriving (Show,Eq)
data Programline = Programline String Int Int Int deriving (Show,Eq)
type ProgramlineMap = IM.IntMap Programline

-- parsing the input file

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
-- Little bit more sophisticated to also parse negative numbers
integer :: Parser Int
integer = read <$> (plus <|> minus <|> number)
  where plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit

programline :: Parser Programline
programline = do
  opcode <- count 4 anyChar <* spaces
  a <- integer <* spaces
  b <- integer <* spaces
  c <- integer
  return $ Programline opcode a b c

inputFileProgram :: Parser (Int,[Programline])
inputFileProgram = do
  ip <- string "#ip " *> integer <* endOfLine
  lines <- programline `sepBy` endOfLine
  eof
  return (ip,lines)

-- opcodes implementations
getRegisterValue :: Registers -> Int -> Int
getRegisterValue (Registers r0 r1 r2 r3 r4 r5) n = case n of
  0 -> r0
  1 -> r1
  2 -> r2
  3 -> r3
  4 -> r4
  5 -> r5
  
  
setRegister :: Registers -> Int -> Int -> Registers
setRegister (Registers r0 r1 r2 r3 r4 r5) n value = case n of
  0 -> Registers value r1 r2 r3 r4 r5
  1 -> Registers r0 value r2 r3 r4 r5
  2 -> Registers r0 r1 value r3 r4 r5
  3 -> Registers r0 r1 r2 value r4 r5
  4 -> Registers r0 r1 r2 r3 value r5
  5 -> Registers r0 r1 r2 r3 r4 value

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
allOpcodeNames :: [String]
allOpcodeNames = ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"]
opcodePairs = zip allOpcodeNames allOpcodes


opcodeMapping :: M.Map String (Registers -> Programline -> Registers)
opcodeMapping = M.fromList opcodePairs

incrementIP :: Registers -> Int -> Registers
incrementIP (Registers r0 r1 r2 r3 r4 r5) iplocation = case iplocation of
  0 -> Registers (r0+1) r1 r2 r3 r4 r5
  1 -> Registers r0 (r1+1) r2 r3 r4 r5
  2 -> Registers r0 r1 (r2+1) r3 r4 r5
  3 -> Registers r0 r1 r2 (r3+1) r4 r5
  4 -> Registers r0 r1 r2 r3 (r4+1) r5
  5 -> Registers r0 r1 r2 r3 r4 (r5+1)

evaluateProgramline :: ProgramlineMap -> Int -> Registers -> Registers
evaluateProgramline program iplocation registers = case IM.lookup (getRegisterValue registers iplocation) program of
  Nothing -> registers
  Just progline@(Programline opcode a b c) -> traceShow registers $ evaluateProgramline program iplocation (incrementIP newregisters iplocation)
    where f = fromJust $ M.lookup opcode opcodeMapping
          newregisters = f registers progline


runProgramPart1 = do
  Right (iplocation,lines) <- parse inputFileProgram "" <$> TIO.readFile "input_day19_program.txt"
  print $ evaluateProgramline (IM.fromList $ zip [0..] lines) iplocation (Registers 0 0 0 0 0 0)

runProgramPart2 = do
  Right (iplocation,lines) <- parse inputFileProgram "" <$> TIO.readFile "input_day19_program.txt"
  print $ evaluateProgramline (IM.fromList $ zip [0..] lines) iplocation (Registers 1 0 0 0 0 0)

--turns out we need the sum of the factors of 10551309?
factorsNaive n = [ i | i <- [1 .. n] , mod n i == 0 ]
solution2 = sum $ factorsNaive 10551309
