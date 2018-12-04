{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Control.Monad.Identity
import Data.Ord
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Time
import Data.Time.LocalTime
import Text.Parsec

-- Some types that might be relevant for this problem
type GuardId = Int
data EventType = GuardFellAsleep | GuardWokeUp | GuardChange GuardId deriving (Show, Eq)
data Event = Event { eventTime :: LocalTime
                   , eventType :: EventType}
                   deriving (Show,Eq)

-- reuse boilerplate for parser:
type Parser a = ParsecT T.Text () Identity a

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
integer :: Stream s m Char => ParsecT s u m Int
integer = read <$> many1 digit

time :: Parser LocalTime
time = do
  char '['
  year <- integer
  char '-'
  month <- integer
  char '-'
  day <- integer
  space
  hour <- integer
  char ':'
  minute <- integer
  string "] "
  return $ LocalTime (fromGregorian (fromIntegral year) month day) (TimeOfDay hour minute 0)

-- Let's make some parsers!
event :: Parser Event
event = do
  timeOfEvent <- time
  typeOfEvent <- string "falls asleep" *> pure GuardFellAsleep
             <|> string "wakes up"     *> pure GuardWokeUp
             <|> do
                 string "Guard #"
                 guardNumber <- integer
                 string " begins shift"
                 return $ GuardChange guardNumber
  return $ Event timeOfEvent typeOfEvent

-- Returns a chronologically sorted list of parsed events, since the input is all mixed up timewise
eventsFileParser :: Parser [Event]
eventsFileParser = do
  events <- sepBy event endOfLine
  eof
  return $ sortBy (comparing eventTime) events

-- just to verify if the input has a sensible shape or if there is trickery afoot
anySleepingGuardChanges :: [EventType] -> Bool
anySleepingGuardChanges (e:[]) = False
anySleepingGuardChanges (GuardFellAsleep:(GuardChange _):ess) = True
anySleepingGuardChanges (e:es:ess) = anySleepingGuardChanges (es:ess)

-- Now we have a sorted log of events, let's count how many minutes each guard sleeps
zerotime = LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0)
minutesAsleep (LocalTime _ nap) (LocalTime _ wake) = (todMin wake) - (todMin nap)
trd (_,_,x) = x
countSnozeMinutes :: [Event] -> M.Map GuardId Int
countSnozeMinutes = trd . foldl' foldfunc (0, zerotime, M.empty)
  where foldfunc (currentGuard, snozeStart, snozeMap) (Event evtTime (GuardChange id)) = (id, zerotime, snozeMap)
        foldfunc (currentGuard, snozeStart, snozeMap) (Event evtTime GuardFellAsleep) =  (currentGuard, evtTime, snozeMap)
        foldfunc (currentGuard, snozeStart, snozeMap) (Event evtTime GuardWokeUp) = (currentGuard, zerotime, M.insertWith (+) currentGuard (minutesAsleep snozeStart evtTime) snozeMap)

main = do
  Right events <- parse eventsFileParser "input" <$> TIO.readFile "input_day4.txt"
  let snozeMap = countSnozeMinutes events
  let maxSnozeTime = maximum . M.elems $ snozeMap
  print $ (\(x,y) -> x*y ) . head . M.toList . M.filter (== maxSnozeTime) $ snozeMap

