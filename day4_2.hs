{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Control.Monad.Identity
import Data.Ord
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
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

-- Now we have a sorted log of events, let's count how many minutes each guard sleeps
zerotime = LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0)

-- Obtain a list of all the guard ids from the list of events, just iterate over all 1k events and see which
-- guards are changed to.
findAllGuardIds :: [Event] -> S.Set GuardId
findAllGuardIds = foldl' foldfunc S.empty
  where foldfunc s (Event evtTime (GuardChange id)) = S.insert id s
        foldfunc s (Event evtTime GuardFellAsleep) = s
        foldfunc s (Event evtTime GuardWokeUp) = s

-- update the given map with all the minutes in an interval
updateMinuteMap :: M.Map Int Int -> LocalTime -> LocalTime -> M.Map Int Int
updateMinuteMap minuteMap (LocalTime _ nap) (LocalTime _ wake) = foldl' foldfunc minuteMap [napMinute..(wakeMinute-1)]
  where wakeMinute = fromIntegral $ todMin wake
        napMinute = fromIntegral $  todMin nap
        foldfunc minmap minute = M.insertWith (+) minute 1 minmap

-- make a map with times for each minute
emptySnozeMap = M.singleton 0 0 -- needed because otherwise maximumBy will barf on guards  with no sleeping at all
trd (_,_,x) = x
findSnoziestTime :: GuardId -> [Event] -> M.Map Int Int
findSnoziestTime guardId = trd . foldl' foldfunc (0, zerotime, emptySnozeMap)
  where foldfunc (currentGuard, snozeStart, snozeMap) (Event evtTime (GuardChange id)) = (id, zerotime, snozeMap)
        foldfunc (currentGuard, snozeStart, snozeMap) (Event evtTime GuardFellAsleep) =  (currentGuard, evtTime, snozeMap)
        foldfunc (currentGuard, snozeStart, snozeMap) (Event evtTime GuardWokeUp)
          | currentGuard == guardId = (currentGuard, zerotime, updateMinuteMap snozeMap snozeStart evtTime)
          | otherwise = (currentGuard, zerotime, snozeMap)

pairWithMaximumValue :: Ord v => M.Map k v -> (k,v)
pairWithMaximumValue m = maximumBy (comparing snd) $ M.toList m

flattenTuple (x,(y,z)) = (x,y,z)

main = do
  Right events <- parse eventsFileParser "input" <$> TIO.readFile "input_day4.txt"
  let allmaps = map (\guardId -> (guardId, findSnoziestTime guardId events)) . S.toList . findAllGuardIds $ events
  let allSleepiestMinutes = map (\(x,y) -> flattenTuple (x, pairWithMaximumValue y)) allmaps
  print . (\(x,y,z) -> x*y) . maximumBy (comparing trd) $ allSleepiestMinutes
