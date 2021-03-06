-- Random Schedule Generator
-- Authored by user Carcigenicate of Stack Exchange Code Review
-- https://codereview.stackexchange.com/questions/59636/random-schedule-generator
--
-- Content contributed to the Stack Exchange network is provided under
--  a Creative Commons Share Alike license.
--  http://creativecommons.org/licenses/by-sa/2.5/
--
-- Modified by: Ben Isserlis
--  Pedagogical improvements

import Data.List
import Data.Maybe
import System.Random
import System.Random.Shuffle (shuffleM)

type ShiftList = String
data Day = Day Int [Shift] deriving (Eq, Show)
type Person = String
data Shift = Shift Char Person deriving (Eq, Show)

type Schedule = [Day]

--Adds Xs ("is-off") to the shift list to pair up with non-working people
padShifts :: Int -> ShiftList -> ShiftList
padShifts nPeople shifts =
    shifts ++ replicate (nPeople - length shifts) 'X'

--Randomly assigns people shifts
randomFillShifts :: [Person] -> ShiftList -> IO [Shift]
randomFillShifts people shifts = do
    let paddedShifts = padShifts (length people) shifts
    shuffledPeople <- shuffleM people
    return $ zipWith Shift paddedShifts shuffledPeople 

randomDay :: Int -> [Person] -> ShiftList -> IO Day
randomDay day people shifts = fmap (Day day) (randomFillShifts people shifts)

getRandomSchedule :: Int -> [Person] -> ShiftList -> IO Schedule
getRandomSchedule days people shifts = 
    sequence [randomDay day people shifts | day <- [0..days - 1]]

testLoop :: Int -> [Person] -> ShiftList -> Int -> IO [Schedule]
testLoop days people shifts trys =
    fmap catMaybes $ mapM (\t -> do
        putStr $ if t `rem` 10000 == 0
                     then "Try number " ++ show t ++ "\n"
                     else ""
        s <- getRandomSchedule days people shifts
        if pred s
            then return $ Just s
            else return Nothing) [0..trys]
        where
            pred s = noonesWorkingLongerThen 7 s

main :: IO ()
main = do
    let days = 30 --The full rotation length
        people = ["B","E","K","T","R","C"] --Staff list
        shifts = "123" --Each char is a seperate shift
    found <- testLoop days people shifts 50000
    print found
    return ()

getStaffList :: Schedule -> [Person]
getStaffList []                = []
getStaffList (Day _ shifts: _) = map person shifts
    where person (Shift _ p) = p

hasOff :: Person -> Day -> Bool
p `hasOff` (Day _ shifts) = all isOff shifts
    where isOff (Shift s q) = p /= q || s == 'X'

longestStreak :: Eq e => e -> [e] -> Int
longestStreak e l = maximum . (0:) -- Identity element, in case of no matches
                  . map length
                  . filter (e `matchesGroup`)
                  . groupBy (==)
                  $ l
    where matchesGroup _ []     = False
          matchesGroup e (x:xs) = e == x

getStretchList :: Schedule -> [Int]
getStretchList sched =
    map (\person -> 
        longestStreak False $ map (\day -> person `hasOff` day) sched) staffList
    where
        staffList = getStaffList sched 

--Restrictions:

noonesWorkingLongerThen :: Int -> Schedule -> Bool
noonesWorkingLongerThen daysLong sched =
    not . any (>daysLong) $ getStretchList sched

