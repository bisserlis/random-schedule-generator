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
getRandomSchedule days people shifts = mapM (\day ->
    randomDay day people shifts) [0..(days - 1)]

getTestSched :: IO Schedule
getTestSched = do
    let days = 30
        people = ["Brendon","Erin","Kenton","Troy"]
        shifts = "DEXX"
    s <- getRandomSchedule days people shifts
    return s

testLoop :: Int -> [Person] -> ShiftList -> Int -> IO [Schedule]
testLoop days people shifts trys =
    fmap catMaybes $ mapM (\t-> do
        putStr $ if t `rem` 10000 == 0 then "Try number " ++ show t ++ "\n"
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
getStaffList (cDay:_) = let (Day _ shifts) = cDay in
    map (\(Shift _ p) -> p) shifts

isOffOn :: Person -> Day -> Bool
person `isOffOn` (Day _ shifts) =
    any (== True) $ map (\(Shift shift p) -> shift == 'X' && p == person) shifts

longestStreak :: Eq e => e -> [e] -> Int
longestStreak toMatch list =
    snd $ foldr (\value oldV@(cStretch,cMax) ->
        if value == toMatch
            then (cStretch + 1,max cMax (cStretch + 1)) else (0,cMax)) (0,0) list

getStretchList :: Schedule -> [Int]
getStretchList sched =
    map (\person -> 
        longestStreak False $ map (\day -> person `isOffOn` day) sched) staffList
    where
        staffList = getStaffList sched 

--Restrictions:

noonesWorkingLongerThen :: Int -> Schedule -> Bool
noonesWorkingLongerThen daysLong sched =
    not . any (>daysLong) $ getStretchList sched

