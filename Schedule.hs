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

type ShiftList = String
data Day = Day Int [Shift] deriving (Eq, Show)
type Person = String
data Shift = Shift Char Person deriving (Eq, Show)

type Schedule = [Day]

randomElem :: [a] -> IO a
randomElem list = do
    let enumLimit = (length list) - 1
    r <- randomRIO (0,enumLimit)
    let e = [e | (n,e) <- zip [0..enumLimit] list, n == r]
    return $ head e

shuffle :: Eq e => [e] -> IO [e]
shuffle [] = return []
shuffle list = do
    re <- randomElem list
    let restList = delete re list
    recur <- shuffle restList
    return $ re : recur

--Adds Xs ("is-off") to the shift list to pair up with non-working people
padShifts :: Int -> ShiftList -> ShiftList
padShifts nPeople shifts =
    shifts ++ (take (nPeople - length shifts) $ repeat 'X')

--Randomly assigns people shifts
randomFillShifts :: [Person] -> ShiftList -> IO [Shift]
randomFillShifts people shifts = do
    shuffledPeople <- shuffle people
    return $ map (\(p,s) -> Shift s p) (zip shuffledPeople paddedShifts)
    where
        paddedShifts = padShifts (length people) shifts

randomDay :: Int -> [Person] -> ShiftList -> IO Day
randomDay day people shifts = do
    rshifts <- randomFillShifts people shifts 
    return $ Day day rshifts

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

