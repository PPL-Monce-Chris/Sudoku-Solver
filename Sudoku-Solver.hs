-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author(s): Chris Johnson, Moncerat Romero
-- Date: 10/31/19

import System.Environment
import System.IO
import Data.List
import Data.Typeable
import System.Directory
import System.Exit

type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

-- name: toInt
-- description: converts given parameter to integer
-- input: a string
-- output: the string converted to integer
-- example: toInt "123" returns 123
toInt :: [Char] -> Int
toInt s = read s :: Int

-- name: toIntList
-- description: converts given parameter to a sequence of integers (one digit at a time)
-- input: a string
-- output: the string converted into a sequence of integers
-- example: toIntList "123" returns [1, 2, 3]
toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]

-- ***** GETTER FUNCTIONS *****

-- TODO #1
getBoard :: [Char] -> Board
getBoard s = map toIntList(lines s)

-- TODO #2
getNRows :: Board -> Int
getNRows r = length(r)

-- TODO #3
-- need to test
getNCols :: Board -> Int
getNCols b
  | all (== length (head b)) [length x | x <- b] = length (head b)
  | otherwise = 0

-- TODO #4
-- need to test
slice l st en = (drop st . take en) l

getBox :: Board -> Int -> Int -> Sequence
getBox b x y = concat [ slice xs (x*3) (x*3+3) | xs <- (slice b (y*3) (y*3+3)) ]

-- getBox b x y = concat [ (drop xs (x3) (x3+3) . take xs (x3) (x3+3))  | xs <- ((drop b (y3) (y3+3) . take b (y3) (y3+3)) ) ]

-- getBox board i1 i2 = concat [ slice box (i2*3) ((i2*3)+3) | box <- (slice board (i1*3) ((i1*3)+3) ]

-- TODO #5
-- need to test
getEmptySpot :: Board -> (Int, Int)
getEmptySpot b = head [(x, y)
    | x <- [0..8]
    , y <- [0..8]
    , (b !! y) !! x == 0]
-- x and y might need to be swapped at the end

-- TODO #6
-- need to test
isGridValid :: Board -> Bool
isGridValid b = getNRows b == getNCols b

-- TODO #7
-- need to test
isSequenceValid :: Sequence -> Bool
isSequenceValid s = [xs | xs <- s, xs > 0] == nub [xs | xs <- s, xs > 0 ]

-- TODO #8
-- need to test
areRowsValid :: Board -> Bool
areRowsValid b = and [isSequenceValid xs | xs <- b]

-- TODO #9
-- need to test
areColsValid :: Board -> Bool
areColsValid b = areRowsValid transpose b

-- TODO #10
-- name: areBoxesValid
-- description: return True/False depending whether ALL of the box sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use list comprehension, isSequenceValid, and getBox
areBoxesValid :: Board -> Bool
areBoxesValid b = areRowsValid [ getBox b x y | x <- [0..2], y <- [0..2]]

-- TODO #11
-- name: isValid
-- description: return True/False depending whether the given board is valid sudoku configuration or not
-- input: a board
-- output: True/False
-- hint: use isGridValid, areRowsValid, areColsValid, and areBoxesValid
isValid :: Board -> Bool
isValid board = isGridValid board && areRowsValid board && areColsValid board && areBoxesValid board


-- TODO #12
-- name: isCompleted
-- description: return True/False depending whether the given board is completed or not; a board is considered completed if there isn't a single empty cell
-- input: a board
-- output: True/False
-- hint: use list comprehension and the elem function
isCompleted :: Board -> Bool
isCompleted board = not(0 `elem` concat board)

-- TODO #13
-- name: isSolved
-- description: return True/False depending whether the given board is solved or not; a board is solved if it is completed and still valid
-- input: a board
-- output: True/False
isSolved :: Board -> Bool
isSolved board = isValid board && isCompleted board

-- ***** SETTER FUNCTIONS *****

-- TODO #14
-- name: setRowAt
-- description: given a sequence, an index, and a value, writes the value at the index location, returning a new sequence, but only if the original value at the specified location is empty; otherwise, return the original sequence unchanged
-- input: a sequence, an index, and a value
-- output: a new sequence
-- example 1: setRowAt [1, 2, 3, 0, 4, 5] 3 9 yields [1,2,3,9,4,5]
-- example 2: setRowAt [1, 2, 3, 8, 4, 5] 3 9 yields [1,2,3,8,4,5]
-- hint: use concatenation, take, and drop
setRowAt :: Sequence -> Int -> Int -> Sequence
setROwAt seq index value = if seq !! index == 0
                                then concat [take index seq, [value], drop (index + 1) seq]
                                else seq


-- TODO #15
-- name: setBoardAt
-- description: given a board, two indexes i and j (representing coordinates), and a value, writes the value at the (i, j) coordinate, returning the new board, but only if the original value at the specified location is empty; otherwise, return the original board unchanged
-- input: a board, two indexes (i, j), and a value
-- output: a new board
-- example 1: setBoardAt
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 4 yields
-- [ [5,3,4,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- hint: use concatenation and setRowAt
setBoardAt :: Board -> Int -> Int -> Int -> Board
setBoardAt board i j val = concat [take i board, [setRowAt board !! i j val], drop (i + 1) board]

-- TODO #16
-- name: buildChoices
-- description: given a board and a two indexes i and j (representing coordinates), generate ALL possible boards, replacing the cell at (i, j) with ALL possible digits from 1 to 9; OK to assume that the cell at (i, j) is empty
-- input: a board and two indexes (i, j)
-- output: a list of boards from the original board
-- example: buildChoices
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 yields
-- [
-- [ [5,3,1,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- [ [5,3,2,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- ...
-- [ [5,3,9,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- ]
-- hint: use list comprehension and the function setBoardAt
-- buildChoices :: Board -> Int -> Int -> [Board]

-- name: solve
-- description: given a board, finds all possible solutions (note that dead ends or invalid intermediate solutions are listed as empty boards)
-- input: a board
-- output: a list of boards from the original board
-- note: this code is given to you (just uncomment it when you are ready to test the solver)
-- solve :: Board -> [Board]
-- solve board
--   | isSolved board = [board]
--   | isCompleted board = [[[]]]
--   | not (isValid board) = [[[]]]
--   | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
--     where
--       emptySpot = getEmptySpot board
--       i = fst emptySpot
--       j = snd emptySpot

-- program starts here
main = do
    putStrLn "What is the name of the file? Do not include extension."
    inputStr <- getLine
    let fileName = inputStr ++ ".txt"
    x <- doesFileExist fileName
    putStrLn fileName
    if x
        then putStrLn"file Exists"
        else die "bye"

    contents <- readFile fileName
    --let con =
    --board <- (getBoard contents)
    putStrLn (show ( isCompleted (getBoard contents)))




  -- TODO #17: validate the command-line and get the file name containing the board
    -- done
  -- TODO #18: read the contents of the board file into a string
    -- done
  -- TODO #19: create a board from the string board (hint: use getBoard)
    -- done

  -- TODO #20: use solve to find the solutions, disconsidering the ones that are [[]]

  -- TODO #21: print the solutions found

  --print "Done!"
