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
import Data.Kind

type Sequence = [Int]
type Board    = [Sequence]
--type Empty    = [[a]]
-- ***** HELPER FUNCTIONS *****

-- name: toInt
{- description: converts given parameter to integer
input: a string
output: the string converted to integer
example: toInt "123" returns 123 -}
toInt :: [Char] -> Int
toInt seq = read seq :: Int

-- name: toIntList
{- description: converts given parameter to a sequence of integers (one digit at a time)
input: a string
output: the string converted into a sequence of integers
example: toIntList "123" returns [1, 2, 3] -}
toIntList :: [Char] -> Sequence
toIntList seq = [ toInt [c] | c <- seq ]

-- ***** GETTER FUNCTIONS *****

-- name: getBoard
{-
description: convert given string to a sudoku board
input: a string (the board as read from a sudoku input file)
output: a sudoku board
example: getBoard "530070000\n600195000\n098000060\n800060003\n400803001\n700020006\n060000280\n000419005\n000080079\n" yields
[ [5,3,0,0,7,0,0,0,0],
[6,0,0,1,9,5,0,0,0],
[0,9,8,0,0,0,0,6,0],
[8,0,0,0,6,0,0,0,3],
[4,0,0,8,0,3,0,0,1],
[7,0,0,0,2,0,0,0,6],
[0,6,0,0,0,0,2,8,0],
[0,0,0,4,1,9,0,0,5],
[0,0,0,0,8,0,0,7,9] ]
hint: use lines to convert the string into a list of strings, and then apply toIntList on each of the strings of the list to return the board
-}
getBoard :: [Char] -> Board
getBoard seq = map toIntList(lines seq)

-- name: getNRows
{-
description: given a board, return its number of rows
input: a board
output: number of rows
example: getNRows
[ [5,3,0,0,7,0,0,0,0],
[6,0,0,1,9,5,0,0,0],
[0,9,8,0,0,0,0,6,0],
[8,0,0,0,6,0,0,0,3],
[4,0,0,8,0,3,0,0,1],
[7,0,0,0,2,0,0,0,6],
[0,6,0,0,0,0,2,8,0],
[0,0,0,4,1,9,0,0,5],
[0,0,0,0,8,0,0,7,9] ] yields 9
hint: use length
-}
getNRows :: Board -> Int
getNRows row = length(row)

-- name: getNCols
{-
description: given a board, return its number of columns or 0 if rows do not have the same number of columns
input: a board
output: number of columns
example 1: getNCols
[ [5,3,0,0,7,0,0,0,0],
[6,0,0,1,9,5,0,0,0],
[0,9,8,0,0,0,0,6,0],
[8,0,0,0,6,0,0,0,3],
[4,0,0,8,0,3,0,0,1],
[7,0,0,0,2,0,0,0,6],
[0,6,0,0,0,0,2,8,0],
[0,0,0,4,1,9,0,0,5],
[0,0,0,0,8,0,0,7,9] ] yields 9
example 2: getNCols
[ [5,3,0,0,7,0,0,0,0],
[6,0,0,1,9,5,0,0,0],
[0,9,8,0,0,0,6,0],
[8,0,0,0,6,0,3],
[4,0,0,8,0,3,0,0,1],
[7,0,0,0,2,0,0,0,6],
[0,6,0,0],
[0,0,0,4,1,9,0,0,5],
[0,0,0,0,8,0,0,7,9] ] yields 0
hint: use length to create a list with all the sizes of each row from the board; then decide whether all of the rows have the same size, returning that size if yes, or 0 otherwise
-}
getNCols :: Board -> Int
getNCols board
  | all (== length (head board)) [length x | x <- board] = length (head board)
  | otherwise = 0

-- name: getBox
{-
description: given a board and box coordinates, return the correspondent box as a sequence
input: a board and two integer (box coordinates)
output: a sequence
example: getBox
[ [5,3,0,0,7,0,0,0,0],
[6,0,0,1,9,5,0,0,0],
[0,9,8,0,0,0,0,6,0],
[8,0,0,0,6,0,0,0,3],
[4,0,0,8,0,3,0,0,1],
[7,0,0,0,2,0,0,0,6],
[0,6,0,0,0,0,2,8,0],
[0,0,0,4,1,9,0,0,5],
[0,0,0,0,8,0,0,7,9] ] 1 1 yields [0,8,0,6,0,2,0,3,0]
hint: use list comprehension to filter the rows of the target box; then transpose what you got and apply the same reasoning to filter the columns; use concat to return the sequence
-}
getBox :: Board -> Int -> Int -> Sequence
getBox board x y = concat [ (drop  (x*3) . take  (x*3 + 3)) l | l <- (drop (y*3) . take (y*3 + 3)) board ]

 -- name: getEmptySpot
 {-
 description: given a board, return the first location that is empty (i.e., it has zero), if one exists; OK to assume that you will only call this function when you know that there is an empty spot
 input: a board
 output: a tuple with the coordinates (i, j) of the empty spot found
 example: getEmptySpot
 [ [5,3,0,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ] yields (0,2)
 hint: use list comprehension to generate all the coordinates of the board that are empty; use head to return the first coordinate of your list -}
getEmptySpot :: Board -> (Int, Int)
getEmptySpot board = head [(x, y)
    | x <- [0..8]
    , y <- [0..8]
    , (board !! x) !! y == 0]

-- name: isGridValid
 {-
 description: given a board, return True/False depending whether the given board constitutes a valid grid (i.e., #rows = #cols = 9) or not
 input: a board
 output: True/False
 example 1: isGridValid
 [ [5,3,0,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ] yields True
 example 2:
 [ [5,3,0,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ] returns False
 example 3:
 [ [5,3,0,7,0,0,0,0],
   [6,0,1,9,5,0,0,0],
   [8,0,0,6,0,0,0,3],
   [4,0,8,0,3,0,0,1],
   [7,0,0,2,0,0,0,6],
   [0,0,0,0,0,2,8,0],
   [0,0,4,1,9,0,0,5],
   [0,0,0,8,0,0,7,9] ] returns False
 hint: use getNRows and getNCols -}
isGridValid :: Board -> Bool
isGridValid board = getNRows board == getNCols board

-- name: isSequenceValid
{-
 description: return True/False depending whether the given sequence is valid or not, according to sudoku rules
 input:  a sequence of digits from 0 to 9
 output: True/False
 example 1: isSequenceValid [5,3,0,0,7,0,0,0,0] yields True
 example 2: isSequenceValid [5,3,0,5,7,0,0,0,0] yields False
 hint: build a list with the digits from the given sequence that are different than zero; then determine whether there are digits that repeats in the created list-}
isSequenceValid :: Sequence -> Bool
isSequenceValid seq = [l | l <- seq, l > 0] == nub [l | l <- seq, l > 0 ]

-- name: areRowsValid
 {-
 description: return True/False depending whether ALL of the row sequences are valid or not
 input: a board
 output: True/False
 hint: use list comprehension and isSequenceValid -}
areRowsValid :: Board -> Bool
areRowsValid board = and [isSequenceValid l | l <- board]

-- name: areColsValid
{-
 description: return True/False depending whether ALL of the col sequences are valid or not
 input: a board
 output: True/False
 hint: use areRowsValid of the transposed board -}
areColsValid :: Board -> Bool
areColsValid board = areRowsValid (transpose board)

-- name: areBoxesValid
 {-
 description: return True/False depending whether ALL of the box sequences are valid or not
 input: a board
 output: True/False
 hint: use list comprehension, isSequenceValid, and getBox -}
areBoxesValid :: Board -> Bool
areBoxesValid board = areRowsValid [ getBox board x y | x <- [0..2], y <- [0..2]]

-- name: isValid
{-
 description: return True/False depending whether the given board is valid sudoku configuration or not
 input: a board
 output: True/False
 hint: use isGridValid, areRowsValid, areColsValid, and areBoxesValid -}
isValid :: Board -> Bool
isValid board = isGridValid board && areRowsValid board && areColsValid board && areBoxesValid board

-- name: isCompleted
 {-
 description: return True/False depending whether the given board is completed or not; a board is considered completed if there isn't a single empty cell
 input: a board
 output: True/False
 hint: use list comprehension and the elem function -}
isCompleted :: Board -> Bool
isCompleted board = not(0 `elem` concat board)

-- name: isSolved
{-
 description: return True/False depending whether the given board is solved or not; a board is solved if it is completed and still valid
 input: a board
 output: True/False -}
isSolved :: Board -> Bool
isSolved board = isValid board && isCompleted board

-- ***** SETTER FUNCTIONS *****

-- name: setRowAt
{-
 description: given a sequence, an index, and a value, writes the value at the index location, returning a new sequence, but only if the original value at the specified location is empty; otherwise, return the original sequence unchanged
 input: a sequence, an index, and a value
 output: a new sequence
 example 1: setRowAt [1, 2, 3, 0, 4, 5] 3 9 yields [1,2,3,9,4,5]
 example 2: setRowAt [1, 2, 3, 8, 4, 5] 3 9 yields [1,2,3,8,4,5]
 hint: use concatenation, take, and drop -}
setRowAt :: Sequence -> Int -> Int -> Sequence
setRowAt seq index val | seq  !! index == 0 = (concat [take index seq, [val], drop (index+1) seq])
                       | otherwise = seq

-- name: setBoardAt
{-
 description: given a board, two indexes i and j (representing coordinates), and a value, writes the value at the (i, j) coordinate, returning the new board, but only if the original value at the specified location is empty; otherwise, return the original board unchanged
 input: a board, two indexes (i, j), and a value
 output: a new board
 example 1: setBoardAt
 [ [5,3,0,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ] 0 2 4 yields
 [ [5,3,4,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ]
 hint: use concatenation and setRowAt -}
setBoardAt :: Board -> Int -> Int -> Int -> Board
setBoardAt board i j val = concat [take i board, [setRowAt (board !! i) j val], drop (i + 1) board]

-- name: buildChoices
{-
 description: given a board and a two indexes i and j (representing coordinates), generate ALL possible boards, replacing the cell at (i, j) with ALL possible digits from 1 to 9; OK to assume that the cell at (i, j) is empty
 input: a board and two indexes (i, j)
 output: a list of boards from the original board
 example: buildChoices
 [ [5,3,0,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ] 0 2 yields
 [
 [ [5,3,1,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ],
 [ [5,3,2,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ],
 ...
 [ [5,3,9,0,7,0,0,0,0],
   [6,0,0,1,9,5,0,0,0],
   [0,9,8,0,0,0,0,6,0],
   [8,0,0,0,6,0,0,0,3],
   [4,0,0,8,0,3,0,0,1],
   [7,0,0,0,2,0,0,0,6],
   [0,6,0,0,0,0,2,8,0],
   [0,0,0,4,1,9,0,0,5],
   [0,0,0,0,8,0,0,7,9] ]
 ]
 hint: use list comprehension and the function setBoardAt -}
buildChoices :: Board -> Int -> Int -> [Board]
buildChoices board i j = [ setBoardAt board i j val | val <- [1..9] ]

-- name: solve
{-
 description: given a board, finds all possible solutions (note that dead ends or invalid intermediate solutions are listed as empty boards)
 input: a board
 output: a list of boards from the original board
 note: this code is given to you (just uncomment it when you are ready to test the solver)-}

solve :: Board -> [Board]
solve board
   | isSolved board = [board]
   | isCompleted board = [[]]
   | not (isValid board) = [[]]
   | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
     where
       emptySpot = getEmptySpot board
       i = fst emptySpot
       j = snd emptySpot

-- program starts here
main = do
    putStrLn "What is the name of the txt file? Do not include extension."
    inputStr <- getLine
    let fileName = inputStr ++ ".txt"
    x <- doesFileExist fileName

    if x
        then putStrLn "File found."
        else die "Not a valid file, bye."

    contents <- readFile fileName

    let board = (getBoard contents)

    if (0 `elem` concat board)
        then putStrLn "Board not solved, continuing..."
        else die "No empty spaces, bye."

    let nonEmptyBoard = [new | new <- solve board, length new /= 0]
    putStrLn (show (nonEmptyBoard))

    print "Done!"