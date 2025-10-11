🧩 Sudoku Solver (Haskell)
A command-line Sudoku solver implemented in Haskell. This project was completed as an assignment for CS3210 (Principles of Programming Languages) in Fall 2019, demonstrating mastery of functional programming concepts such as recursion, pattern matching, and high-order functions.

🚀 Key Features
The solver is built around a robust set of validation and utility functions, culminating in the main solving algorithm:

Backtracking Algorithm: The core logic is implemented in the solve function, which uses recursive backtracking to efficiently search the solution space. It strategically tries values 1 through 9 for the first empty cell, validates the resulting board state, and then recursively calls itself until a solution is found.

Comprehensive Validation: Includes functions to check the validity of sequences (rows, columns, and 3×3 boxes) ensuring that no non-zero digits are repeated.

Board Utilities: Functions for input parsing (getBoard), dimension checking (isGridValid), and coordinate mapping (getBox, getEmptySpot).

I/O Handling: The main function handles file input and validates that a valid board file exists before initiating the solver.

🛠️ Technology Stack
Language: Haskell

Dependencies: Standard Haskell libraries (System.Environment, Data.List, System.IO, etc.)

▶️ How to Compile and Run
This project requires the Haskell Platform (including the GHC compiler and cabal or stack) to be installed on your system.

1. Compilation
Use the Glasgow Haskell Compiler (GHC) to compile the source file:

ghc --make Sudoku-Solver.hs

This will generate an executable file named Sudoku-Solver (or Sudoku-Solver.exe on Windows).

2. Input File Format
The solver expects the Sudoku board to be provided in a plain text file (.txt).

The board must be 9×9.

Use digits 1−9 for pre-filled numbers.

Use 0 (zero) to represent an empty cell.

Each row of 9 digits must be on a new line.

Example Input (puzzle.txt):

530070000
600195000
098000060
800060003
400803001
700020006
060000280
000419005
000080079

3. Execution
Run the compiled executable and provide the name of your input file (without the .txt extension) when prompted:

./Sudoku-Solver

What is the name of the txt file? Do not include extension.
puzzle
File found.
Board not solved, continuing...
[[[5,3,4,6,7,8,9,1,2],[6,7,2,1,9,5,3,4,8],[1,9,8,3,4,2,5,6,7],[8,5,9,7,6,1,4,2,3],[4,2,6,8,5,3,7,9,1],[7,1,3,9,2,4,8,5,6],[9,6,1,5,3,7,2,8,4],[2,8,7,4,1,9,6,3,5],[3,4,5,2,8,6,1,7,9]]]
"Done!"

The output is the list of all possible solved boards (in this case, only one solution).
