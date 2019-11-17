{-# OPTIONS_GHC -Wall #-}
import Data.List

printBoard :: [[Int]] -> IO ()
printBoard sudoku = mapM_ print (sudoku)

validRow :: Int -> [Int] -> Bool
validRow x line = not (x `elem` line)

validColumn :: Int -> [Int] -> Bool
validColumn x column = not (x `elem` column)

validSquare :: Int -> Int -> Int -> [[Int]] -> Bool
validSquare value row col sudoku =
    let row_square = [(row `div` 3 * 3)..(row `div` 3 * 3)+2]
        col_square = [(col `div` 3 * 3)..(col `div` 3 * 3)+2]
    in not (value `elem` [ sudoku !! y !! x | y <- row_square, x <- col_square])

isValid :: Int -> Int -> Int -> [[Int]] -> Bool
isValid value row col sudoku =
    let rowIsValid = validRow value (sudoku !! row) 
        colIsValid = validColumn value (transpose sudoku !! col)
        squareIsValid = validSquare value row col sudoku
    in (rowIsValid && colIsValid && squareIsValid)

sudokuSolve :: [[Int]] -> [[[Int]]]
sudokuSolve sudoku = sudokuSolve' 0 0 sudoku

-- row col sudoku -> return sudoku
sudokuSolve' :: Int -> Int -> [[Int]] -> [[[Int]]]
sudokuSolve' row col sudoku
    | row == 9 = [sudoku]
    | col == 9 = sudokuSolve' (row+1) 0 sudoku
    | (sudoku !! row !! col == 0) = [solution | value <- [1..9], solution <- sudokuSolve' row (col+1) (updateSudoku value row col sudoku), isValid value row col sudoku]
    | otherwise = sudokuSolve' row (col+1) sudoku

updateSudoku :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateSudoku value row col sudoku = 
    let sudoku_row = sudoku !! row
        new_row = take col sudoku_row ++ [value] ++ drop (col + 1) sudoku_row
        newSudoku = take row sudoku ++ [new_row] ++ drop (row + 1) sudoku
    in newSudoku

main :: IO ()
main = do
    -- Aqui os valores com 0 são os espaços vazios
    let sudoku = [
                [3,1,6,5,7,8,4,9,2],
                [5,2,9,1,3,4,7,6,8],
                [4,8,7,6,2,9,5,3,1],
                [2,6,3,4,1,5,9,8,7],
                [9,7,4,8,6,3,1,2,5],
                [8,5,1,7,9,2,6,4,3],
                [1,3,8,9,4,7,2,5,6],
                [6,9,2,3,5,1,8,7,4],
                [7,0,0,0,0,0,0,0,0]
            ]
    printBoard (head (sudokuSolve sudoku))
-- Resposta
-- [3,1,6,5,7,8,4,9,2],
-- [5,2,9,1,3,4,7,6,8],
-- [4,8,7,6,2,9,5,3,1],
-- [2,6,3,4,1,5,9,8,7],
-- [9,7,4,8,6,3,1,2,5],
-- [8,5,1,7,9,2,6,4,3],
-- [1,3,8,9,4,7,2,5,6],
-- [6,9,2,3,5,1,8,7,4],
-- [7,4,5,2,8,6,3,1,9]

-- Alguns em branco
-- [[3,0,6,5,0,8,4,0,0], 
-- [5,2,0,0,0,0,0,0,0], 
-- [0,8,7,0,0,0,0,3,1], 
-- [0,0,3,0,1,0,0,8,0], 
-- [9,0,0,8,6,3,0,0,5], 
-- [0,5,0,0,9,0,6,0,0], 
-- [1,3,0,0,0,0,2,5,0], 
-- [0,0,0,0,0,0,0,7,4], 
-- [0,0,5,2,0,6,3,0,0]]