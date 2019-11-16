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

sudokuSolve :: [[Int]] -> [[Int]]
sudokuSolve sudoku = sudokuSolve' 0 0 sudoku

sudokuSolve' :: Int -> Int -> [[Int]] -> [[Int]]
-- sudokuSolve' 8 8 sudoku = [ solution | value <- [1..9], solution <- updateSudoku value 8 8 sudoku, (isValid value 8 8 sudoku)]
-- sudokuSolve' row col sudoku = 
--     [solution | 
--     solution <- sudokuSolve' nrow ncol sudoku, 
--     nrow = row+1, 
--     ncol = col+1, 
--     value <- [1..9],
--     (sudoku !! col !! row == 0) && (isValid value row col sudoku)]

updateSudoku :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateSudoku value row col sudoku = 
    let sudoku_row = sudoku !! row
        new_row = take col sudoku_row ++ [value] ++ drop (col + 1) sudoku_row
        newSudoku = take row sudoku ++ [new_row] ++ drop (row + 1) sudoku
    in newSudoku

main = do
    -- Aqui os valores com 0 são os espaços vazios
    let sudoku = [
                [3,0,6,5,0,8,4,0,0], 
                [5,2,0,0,0,0,0,0,0], 
                [0,8,7,0,0,0,0,3,1], 
                [0,0,3,0,1,0,0,8,0], 
                [9,0,0,8,6,3,0,0,5], 
                [0,5,0,0,9,0,6,0,0], 
                [1,3,0,0,0,0,2,5,0], 
                [0,0,0,0,0,0,0,7,4], 
                [0,0,5,2,0,6,3,0,0]
            ]
    -- print (updateSudoku 9 0 1 sudoku)
    -- print (isValid 3 2 0 sudoku)
    printBoard (sudokuSolve sudoku)
    


