module AutoSolver( 
    makeRandomMove,
    computerMove,
    computerSolve
    ) where

import Minesweeper    
import System.Random    

--Automatic Computer Solver
makeRandomMove :: Grid -> StdGen -> (Grid, StdGen)
makeRandomMove grid gen = (fst (clearSquare grid coords), newGen)   --Chance it could pick an already cleared square
    where ([coords], newGen) = genRandomPairs gen 1 []

computerMove :: Grid -> Grid
computerMove grid = if gridChanged grid newGrid then newGrid else error "Need to get smarter" --If we have changed something (either adding a flag or clearing a tile) go through grid again
    where newGrid = makeTrivialMove grid (0, 0)

makeTrivialMove :: Grid -> (Int, Int) -> Grid
makeTrivialMove grid (row, col) | row == heightOfMap = grid
                                 | col == widthOfMap = makeTrivialMove grid (row+1, 0)
                                 | otherwise = if changed then newGrid else makeTrivialMove newGrid (row, col+1)
                                    where (newGrid, changed) = checkSquare grid (row, col) 

checkSquare :: Grid -> (Int, Int) -> (Grid, Bool)
checkSquare grid coords = case lookupSquare grid coords of (Clear 0 _) -> (grid, False)
                                                           (Clear n _) -> checkSurrounding grid (getSurrounding coords) n []
                                                           (Mine True _) -> (grid, False)
                                                           (Hidden _ True _) -> error "The computer has incorrectly flagged a tile" --Debug code
                                                           otherwise -> (grid, False) --No information here

checkSurrounding :: Grid -> [(Int, Int)] -> Int -> [(Int, Int)] -> (Grid, Bool) --Shouldn't need to check for empty list?
checkSurrounding  grid [] surroundingMines unknownSqs | surroundingMines == (length unknownSqs) && (length unknownSqs) /= 0 = (placeFlag grid (head unknownSqs), True)   --Place a flag
                                                      | surroundingMines == 0 && (length unknownSqs) /= 0 = (fst (clearSquare grid (head unknownSqs)), True)  --Clear a square
                                                      | otherwise = (grid, False) --We have failed to work out anything useful
checkSurrounding  grid (coords:rest) surroundingMines unknownSqs = 
    case lookupSquare grid coords of (Clear _ _) -> checkSurrounding grid rest surroundingMines unknownSqs
                                     (Hidden _ True _) -> error "Computer has incorrectly flagged a square which doesn't have a mine" --Debugging
                                     (Mine True _) -> checkSurrounding grid rest (surroundingMines-1) unknownSqs
                                     _ -> checkSurrounding grid rest surroundingMines (coords:unknownSqs) --Nothing useful known

--Get computer to solve whole grid
computerSolve :: Grid -> Grid
computerSolve grid = if gridChanged grid newGrid then computerSolve newGrid else newGrid --If we have changed something (either adding a flag or clearing a tile) go through grid again
    where newGrid = makeTrivialMove grid (0, 0)

--Check if a grid has been changed. Only check number of squares revealed and number of flags.
--This means this function should never be used if the grids are different to begin with
gridChanged :: Grid -> Grid -> Bool
gridChanged ( Grid _ _ _ numLeft1 numFlagged1 ) ( Grid _ _ _ numLeft2 numFlagged2 ) = numLeft1 /= numLeft2 || numFlagged1 /= numFlagged2 