module AutoSolver
    ( makeRandomMove,
    computerSolve,
    ) where

import Minesweeper    
import System.Random    

--Automatic Computer Solver
makeRandomMove :: Grid -> StdGen -> (Grid, StdGen)
makeRandomMove grid gen = ((clearSquare grid coords), newGen)   --Chance it could pick an already cleared square
    where ([coords], newGen) = genRandomPairs gen 1 []

computerSolve :: Grid -> Grid
computerSolve grid = if grid == newGrid then newGrid else computerSolve newGrid --If we have changed something (either adding a flag or clearing a tile) go through grid again
    where newGrid = makeTrivialMoves grid (0, 0)

makeTrivialMoves :: Grid -> (Int, Int) -> Grid
makeTrivialMoves grid (row, col) | row == heightOfMap = grid
                                 | col == widthOfMap = makeTrivialMoves grid (row+1, 0)
                                 | otherwise = makeTrivialMoves newGrid (row, col+1)
                                    where newGrid = checkSquare grid (row, col) 

checkSquare :: Grid -> (Int, Int) -> Grid
checkSquare grid coords = case square of (Clear n _) -> checkSurrounding grid (getSurrounding coords) n []
                                         (Mine True _) -> grid
                                         (Hidden _ True _) -> error "The computer has incorrectly flagged a tile" --Debug code
                                         otherwise -> grid --No information here
    where (_, square) = updateGrid grid distance action
          action = (Action getSquare coords)
          distance = (targetRowDistance grid action)

checkSurrounding :: Grid -> [(Int, Int)] -> Int -> [(Int, Int)] -> Grid
checkSurrounding  grid [] surroundingMines unknown | surroundingMines == (length unknown) = performMoves grid (queueActions toggleFlag unknown) --Need to update flag number here
                                                   | surroundingMines == 0 = clearSelected grid unknown
                                                   | otherwise = grid --We have failed to work out anything useful
checkSurrounding  grid (coords:rest) surroundingMines unknown = case square of (Clear _ _) -> checkSurrounding grid rest surroundingMines unknown
                                                                               (Hidden _ True _) -> error "Computer has incorrectly flagged a square which doesn't have a mine" --Debugging
                                                                               (Mine True _) -> checkSurrounding grid rest (surroundingMines-1) unknown
                                                                               (Hidden _ _ coords) -> checkSurrounding grid rest surroundingMines (coords:unknown)--Nothing useful known
                                                                               (Mine _ coords) -> checkSurrounding grid rest surroundingMines (coords:unknown)    
    where (_, square) = updateGrid grid distance action
          action = (Action getSquare coords)
          distance = (targetRowDistance grid action)


clearSelected :: Grid -> [(Int, Int)] -> Grid
clearSelected grid []  = grid
clearSelected grid (x:xs)  = clearSelected newGrid xs
    where newGrid = clearSquare grid x
