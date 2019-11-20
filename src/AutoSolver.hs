module AutoSolver( 
    makeRandomMove,
    computerMove
    --computerSolve
    ) where

import Minesweeper    
import System.Random   
import Control.Monad(liftM) 
import System.IO.Unsafe (unsafePerformIO)

{-newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap a = liftM a

instance Applicative (State s) where
    pure a = State (\s -> (a,s))

instance Monad (State s) where
    m >>= k = State (\s -> let (a,s') = runState m s
                           in runState (k a) s')
-}

data SquareProb = SquareProb (Int, Int) Double    --Takes the coordinates of a square and a function to run on that square
    deriving Show

instance Eq SquareProb where
    (==) (SquareProb coords1 _) (SquareProb coords2 _) = coords1 == coords2

instance Ord SquareProb where
    compare (SquareProb (row1, col1) _) (SquareProb (row2, col2) _) 
        = case compare row1 row2 of EQ -> compare col1 col2
                                    _ -> compare row1 row2

data InferredMove = ClearSq (Int, Int) | FlagSq (Int, Int)
    deriving Show
    
instance Eq InferredMove where
    (==) (ClearSq coords) (FlagSq coord) = coords == coords
    (==) (FlagSq coords) (FlagSq coord) = coords == coords
    (==) _ _ = False

--Automatic Computer Solver
makeRandomMove :: Grid -> StdGen -> (Grid, Bool)
makeRandomMove grid gen = case lookupSquare grid coords of (Hidden _ False _) -> (newGrid, True)
                                                           (Mine False _) -> (newGrid, False)
                                                           otherwise -> makeRandomMove grid newGen --Square has already been picked or is flagged
    where ([coords], newGen) = genRandomPairs gen 1 []
          (newGrid, _) = clearSquare grid coords

makeGuess :: Grid -> [SquareProb] -> StdGen -> (Grid, Bool)
makeGuess grid [] gen = makeRandomMove grid gen   --TODO If our least prob can't beat the default YOLO
makeGuess grid (sqProb:sqProbs) gen = if prob < (getDefaultProb grid) then clearSquare grid coords else (unsafePerformIO $ print "Useless Prob ") `seq` makeRandomMove grid gen 
    where (SquareProb coords prob) = getLeastProb sqProbs sqProb

getLeastProb :: [SquareProb] -> SquareProb -> SquareProb
getLeastProb [] sqProb = (unsafePerformIO $ print sqProb) `seq` sqProb  --TODO remove this abomination
getLeastProb ((SquareProb nextCoords nextProb):sqProbs) (SquareProb coords currLeastProb) 
    | nextProb < currLeastProb = getLeastProb sqProbs (SquareProb nextCoords nextProb)
    | otherwise = getLeastProb sqProbs (SquareProb coords currLeastProb)

--Based off the algorithm described here https://luckytoilet.wordpress.com/2012/12/23/2125/
tankAlgorithm :: Grid -> [SquareProb] -> (Grid, Bool)
tankAlgorithm grid [] = (grid, False)  --We have failed to infer any moves
tankAlgorithm grid ((SquareProb coords _):rest) = if changed then (newGrid, changed) else tankAlgorithm newGrid rest
    where (newGrid, changed) = investigateSquare grid coords

investigateSquare :: Grid -> (Int, Int) -> (Grid, Bool)
investigateSquare grid sqCoords = if length movesIntersection > 0 then (unsafePerformIO $ print (head movesIntersection))`seq` (playMove grid (head movesIntersection), True) else (grid, False)
    where movesIfMine = propagateInference grid [FlagSq sqCoords] [FlagSq sqCoords]
          movesIfClear = propagateInference grid [ClearSq sqCoords] [ClearSq sqCoords]
          movesIntersection = intersect movesIfMine movesIfClear

propagateInference :: Grid -> [InferredMove] -> [InferredMove] -> [InferredMove]
propagateInference grid [] inferred = inferred
propagateInference grid (x:xs) inferred = inferred
        where surroundingSqs = getSurrounding (coordsOfInferred x)

searchSurrounding :: Grid -> [(Int, Int)] -> [InferredMove] -> [InferredMove]
searchSurrounding grid [] inferred = []
searchSurrounding grid (coords:rest) inferred = case (getInferredValue grid coords inferred) of 
    (Clear 0 _) -> searchSurrounding grid rest inferred --No information here as all adjacent tiles will already be cleared
    (Clear n _) -> searchSurrounding grid rest (tankSurrounding grid inferred (getSurrounding coords) n []  )
    otherwise -> searchSurrounding grid rest inferred --No information here

--Nasty code duplication
tankSurrounding :: Grid -> [InferredMove] -> [(Int, Int)] -> Int -> [(Int, Int)]-> [InferredMove]
tankSurrounding gird inferred [] minesNum unknownSqs
    | minesNum == (length unknownSqs) && (length unknownSqs) /= 0 = map FlagSq unknownSqs
    | minesNum == 0 && (length unknownSqs) /= 0 = map ClearSq unknownSqs
    | otherwise = []
tankSurrounding grid inferred (coords:rest) minesNum unknownSqs = case getInferredValue grid coords inferred of
    (Clear _ _) -> tankSurrounding grid inferred rest minesNum unknownSqs --No information here
    (Mine True _) -> tankSurrounding grid inferred rest (minesNum-1) unknownSqs --Flagged mine
    (Hidden _ True _) -> error "Computer has incorrectly flagged a square which doesn't have a mine" --Debugging
    _ -> tankSurrounding grid inferred rest minesNum (coords:unknownSqs) --May or may not be a mine

playMove :: Grid -> InferredMove -> Grid
playMove grid (FlagSq coords) = placeFlag grid coords
playMove grid (ClearSq coords) = fst $ clearSquare grid coords

intersect :: Eq a => [a] -> [a] -> [a] 
intersect [] list = list
intersect (x:xs) list = if elem x list then (x:(intersect xs list)) else intersect xs list

coordsOfInferred :: InferredMove -> (Int, Int)
coordsOfInferred (FlagSq coords) = coords
coordsOfInferred (ClearSq coords) = coords

getInferredValue :: Grid -> (Int, Int) -> [InferredMove] -> Square
getInferredValue grid searchCoords [] = lookupSquare grid searchCoords --Haven't inferred a value for this so use what we know
getInferredValue grid searchCoords ((FlagSq coords):rest) = if searchCoords == coords then (Mine True searchCoords) else getInferredValue grid searchCoords rest
getInferredValue grid searchCoords ((ClearSq coords):rest) = if searchCoords == coords then (Clear 0 searchCoords) else getInferredValue grid searchCoords rest
--Don't want to run analysis on this so just say it has no neighbouring mines

--Bool represents whether we hit a mine or not. 
--If we were able to deduce a move then we know it can't have been a mine.
computerMove :: Grid -> StdGen -> (Grid, Bool)  --If we fail to make a move, just make a random move.
computerMove grid gen = if trivialChange then (trivialUpdatedGrid, True) 
                        else if complexChange then (complexUpdatedGrid, True)
                        else makeGuess complexUpdatedGrid sqProbs gen 
    where (trivialUpdatedGrid, trivialChange, sqProbs) = makeTrivialMove grid (0, 0) []
          (complexUpdatedGrid, complexChange) = tankAlgorithm trivialUpdatedGrid sqProbs

--The bool indicates whether we have successfully made a move. We can stop once we have.
--The SquareProb list is used if we fail to make any trivial moves
makeTrivialMove :: Grid -> (Int, Int) -> [SquareProb] -> (Grid, Bool, [SquareProb])
makeTrivialMove grid (row, col) sqProbs
    | row == heightOfMap = (grid, False, sqProbs)
    | col == widthOfMap = makeTrivialMove grid (row+1, 0) sqProbs
    | otherwise = if changed then (newGrid, changed, newSqProbs) else makeTrivialMove newGrid (row, col+1) newSqProbs
        where (newGrid, changed, newSqProbs) = checkSquare grid (row, col) sqProbs

checkSquare :: Grid -> (Int, Int) -> [SquareProb] -> (Grid, Bool, [SquareProb])
checkSquare grid coords sqProbs = 
    case lookupSquare grid coords of (Clear 0 _) -> (grid, False, sqProbs) --No information here as all adjacent tiles will already be cleared
                                     (Clear n _) -> checkSurrounding grid (getSurrounding coords) n [] sqProbs
                                     otherwise -> (grid, False, sqProbs) --No information here

checkSurrounding :: Grid -> [(Int, Int)] -> Int -> [(Int, Int)] -> [SquareProb] -> (Grid, Bool, [SquareProb])
checkSurrounding  grid [] surroundingMines unknownSqs sqProbs
    | surroundingMines == (length unknownSqs) && (length unknownSqs) /= 0 = (placeFlag grid (head unknownSqs), True, sqProbs)   --Place a flag
    | surroundingMines == 0 && (length unknownSqs) /= 0 = (fst (clearSquare grid (head unknownSqs)), True, sqProbs)  --Clear a square
    | otherwise = (grid, False, addNewSqProbs sqProbs unknownSqs (probOfEvent surroundingMines (length unknownSqs))) --We have failed to work out anything useful
checkSurrounding  grid (coords:rest) surroundingMines unknownSqs sqProbs = 
    case lookupSquare grid coords of (Clear _ _) -> checkSurrounding grid rest surroundingMines unknownSqs sqProbs  --Already clear so we don't care
                                     (Hidden _ True _) -> error "Computer has incorrectly flagged a square which doesn't have a mine" --Debugging
                                     (Mine True _) -> checkSurrounding grid rest (surroundingMines-1) unknownSqs sqProbs --Flagged mine
                                     _ -> checkSurrounding grid rest surroundingMines (coords:unknownSqs) sqProbs --May or may not be a mine

--Need to check coords haven't already been added. Maybe use a tree instead, if they have go with higher prob one to be safe
addNewSqProbs :: [SquareProb] -> [(Int, Int)] -> Double -> [SquareProb]
addNewSqProbs sqProbs [] _ = sqProbs
addNewSqProbs sqProbs (sq:rest) prob = addNewSqProbs ((SquareProb sq prob):sqProbs) rest prob
    
getDefaultProb :: Grid -> Double
getDefaultProb (Grid _ _ _ numLeft numFlagged) = probOfEvent numMines sqsLeft
    where numMines = numberOfMines - numFlagged
          sqsLeft = numLeft + numberOfMines

probOfEvent :: Int -> Int -> Double
probOfEvent event sampleSpace = (fromIntegral event) / (fromIntegral sampleSpace)

--Check if a grid has been changed. Only check number of squares revealed and number of flags.
--This means this function should never be used if the grids are different to begin with
gridChanged :: Grid -> Grid -> Bool
gridChanged ( Grid _ _ _ numLeft1 numFlagged1 ) ( Grid _ _ _ numLeft2 numFlagged2 ) 
    = numLeft1 /= numLeft2 || numFlagged1 /= numFlagged2 

--Get computer to solve whole grid. Might remove this
{-computerSolve :: Grid -> Grid   --If we have changed something (either adding a flag or clearing a tile) go through grid again
computerSolve grid = if gridChanged grid newGrid then computerSolve newGrid else newGrid --If we have changed something (either adding a flag or clearing a tile) go through grid again
    where newGrid = makeTrivialMove grid (0, 0)
    -}