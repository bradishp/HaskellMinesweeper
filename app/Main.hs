module Main where

import Lib
import System.Random
import System.IO

data GameStatus = Ongoing 
                  | Victory 
                  | GameOver

data Tile = Clear Int
            | Hidden Int Bool
            | Mine Bool

newtype Board = Board [ [Tile] ]

newtype NoQuotes = NoQuotes String      --Taken from https://www.reddit.com/r/haskell/comments/blxveo/print_out_string_without_double_quotes/
instance Show NoQuotes where show (NoQuotes str) = str

instance Show Board where
    show ( Board [] ) = ""
    show ( Board (x:xs) ) = show x ++ "\n" ++ ( show (Board xs) )

instance Show Tile where
    show (Clear n) = show n --Revealed tile
    show (Hidden n _) = show (NoQuotes "U") --Unknown
    show (Mine _) = show (NoQuotes "X") --Mine

--Declare Constants
numberOfMines :: Int
numberOfMines = 2
sizeOfMap :: Int
sizeOfMap = 3   --Map is always a square

--Setup the game
main :: IO ()
main = do
    generator <- newStdGen
    let randomPairs = genRandomPairs generator numberOfMines [] --Pass in an empty list which we can recursively add the pairs to so long as they haven't already been added
    let myMap = createMap
    let minedMap = placeMines myMap randomPairs
    print minedMap
    turn minedMap Ongoing

--Run until the game is finished
turn :: Board -> GameStatus -> IO ()
turn myMap Victory = do
    print myMap
    print "Congratulations. The mines can now be cleared. You have won!"
turn myMap GameOver = do
    print myMap
    print "You have hit a mine and been violently killed. Sorry!"
turn myMap Ongoing = do    
    print "Select a row"
    rowString <- getLine     
    let row = read rowString :: Int
    print "Select a column"
    colString <- getLine     
    let col = read colString :: Int
    let (myNewMap, tile) = updateMap row col revealTile myMap
    print myNewMap
    turn myNewMap (checkGameStatus myNewMap tile)

--Create the map
createMap :: Board
createMap = Board (recCreateMap sizeOfMap)
    where recCreateMap 0 = []
          recCreateMap row = (genRow sizeOfMap):( recCreateMap (row-1) )

genRow :: Int -> [Tile]
genRow 0 = []
genRow col = ( Hidden 0 False ):(genRow (col-1) )

--Helper functions for updating the map
updateMap :: Int -> Int -> (Tile -> Tile) -> Board -> (Board, Tile)
updateMap 0 col op (Board (x:xs)) = ( Board (newMap:xs), resultTile )
    where (newMap, resultTile) =  (selectTile col op x)
updateMap row col op (Board (x:xs)) = ( Board (x:newMap), resultTile )
    where (Board newMap, resultTile) = updateMap (row-1) col op (Board xs)

selectTile :: Int -> (Tile -> Tile) -> [Tile] -> ([Tile], Tile)
selectTile 0 op (t:ts) = (newT:ts, newT)
    where newT = op t
selectTile n op (t:ts) = ( (t:newList), resultTile )
    where (newList, resultTile) = selectTile (n-1) op ts

--Reveal a mine
revealTile :: Tile -> Tile
revealTile (Clear n) = Clear n                                    
revealTile (Hidden n _) = (Clear n)
revealTile (Mine flagged) = Mine flagged

cascadeReveal :: Tile -> Tile
cascadeReveal (Clear n) = (Clear n)
cascadeReveal (Hidden n _) = (Clear n)
cascadeReveal (Mine flagged) = Mine flagged 

--Place the mines on the board
placeMines :: Board -> [ (Int, Int) ] -> Board
placeMines board [] = board
placeMines board ((row, col):rest) = placeMines ( incrementMineCount newMap (row, col) ) rest
    where (newMap, _) = updateMap row col placeMine board

placeMine :: Tile -> Tile
placeMine (Hidden _ flagged) = Mine flagged
placeMine _ = error "Can't place a mine there"

incrementMineCount :: Board -> (Int, Int) -> Board
incrementMineCount map (row, col) = recIncrementMineCount map [(row-1, col-1), (row-1, col), (row-1, col+1), (row, col-1), (row, col+1), (row+1, col-1), (row+1, col), (row+1, col+1)]
    where recIncrementMineCount map [] = map
          recIncrementMineCount map ((row, col):rest) | inRange row col = recIncrementMineCount newMap rest
                                                      | otherwise = recIncrementMineCount map rest
            where (newMap, _) = updateMap row col incrMineCount map

incrMineCount :: Tile -> Tile
incrMineCount (Hidden n flagged) = (Hidden (n+1) flagged)
incrMineCount (Mine flagged) = (Mine flagged)

--Check game status
checkGameStatus :: Board -> Tile -> GameStatus
checkGameStatus _ (Mine _) = GameOver
checkGameStatus map _ = if checkForWin map then Victory else Ongoing

checkForWin :: Board -> Bool
checkForWin (Board []) = True
checkForWin (Board (row:rest)) = if checkRowForWin row then (checkForWin (Board rest)) else False

checkRowForWin :: [Tile] -> Bool
checkRowForWin [] = True
checkRowForWin ((Clear _):rest) = checkRowForWin rest
checkRowForWin ((Hidden _ _):rest) = False
checkRowForWin ((Mine _):rest) = checkRowForWin rest

--Helper functions
inRange :: Int -> Int -> Bool
inRange row col = row >= 0 && row < sizeOfMap && col > 0 && col < sizeOfMap

genRandomPairs :: StdGen -> Int -> [(Int, Int)] -> [(Int, Int)]
genRandomPairs _ 0 list = list
genRandomPairs g n list = if elem (row, col) list then genRandomPairs g3 n list else genRandomPairs g3 (n-1) ((row, col):list) --Make sure the tile doesn't already have a mine in it
    where (row, g2) = randomR (0, sizeOfMap-1) g
          (col, g3) = randomR (0, sizeOfMap-1) g2
