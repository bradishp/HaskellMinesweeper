module Main where

import Lib
import System.Random
import System.IO (stdin)

data Tile = Tile (Int, Int) Bool Bool Int --TODO make this a record

newtype Board = Board [ [Tile] ]

newtype NoQuotes = NoQuotes String      --Taken from https://www.reddit.com/r/haskell/comments/blxveo/print_out_string_without_double_quotes/
instance Show NoQuotes where show (NoQuotes str) = str

instance Show Board where
    show ( Board [] ) = " "
    show ( Board (x:xs) ) = show x ++ "\n" ++ ( show (Board xs) )

instance Show Tile where
    show (Tile _ _ False _) = show (NoQuotes "U") --unknown
    show (Tile _ True _ _) = show (NoQuotes "X") --mine
    show (Tile _ _ True num) = show num

--Declare Constants
numberOfMines :: Int
numberOfMines = 8
sizeOfMap :: Int
sizeOfMap = 8   --Map is always a square

main :: IO ()
main = do
    generator <- newStdGen
    let randomPairs = genRandomPairs generator 8 []
    let myMap = createMap
    let minedMap = placeMines myMap randomPairs
    print minedMap
    turn minedMap False 50

turn :: Board -> Bool -> Int -> IO ()
turn myMap True _ = do
    print myMap
    print "Congratulations. The mines can now be cleared. You have won!"
turn myMap _ 0 = print "Time is up. You have run out of turns."
turn myMap _ c = do    
    print "Select a row"
    rowString <- getLine     
    let row = read rowString :: Int
    print "Select a column"
    colString <- getLine     
    let col = read colString :: Int
    let myNewMap = selectRow row col revealTile myMap
    print myNewMap
    turn myNewMap (checkForWin myNewMap) (c-1)


createMap :: Board
createMap = Board (recCreateMap 0)
    where recCreateMap 8 = []
          recCreateMap row = (genRow row 0):( recCreateMap (row+1) )

genRow :: Int -> Int -> [Tile]
genRow row 8 = []
genRow row n = ( Tile (row, n) False False 0 ):(genRow row (n+1) )

revealTile :: Tile -> Tile
revealTile (Tile (x, y) mine _ num) | mine == True = error "Bam, that was a mine"
                                    | otherwise = (Tile (x, y) mine True num)

placeMines :: Board -> [ (Int, Int) ] -> Board
placeMines board [] = board
placeMines board ((row, col):rest) = placeMines ( incrementMineCount (selectRow row col placeMine board) (row, col) ) rest

placeMine :: Tile -> Tile
placeMine (Tile (x, y) mine revealed num) = (Tile (x, y) True revealed num)

selectRow :: Int -> Int -> (Tile -> Tile) -> Board -> Board
selectRow 0 col op (Board (x:xs)) = Board ((selectTile col op x):xs)
selectRow row col op (Board (x:xs)) = Board (x:result)
    where (Board result) = selectRow (row-1) col op (Board xs)

selectTile :: Int -> (Tile -> Tile) -> [Tile] -> [Tile]
selectTile 0 op (x:xs) = (op x):xs
selectTile n op (x:xs) = x:(selectTile (n-1) op xs)


genRandomPairs :: StdGen -> Int -> [(Int, Int)] -> [(Int, Int)]
genRandomPairs _ 0 list = list
genRandomPairs g n list = if elem (row, col) list then genRandomPairs g3 n list else genRandomPairs g3 (n-1) ((row, col):list) --Make sure the tile doesn't already have a mine in it
    where (row, g2) = randomR (0, 7) g
          (col, g3) = randomR (0, 7) g2

incrementMineCount :: Board -> (Int, Int) -> Board
incrementMineCount map (row, col) = recIncrementMineCount map [(row-1, col-1), (row-1, col), (row-1, col+1), (row, col-1), (row, col+1), (row+1, col-1), (row+1, col), (row+1, col+1)]
    where recIncrementMineCount map ((row, col):rest) | row >= 0 && row < 8 && col > 0 && col < 8 = recIncrementMineCount (selectRow row col incrMineCount map) rest
                                                      | otherwise = recIncrementMineCount map rest
          recIncrementMineCount map [] = map

incrMineCount :: Tile -> Tile
incrMineCount (Tile (x, y) mine revealed num) = (Tile (x, y) mine revealed (num+1))


checkForWin :: Board -> Bool
checkForWin (Board []) = True
checkForWin (Board (row:rest)) = if checkRowForWin row then (checkForWin (Board rest)) else False

checkRowForWin :: [Tile] -> Bool
checkRowForWin [] = True
checkRowForWin ((Tile _ mine revealed _):rest) | mine == True || revealed == True = checkRowForWin rest
                                               | otherwise = False

cascadeReveal :: Tile -> Tile
cascadeReveal (Tile (x, y) mine revealed num) | mine == False && revealed == False && num == 0 = ( Tile (x, y) mine True num )
                                              | otherwise = ( Tile (x, y) mine revealed num )
