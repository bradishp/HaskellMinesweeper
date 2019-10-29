module Main where

import Lib
import System.Random
import System.IO (stdin)

data Tile = Tile (Int, Int) Bool Bool Int 

newtype Board = Board [ [Tile] ]

instance Show Board where
    show ( Board [] ) = "\n"
    show ( Board (x:xs) ) = show x ++ "\n" ++ ( show (Board xs) )

instance Show Tile where
    show (Tile (x, y) mine False num) = show " " --mine
    show (Tile (x, y) mine True num) = show num
        

main :: IO ()
main = do
    generator <- newStdGen
    let randomPairs = genRandomPairs generator 8 []
    let myMap = createMap
    let minedMap = placeMines myMap randomPairs
    print minedMap
    turn minedMap 3

turn :: Board -> Int -> IO ()
turn myMap 0 = print "Hello"
turn myMap c = do
    colString <- getLine     
    let col = read colString :: Int
    rowString <- getLine     
    let row = read rowString :: Int
    let myNewMap = selectRow row col revealTile myMap
    print myNewMap
    turn myNewMap (c-1)


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
placeMines board ((x, y):rest) = placeMines (selectRow y x placeMine board) rest

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
genRandomPairs g n list = if elem (row, col) list then genRandomPairs g3 n list else genRandomPairs g3 (n-1) ((col, row):list) --Make sure the tile doesn't already have a mine in it
    where (row, g2) = randomR (0, 7) g
          (col, g3) = randomR (0, 7) g2
