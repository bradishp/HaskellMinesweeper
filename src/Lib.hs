module Lib
    ( intersect
    ) where

intersect :: Eq a => [a] -> [a] -> [a] 
intersect [] list = []
intersect (x:xs) list = if elem x list then (x:(intersect xs list)) else intersect xs list
