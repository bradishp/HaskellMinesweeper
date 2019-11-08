{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.Random
import System.IO
import AutoSolver
import Minesweeper

--Scotty/Blaze UI stuff
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as RH

--Ideas
-- Pass stdGen in some state so program isn't as messy
-- Remove coords from square as they aren't really needed and use up a lot of space

--Setup the game
main :: IO ()
main = do
    S.scotty 3000 $ do
        S.get "/" $ do
            blaze $ render
    
        S.get "/greet/:name" $ do
            --name <- T.unpack <$> S.param "name"
            --let row2 = read name :: Int
            S.html $ mconcat [  "Hello there ",  "name" ]

--Render the blaze template and import the view
--Gotten from http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html
--blaze ::
blaze = S.html . RH.renderHtml


render = do
    H.html $ do
        H.body $ do
            H.h1 "My todo list"
            H.ul $ do
                H.li "learn haskell"
                H.li "make a website"
        H.div $ do
            H.a "See breakdown of other section."
