module Lib
    ( start
    ) where

import HandParser ( parseHand, handFromStr )

{- NOTE that you are NOT obligated to keep any of the files from 
the skeleton code, including this one. You should give your 
modules sensible names that correspond to their contents. -}

{-
Parse some representation of a set of playing cards
return the highest value poker hand that can be made from the parsed cards.
-}

start :: IO ()
start = do
    startPrompt
    str <- getLine
    -- putStrLn $ handFromStr str
    putStrLn "uncomment line above"

startPrompt :: IO ()
startPrompt = do
    putStrLn "\n\n-------------- WELCOME ! -------------"
    putStrLn "Enter poker cards with spaces in between."
    putStrLn "Values are: A23456789TJQK (note Ten is T not 10)"
    putStrLn "Suits are: 's' for ♠, 'h' for ♥, 'd' for ♦, 'c' for ♣"
    putStrLn "Enter: "