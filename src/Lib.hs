module Lib
    ( start
    ) where

import HandParser ( parseHand, handFromStr )
import HandRankings (getHandRanking, Ranking)
import System.Console.ANSI (clearScreen)
import Control.Exception

{- NOTE that you are NOT obligated to keep any of the files from 
the skeleton code, including this one. You should give your 
modules sensible names that correspond to their contents. -}

{-
Parse some representation of a set of playing cards
return the highest value poker hand that can be made from the parsed cards.
-}

start :: IO ()
start = do
    clearScreen
    startPrompt
    str <- getLine
    putStrLn "\n"
    print $ handFromStr str
    print $ getHandRanking (handFromStr str)
    putStrLn "\n"

startPrompt :: IO ()
startPrompt = do
    putStrLn "🃏 ♥♣♦♠ \ESC[1mPOKER HAND EVALUATOR\ESC[0m ♠♦♣♥ 🃏 "
    putStrLn "\n\nEnter a 5 card poker hand... \n Values: A23456789TJQK (!! ten is T) \n Suits: 's'=♠, 'h'=♥, 'd'=♦, 'c'=♣"
    putStrLn "Enter: "