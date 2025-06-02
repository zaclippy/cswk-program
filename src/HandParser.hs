{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE OverloadedStrings #-}
module HandParser where

import Text.Megaparsec
    ( (<?>), parse, satisfy, some, Parsec, MonadParsec, Stream(Token) )
import Text.Megaparsec.Char ( space )
import Data.Void ( Void )
import Data.List (nub)


-- import qualified Data.Text as T

import Cards
    ( Card(Card), Suit(Clubs, Spades, Diamonds), Value(Five, Four, Ace, King), SuitOrValue(..), suitChars, valueChars )
import qualified Data.Functor.Identity as Func.Id

-- Parser type synonym: 
-- type Parser = {custom error component} {input stream type}
type Parser = Parsec Void String

-- once parsed, check for repeats of same card.
handFromStr :: String -> [Card]
handFromStr s = 
    case parse parseHand "" s of
        Left _ -> [] 
        Right xs -> 
            -- check for duplicates: nub removes duplicates so if there is a difference when nub is applied then there must be a duplicate
            if xs == nub xs then xs
            else []

parseCard :: Parser Card
parseCard = do
    v <- parseValue -- parses a single char
    s <- parseSuit
    space -- allows space characters between
    return (Card v s)

testHandString :: [Char]
testHandString = "Kd Qs 2h 5c 8d 4c"

parseValue :: Parser Value
parseValue = readSuitOrValue <$> some valChar

parseSuit :: Parser Suit
parseSuit = readSuitOrValue <$> some suitChar

parseHand :: Parser [Card]
parseHand = do
  c1 <- parseCard
  c2 <- parseCard
  c3 <- parseCard
  c4 <- parseCard
  c5 <- parseCard
  return [c1, c2, c3, c4, c5]

readSuitOrValue :: SuitOrValue a => String -> a
readSuitOrValue = fromChar . head -- just takes the first (and only) character of the string

valChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
valChar = satisfy (`elem` valueChars) <?> "card value (i.e. A,2,3,..)"

suitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
suitChar = satisfy (`elem` suitChars) <?> "card suit (h,d,c,s)"