{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE OverloadedStrings #-}
module HandParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
-- import qualified Data.Text as T

import Cards
import Data.Text (Text)
import qualified Data.Functor.Identity as Func.Id

-- Parser type synonym: 
-- type Parser = {custom error component} {input stream type}
type Parser = Parsec Void String

-- once parsed, check only 5 cards, check for repeats of same card.
handFromStr :: String -> Maybe [Card]
handFromStr str
    | length cards == 5 = Just parseHand handTest
    | otherwise = Nothing
    where cards = handTest

-- this is for 'chaining together' all the cards
-- parses all occurrences of c until finished
-- sep is what separates the cards. 
parseHand :: Parser [Card] -> Parser ([Card] -> [Card] -> [Card]) -> Parser [Card]
parseHand x sep = do
    a <- x
    restOf a
    where restOf a = (do
            s <- sep
            b <- x
            restOf(s a b)
            ) <|> return a


parseCard :: Parser Card
parseCard = do
    v <- valChar
    s <- suitChar
    space -- spaceChar
    return (Card v s)

pokerChar = loop
    where loop = (valChar <|> suitChar) >> loop


parseValue :: Parser Value
parseValue = do
    return valChar


valChar :: (MonadParsec e s m, Token s ~ Value ) => m (Token s)
valChar = anySingle <?> "card value (i.e. A,2,3,..)"
-- satisfy isCardValue <?> ...

suitChar :: (MonadParsec e s m, Token s ~ Char) => m Char
-- suitChar :: ParsecT Void String Func.Id.Identity Char
suitChar = satisfy isCardValue <?> "card suit (h,d,c,s)"



-- parseCard :: Parser Char
-- parseCard c = 's'

handTest :: [Card]
handTest = [Card Four Clubs, Card Five Spades, Card Ace Diamonds, Card King Clubs, Card Five Clubs]

-- operator to test if in certain range
infixr 5 <=&<=
(<=&<=) :: Ord a => a -> (a,a) -> Bool
x <=&<= (y,z) = y <= x && x <= z