{-# LANGUAGE BlockArguments #-}
module Cards where

import GHC.Read (parens)
import Text.Read (readPrec)

-- suits do not derive Ord because there is no ranking of suits in poker
data Suit = Diamonds | Hearts | Clubs | Spades deriving (Eq)

-- newtype Suit = Suit Char deriving (Eq)

-- newtype Value = NumVal Int | CharVal Char
--     deriving (Eq, Enum)
-- Two is 2 -> Ace is 14 

-- instance Enum Value where
--     toEnum 0 = NumVal 2
--     toEnum 1 = NumVal 3

-- 'value' is the card number - two is the lowest ranked and ace is the highest
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
                | Jack | Queen | King | Ace
                deriving (Eq, Enum, Ord)

-- record syntax to define suit and number functions
data Card = Card {
    value :: Value,
    suit  :: Suit
    }

tupleToCard :: (Value, Suit) -> Card
tupleToCard = uncurry Card

instance Show Card where
    show (Card v s) = show v ++ show s

instance Show Suit where
    show Diamonds = "d"
    show Hearts = "h"
    show Clubs = "c"
    show Spades = "s"

instance Show Value where
    show Ten = "T"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"
    show x = show $ fromEnum x + 2

instance Read Suit where 
    readsPrec _ "d" = Diamonds


isSuit :: Char -> Bool
isSuit = flip elem ['d', 'h', 'c', 's']

isCardValue :: Char -> Bool
isCardValue = flip elem $ ['2'..'9'] ++ ['T', 'J', 'Q', 'K', 'A'] 