{-# LANGUAGE BlockArguments #-}
module Cards where

import GHC.Read (parens)
import Text.Read (readPrec)
import Data.List (elemIndex, intersperse)
import Data.Maybe ( fromJust )

-- suits do not derive Ord because there is no ranking of suits in poker
data Suit = Diamonds | Hearts | Clubs | Spades deriving (Eq, Bounded, Enum)

-- 'value' is the card number - two is the lowest ranked and ace is the highest
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
                | Jack | Queen | King | Ace
                deriving (Eq, Enum, Ord, Bounded)

-- record syntax to define suit and number functions
data Card = Card {
    value :: Value,
    suit  :: Suit
    } deriving Eq


tupleToCard :: (Value, Suit) -> Card
tupleToCard = uncurry Card

instance Show Card where
    show (Card v s) = show v ++ show s

instance Show Suit where
    show Diamonds = "♦" -- these are for showing NOT FOR PARSING, parsing uses the suitChars character array
    show Hearts = "♥"
    show Clubs = "♣"
    show Spades = "♠"

suitChars :: [Char]
suitChars = "dhcs"

instance Show Value where
    show Ten = "T"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"
    show x = show $ fromEnum x + 2 -- since they are all in order can just be converted from the Enum number

valueChars :: [Char]
valueChars = ['2' .. '9'] ++ "TJQKA"

-- range can be use since they both derive bounded
suits :: [Suit]
suits = [(minBound :: Suit) ..]

values :: [Value]
values = [(minBound :: Value) ..]


class SuitOrValue a where
    fromChar :: Char -> a

instance SuitOrValue Suit where
    fromChar c = toEnum (fromJust $ i c) -- i is a Maybe Int
        where i = flip elemIndex suitChars -- index of the suit in the char list, will correspond to the enum value as it is in the same order

instance SuitOrValue Value where
    fromChar c = toEnum (fromJust $ i c)
      where i = flip elemIndex valueChars


-- this is used exclusively in testing:
newtype Hand = H [Card]

instance Show Hand where
    show (H xs) = foldr (concWSpace . show) "" xs
        where
            concWSpace x "" = x
            concWSpace x y = x ++ " " ++ y
