module HandRankings where

import Data.List (sort, nub)
import HandParser ()
import Cards
    ( Card(value, suit), Suit, Value(King, Two, Three, Four, Five, Ten, Jack, Queen, Ace) )
import Data.Map (fromListWith, toList)

data Ranking = InvalidHand | HighCard | Pair | TwoPair | ThreeOfAKind | Straight
    | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
    deriving (Show, Eq, Ord)

getHandRanking :: [Card] -> Ranking
getHandRanking [] = InvalidHand
getHandRanking cs
    | valEqTo royalStraight cs  && isFlush cs = RoyalFlush 
    | isStraight cs && isFlush cs = StraightFlush -- since it is only five cards, if you have a straight and a flush they must coincide and thus be a straight flush!
    | ofAKind == 4 = FourOfAKind
    | ofAKind == 3 && otherOfAKind == 2 = FullHouse
    | isFlush cs = Flush
    | isStraight cs = Straight
    | ofAKind == 3 = ThreeOfAKind
    | length (filter (==2) frequencies) == 2 = TwoPair -- 2 will appear twice because there are 2 '2 of a kinds' (pairs) so two cards will have a frequeny of 2
    | ofAKind == 2 = Pair
    | otherwise = HighCard
    where
        ofAKind = maximum frequencies -- how often the most common card appears
        otherOfAKind = minimum frequencies -- this is just for three of a kind, where the maximum would be 3 and minimum would be 2 : they add to 5 so can't be any more
        frequencies = map snd $ getFrequencies cs
            -- this gets the frequency integers (second of the tuple)

-- use a map to calculate frequencies of each number, i.e. how often each card appears, stored in a tuple with the value and the frequency
getFrequencies :: [Card] -> [(Value, Int)]
getFrequencies cs = toList $ fromListWith (+) [(value c, 1) | c <- cs]

-- following two functions use the value and suit functions from the record syntax
getValueList :: [Card] -> [Value]
getValueList = map value

getSuitList :: [Card] -> [Suit]
getSuitList = map suit

-- checks if the values are the same as the (sorted) list , such as the royalStraight - means you can have the royal flush in any order etc.
valEqTo :: [Value] -> [Card] -> Bool
valEqTo valList cs = valList == sortedvals cs

-- get values and sort them
sortedvals :: [Card] -> [Value]
sortedvals = sort . getValueList

aceLowStraight :: [Value]
aceLowStraight = [Two, Three, Four, Five, Ace]
royalStraight :: [Value]
royalStraight = [Ten, Jack, Queen, King, Ace]

isFlush :: [Card] -> Bool
isFlush cs = all (== head ss) (tail ss)
    where ss = getSuitList cs

-- check the sorted hand is a straight by checking if each value succeeds the previous - or if it is equal to the ace low straight, since ace can be either low or high and I have defined ace as high in Cards.hs
isStraight :: [Card] -> Bool
isStraight xs = inSequence (sortedvals xs) || valEqTo aceLowStraight xs || valEqTo royalStraight xs
    where
        -- c1 can't be king because if sorted there can't be two after it - Ace is the upper bound - this is the reason the list is given as two terms c1:c2:cs
        inSequence (c1:c2:cs) = (succ c1 == c2 && c1 /= King) && inSequence (c2:cs)
        inSequence _ = True