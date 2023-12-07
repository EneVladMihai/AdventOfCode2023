import System.Environment
import Data.List (group, sort, sortBy)
import Data.Char (isDigit, digitToInt)
import Control.Arrow ( Arrow((&&&)) ) 

-- Part 1
data Hand = Hand {
    cards :: [Int],
    bid :: Int,
    handType :: Int
} deriving (Eq, Show)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    compare (Hand a_cards _ a_handType) (Hand b_cards _ b_handType) 
        | a_handType /= b_handType = compare a_handType b_handType
        | otherwise = compare a_cards b_cards

cardToInt :: Char -> Int
cardToInt c
    | c == 'A' = 14
    | c == 'K' = 13
    | c == 'Q' = 12
    | c == 'J' = 11
    | c == 'T' = 10
    | isDigit c  = digitToInt c

parse :: String -> Hand
parse line = Hand (cardToInt <$> cards) (read bid) handType where
    [cards, bid] = words line
    handType = getHandType getFrequencies cards

getHandType :: (String -> [Int]) -> String -> Int
getHandType freqCalculator cards
    | isFiveOfAKind cardsF = 7
    | isFourOfAKind cardsF = 6
    | isFullHouse cardsF = 5
    | isThreeOfAKind cardsF = 4
    | isTwoPair cardsF = 3
    | isOnePair cardsF = 2
    | isHighCard cardsF = 1
    where cardsF = freqCalculator cards

getFrequencies :: String -> [Int]
getFrequencies = sortBy (flip compare) . fmap length . group . sort

isFiveOfAKind :: [Int] -> Bool
isFiveOfAKind = (==[5])

isFourOfAKind :: [Int] -> Bool
isFourOfAKind = (==[4,1])

isFullHouse :: [Int] -> Bool
isFullHouse = (==[3,2])

isThreeOfAKind :: [Int] -> Bool
isThreeOfAKind = (== [3, 1, 1])

isTwoPair :: [Int] -> Bool
isTwoPair = (==[2, 2, 1])

isOnePair :: [Int] -> Bool
isOnePair = (==[2, 1, 1, 1])

isHighCard :: [Int] -> Bool
isHighCard = (==[1, 1, 1, 1, 1])

score :: [Hand] -> Int
score hands = sum $ (\(rank, Hand _ bid _) -> rank * bid) <$> rankedHands where
    rankedHands = zip [1..(length hands)] $ sort hands

-- Part 2
countJokers :: String -> Int
countJokers = length . filter (== 'J')

getFrequencies2 :: String -> [Int]
getFrequencies2 cards = (jokers + highestFreq):rest where 
    jokers = countJokers cards
    (highestFreq:rest) = if jokers == 5 
        then [0]
        else sortBy (flip compare) . fmap length . group . sort $ filter (/= 'J') cards

cardToInt2 :: Char -> Int
cardToInt2 c
    | c == 'A' = 14
    | c == 'K' = 13
    | c == 'Q' = 12
    | c == 'J' = 1
    | c == 'T' = 10
    | isDigit c  = digitToInt c

parse2 :: String -> Hand
parse2 line = Hand (cardToInt2 <$> cards) (read bid) handType where
    [cards, bid] = words line
    handType = getHandType getFrequencies2 cards

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print $ score $ parse <$> rows
    print $ score $ parse2 <$> rows