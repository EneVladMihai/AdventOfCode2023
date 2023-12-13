import System.Environment
import Data.List (findIndex, isPrefixOf, transpose)
import Control.Arrow ((&&&), (***))
import Data.Maybe (catMaybes)

-- Part 1
parse :: [String] -> [[String]]
parse [] = []
parse rows = grid : parse rest where
    grid = takeWhile (/="") rows
    rest = drop (length grid + 1) rows

indexed :: [b] -> [(Int, b)]
indexed = zip [0..]

isMirrorIdx :: [String] -> Int -> Bool
isMirrorIdx rows i
    | null left || null right = False
    | otherwise = and (zipWith (==) (reverse left) right)
        where
            left = take i rows
            right = drop i rows

findMirrorIdx :: [String] -> Maybe Int
findMirrorIdx rows = findIndex (isMirrorIdx rows) [0..length rows]

findReflextion :: [String] -> (Maybe Int, Maybe Int)
findReflextion = (findMirrorIdx.transpose) &&& findMirrorIdx

summarise :: [[String]] -> Int
summarise input = cols + 100 * rows where
    (cols, rows) = (sum.catMaybes *** sum.catMaybes) . unzip . fmap findReflextion $ input

-- Part 2
countDiffs :: Eq a => [a] -> [a] -> Int
countDiffs l1 l2 = sum . map fromEnum $ zipWith (/=) l1 l2

isSmudgedMirrorIdx :: [String] -> Int -> Bool
isSmudgedMirrorIdx rows i
    | null left || null right = False
    | otherwise = 1 == sum (zipWith countDiffs (reverse left) right)
        where
            left = take i rows
            right = drop i rows

findSmudgedIdx :: [String] -> Maybe Int
findSmudgedIdx rows = findIndex (isSmudgedMirrorIdx rows) [0..length rows]

findSmudge :: [String] -> (Maybe Int, Maybe Int)
findSmudge = (findSmudgedIdx.transpose) &&& findSmudgedIdx

summarise2 :: [[String]] -> Int
summarise2 input = cols + 100 * rows where
    (cols, rows) = (sum.catMaybes *** sum.catMaybes) . unzip . fmap findSmudge $ input

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print $ summarise.parse $ rows
    print $ summarise2.parse $ rows