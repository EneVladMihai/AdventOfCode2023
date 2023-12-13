import System.Environment
import Data.List (findIndex, isPrefixOf)
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

transpose :: [String] -> [String]
transpose rows = (\(colHeadIdx, _) -> map (!! colHeadIdx) rows) <$> indexed (head rows)

isMirrorIdx :: [String] -> Int -> Bool
isMirrorIdx rows i
    | null left || null right = False
    | otherwise = left `isPrefixOf` right || right `isPrefixOf` left
        where
            left = reverse $ take i rows
            right = drop i rows

findMirrorIdx :: [String] -> Maybe Int
findMirrorIdx rows = findIndex (isMirrorIdx rows) [0..length rows]

summarise :: [String] -> (Maybe Int, Maybe Int)
summarise = (findMirrorIdx.transpose) &&& findMirrorIdx

part1 :: [[String]] -> Int
part1 input = cols + 100 * rows where
    (cols, rows) = (sum.catMaybes *** sum.catMaybes) . unzip . fmap summarise $ input

-- Part 2

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print $ part1.parse $ rows