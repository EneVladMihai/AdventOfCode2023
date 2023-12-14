import System.Environment
import Data.List (findIndex, elemIndex, transpose)
import Data.Maybe (fromMaybe)

-- Part 1
solveRow :: Int -> String -> Int
solveRow _ [] = 0
solveRow dx s = (sum $ map (+dx) [sqIdx - os + 1.. sqIdx]) + (solveRow (dx+sqIdx+1) rest)
    where
        sqIdx = fromMaybe (length s) (elemIndex '#' s)
        os = length.filter (=='O') $ take sqIdx s
        rest = drop (sqIdx+1) s

solve :: [String] -> Int
solve = sum.map (solveRow 0)


main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content
    let input = replicate (length.head $ rows) '#' : rows

    print $ solve.transpose.reverse $ input