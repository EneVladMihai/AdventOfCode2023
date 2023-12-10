import System.Environment
import Data.Char (isDigit)
import Data.Bifunctor ( Bifunctor(bimap) )

-- Part 1
data EngingNumber = EngingNumber
    { value :: Int,
        row :: Int,
        startCol :: Int,
        endCol :: Int} deriving (Show, Eq)

data Symbol = Symbol {sVal::Char, sRow::Int, sCol::Int} deriving (Show)

indexed :: [b] -> [(Int, b)]
indexed = zip [1..]

parseNumbers :: Int -> [(Int, Char)] -> [EngingNumber]
parseNumbers _ [] = []
parseNumbers rowIndex row
    | length (filter (isDigit.snd) row) == 0 = []
    | otherwise = EngingNumber numVal rowIndex startCol endCol : parseNumbers rowIndex rest
        where
            skipped = dropWhile (not . isDigit . snd) row
            num = takeWhile (isDigit . snd) skipped
            numVal = read $ snd <$> num
            startCol = fst . head $ num
            endCol = startCol + length num - 1
            rest = drop (length num) skipped

parseSymbols :: Int -> [(Int, Char)] -> [Symbol]
parseSymbols rowIndex row = (\s -> Symbol (snd s) rowIndex (fst s)) <$> symbols where
    isSymbol c = (not.isDigit) c && c/='.'
    symbols = filter (isSymbol.snd) row

parse :: [(Int, String)] -> ([EngingNumber], [Symbol])
parse [] = ([], [])
parse (row:rs) = Data.Bifunctor.bimap
  (parseNumbers (fst row) (indexed $ snd row) ++)
  (parseSymbols (fst row) (indexed $ snd row) ++) (parse rs)

isVecinity :: EngingNumber -> Symbol -> Bool
isVecinity (EngingNumber _ row startCol endCol) (Symbol _ symRow symCol) =
    abs (row - symRow) <= 1
    && startCol - 1 <= symCol && symCol <= endCol + 1

isPartNumber :: EngingNumber -> [Symbol] -> Bool
isPartNumber en = any (isVecinity en)

part1 :: [EngingNumber] -> [Symbol] -> Int
part1 numbers symbols = sum $ value <$> filter (`isPartNumber` symbols) numbers

-- Part 2
gears :: [EngingNumber] -> [Symbol] -> [[Int]]
gears engineNumbers symbols = filter ((==2).length) possibleGears where
    possibleGears = (\s -> [value en | en <- engineNumbers, isVecinity en s]) 
        <$> filter ((=='*').sVal) symbols

part2 :: [EngingNumber] -> [Symbol] -> Int
part2 ens syms = sum . fmap product $ gears ens syms


main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print $ uncurry part1.parse.indexed $ rows
    print $ uncurry part2.parse.indexed $ rows
