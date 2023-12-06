import System.Environment

-- Part 1
quadraticSolver :: Double -> Double -> Double -> (Double, Double)
quadraticSolver a b c = (min sol1 sol2, max sol1 sol2) where
    sol1 = ((-b) + sqrt (b**2 - 4*a*c)) / (2*a)
    sol2 = ((-b) - sqrt (b**2 - 4*a*c)) / (2*a)

parse = drop 1 . words

isInt x = x == fromInteger (round x)

floor2 :: Double -> Int
floor2 x = floor $ if isInt x then x - 1 else x 

ceil2 :: Double -> Int
ceil2 x = ceiling $ if isInt x then x + 1 else x 

part1 :: [(Double, Double)] -> Int
part1 races = product waysToWin where
    waysToWin = (\(low, high) -> (floor2 high - ceil2 low) + 1) <$> winningStrategyBounds
    winningStrategyBounds = (\(t,d) -> quadraticSolver (-1) t (-d)) <$> races

-- Part 2
-- reuses code from part 1

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let times:distances:xs = lines content 
    let races = zip (read <$> parse times) (read <$> parse distances)
    print $ part1 races

    let read2 = read.concat.parse
    print $ part1 [(read2 times, read2 distances)]


    