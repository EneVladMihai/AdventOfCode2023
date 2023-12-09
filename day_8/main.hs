import System.Environment
import Data.Map 

-- Part 1
parseNodes = (\(x:xs) -> (x, xs)) . words . Prelude.filter (`notElem` "()=,")

countSteps :: Map String [String]  -> String -> String -> String -> Int
countSteps nodeMap (step:xs) node dest 
    | node == dest = 0
    | otherwise = case step of 
        'L' -> 1 + countSteps nodeMap xs (head children) dest
        'R' -> 1 + countSteps nodeMap xs ((!! 1) children) dest
    where children = findWithDefault [] node nodeMap

cycleInf xs = xs ++ cycleInf xs 

-- Part 2
-- Need a generatlised version of countSteps

countSteps2 :: Map String [String] -> (String -> Bool) -> String -> [String] -> Int
countSteps2 nodeMap isDest (step:xs) nodes
    | all isDest nodes = 0
    | otherwise = case step of 
        'L' -> 1 + countSteps2 nodeMap isDest xs (head <$> children)
        'R' -> 1 + countSteps2 nodeMap isDest xs ((!! 1) <$> children)
    where children = (\n -> findWithDefault [] n nodeMap) <$> nodes

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content
    let instructions = head rows

    let nodeTulples = parseNodes <$> Prelude.drop 2 rows
    let nodes = fromList nodeTulples
    print $ countSteps nodes (cycleInf instructions) "AAA" "ZZZ"

    let startNodes = Prelude.filter ((=='A').last) (fst <$> nodeTulples)
    -- Each '**A' -> '**Z' is a cycle
    let lengths = (\n -> countSteps2 nodes ((=='Z').last) (cycleInf instructions) [n]) <$> startNodes
    print $ Prelude.foldl1 lcm lengths