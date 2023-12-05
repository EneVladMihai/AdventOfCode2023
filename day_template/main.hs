import System.Environment

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let rows = lines content

    print ""