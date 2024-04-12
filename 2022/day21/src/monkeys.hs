import System.IO
import Text.Printf
import System.Environment (getArgs, getExecutablePath)
import System.Directory (setCurrentDirectory)
import System.FilePath.Posix (takeDirectory)
import Debug.Trace

---------------------- IO Stuff for input file reading ----------------------

-- Function to read a file line by line and format each line into a list of strings
readFileLines :: FilePath -> IO [[String]]
readFileLines filePath = do
    -- Open the file
    handle <- openFile filePath ReadMode
    -- Read lines from the file and format each line into a list of strings
    lines <- readLines handle
    -- Close the file handle
    hClose handle
    return lines

-- Helper function to read lines from a file and format each line into a list of strings
readLines :: Handle -> IO [[String]]
readLines handle = do
    eof <- hIsEOF handle
    if eof
        then return []
        else do
            line <- hGetLine handle
            -- Split the line by spaces and format into a list of strings
            let formattedLine = words line
            rest <- readLines handle
            return (formattedLine : rest)

-- Function to print lines with line breaks after each line
printLines :: [[String]] -> IO ()
printLines [] = return ()
printLines (line:rest) = do
    putStrLn (unwords line)
    printLines rest

getInputFile :: [String] -> String
getInputFile (x:xs) = "../src/" ++ x
getInputFile _ = "../src/input"

getEntry :: [[String]] -> String -> [String]
getEntry ([m, p1, op, p2]:xs) e
    | m == e = [m, p1, op, p2]
    | otherwise = getEntry xs e
getEntry ([m, v]:xs) e
    | m == e = [m, v]
    | otherwise = getEntry xs e
getEntry _ b = do
    error "Error in parsing input"

---------------------- Monkey problem ----------------------

data BinTree = Value String Double
    | AddNode String BinTree BinTree
    | SubtractNode String BinTree BinTree
    | MultiplyNode String BinTree BinTree
    | DivideNode String BinTree BinTree
    deriving (Show)

createTree :: [[String]] -> [String] -> BinTree
createTree xs [m, p1, op, p2]
    | op == "+" = AddNode m (createTree xs (getEntry xs p1)) (createTree xs (getEntry xs p2))
    | op == "-" = SubtractNode m (createTree xs (getEntry xs p1)) (createTree xs (getEntry xs p2))
    | op == "*" = MultiplyNode m (createTree xs (getEntry xs p1)) (createTree xs (getEntry xs p2))
    | op == "/" = DivideNode m (createTree xs (getEntry xs p1)) (createTree xs (getEntry xs p2))
createTree xs [m, v] = Value m (read v)

evaluate :: BinTree -> Double
evaluate (Value _ v) = v
evaluate (AddNode _ a b) = evaluate a + evaluate b
evaluate (SubtractNode _ a b) = evaluate a - evaluate b
evaluate (MultiplyNode _ a b) = evaluate a * evaluate b
evaluate (DivideNode _ a b) = evaluate a / evaluate b

inTree :: BinTree -> String -> Bool
inTree (Value s _) n = s == n
inTree (AddNode s left right) n = s == n || inTree left n || inTree right n
inTree (SubtractNode s left right) n = s == n || inTree left n || inTree right n
inTree (MultiplyNode s left right) n = s == n || inTree left n || inTree right n
inTree (DivideNode s left right) n = s == n || inTree left n || inTree right n

solve :: BinTree -> String -> Double -> Double
solve (Value s _) n v
    | n == s = v
solve (AddNode _ left right) n v
    | inTree left n = solve left n (v - evaluate right)
    | inTree right n = solve right n (v - evaluate left)
solve (SubtractNode _ left right) n v
    | inTree left n = solve left n (v + evaluate right)
    | inTree right n = solve right n (evaluate left - v)
solve (MultiplyNode _ left right) n v
    | inTree left n = solve left n (v / evaluate right)
    | inTree right n = solve right n (v / evaluate left)
solve (DivideNode _ left right) n v
    | inTree left n = solve left n (evaluate right * v)
    | inTree right n = solve right n (evaluate left / v)

part2 :: BinTree -> String -> Double
part2 (AddNode s left right) n
    | inTree left n = solve left n (evaluate right)
    | inTree right n = solve right n (evaluate left)
part2 (SubtractNode s left right) n
    | inTree left n = solve left n (evaluate right)
    | inTree right n = solve right n (evaluate left)
part2 (MultiplyNode s left right) n
    | inTree left n = solve left n (evaluate right)
    | inTree right n = solve right n (evaluate left)
part2 (DivideNode s left right) n
    | inTree left n = solve left n (evaluate right)
    | inTree right n = solve right n (evaluate left)

main :: IO ()
main = do
    p <- getExecutablePath
    setCurrentDirectory (takeDirectory p)

    args <- getArgs
    lines <- readFileLines (getInputFile args)

    putStrLn "Part 1:"
    let tree = createTree lines (getEntry lines "root")
    printf "root = %.0f\n\n" (evaluate tree)
    putStrLn "Part 2:"
    printf "humn = %.0f\n\n" (part2 tree "humn")
