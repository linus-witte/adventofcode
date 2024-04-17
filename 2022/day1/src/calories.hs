import System.IO
import Text.Printf
import System.Environment (getArgs, getExecutablePath)
import System.Directory (setCurrentDirectory)
import System.FilePath.Posix (takeDirectory)
import Debug.Trace
import Data.List (sort)

---------------------- IO Stuff for input file reading ----------------------

-- Function to read a file line by line and format each line into a list of strings
readFileLines :: FilePath -> IO [[Int]]
readFileLines filePath = do
    -- Open the file
    handle <- openFile filePath ReadMode
    -- Read lines from the file and format each line into a list of strings
    lines <- readLines handle
    -- Close the file handle
    hClose handle
    return lines

-- Helper function to read lines from a file and format each line into a list of strings
readLines :: Handle -> IO [[Int]]
readLines handle = do
    eof <- hIsEOF handle
    if eof
        then return []
        else do
            line <- hGetLine handle
            -- Split the line by spaces and format into a list of strings
            let formattedLine = map read (words line)
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

---------------------- Calories problem ----------------------

part1 :: [[Int]] -> Int
part1 = foldr (max . sum) 0

compress :: [[Int]] -> [Int]
compress [x] = [sum x]
compress (x:xs) = sum x : compress xs

part2 :: [Int] -> Int -> Int
part2 [] n = 0
part2 (x:xs) n 
    | n > 0 = x + part2 xs (n - 1)
    | n <= 0 = 0

main :: IO ()
main = do
    p <- getExecutablePath
    setCurrentDirectory (takeDirectory p)

    args <- getArgs
    lines <- readFileLines (getInputFile args)

    print (part1 lines)

    let list = reverse (sort (compress lines))
    print (part2 list 3)

