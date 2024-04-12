import System.IO
import Text.Printf
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

---------------------- Monkey problem ----------------------

getEntry :: [[String]] -> String -> [String]
getEntry ([m, p1, op, p2]:xs) e
    | m == e = [m, p1, op, p2]
    | otherwise = getEntry xs e
getEntry ([m, v]:xs) e
    | m == e = [m, v]
    | otherwise = getEntry xs e
getEntry _ b = do 
    -- error "Error in parsing input"
    error b

computeMonkey :: [[String]] -> [String] -> Double
computeMonkey xs [m, p1, op, p2]
    | op == "+" = computeMonkey xs (getEntry xs p1) + computeMonkey xs (getEntry xs p2)
    | op == "-" = computeMonkey xs (getEntry xs p1) - computeMonkey xs (getEntry xs p2)
    | op == "*" = computeMonkey xs (getEntry xs p1) * computeMonkey xs (getEntry xs p2)
    | op == "/" = computeMonkey xs (getEntry xs p1) / computeMonkey xs (getEntry xs p2)
    | otherwise = error "Error determainint operator"
computeMonkey _ [m, v] = read v
computeMonkey _ _ = error "Error computeMonkey"

main :: IO ()
main = do
    lines <- readFileLines "input"
    printf "%.0f\n" (computeMonkey lines (getEntry lines "root"))
