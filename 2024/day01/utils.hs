module Utils(readColumnFile) where

readColumnFile :: FilePath -> IO ([Int], [Int])
-- Read a file with two columns of integers and return the columns as two lists
readColumnFile filePath = do
  contents <- readFile filePath
  let linesFromFile = lines contents
  let wordsList = map words linesFromFile
  let (list1, list2) = unzip $ map (\[x, y] -> (read x :: Int, read y :: Int)) wordsList
  return (list1, list2)