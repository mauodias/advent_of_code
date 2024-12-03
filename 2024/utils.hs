module Utils(readColumnFile,readRowFile) where

import Data.List (transpose)

readColumnFile :: FilePath -> IO [[Int]]
-- Read a file with a number of columns of integers and return the columns as several lists
readColumnFile filePath = do
  contents <- readFile filePath
  let linesFromFile = lines contents
  let wordsList = map words linesFromFile
  let columns = transpose $ map (map read) wordsList
  return columns

readRowFile :: FilePath -> IO [[Int]]
-- Read a file with a number of rows of integers and return the rows as several lists
readRowFile filePath = do
  contents <- readFile filePath
  let linesFromFile = lines contents
  let wordsList = map words linesFromFile
  let rows = map (map read) wordsList
  return rows