import Data.List (delete, minimum)
import Utils (readColumnFile)

popSmallest :: [Int] -> (Int, [Int])
-- Calculate the minimum value of a list and return it along with the list without that value
popSmallest xs = (minval, delete minval xs)
  where minval = minimum xs

calculateDifference :: [Int] -> [Int] -> [Int]
-- Calculate the difference between the smallest values of two lists
calculateDifference [] _ = []
calculateDifference _ [] = []
calculateDifference xs ys = diff : calculateDifference xs' ys'
  -- Recursive, yay!
  where
    (minX, xs') = popSmallest xs
    (minY, ys') = popSmallest ys
    diff = abs (minX - minY)

sumDifferences :: [Int] -> [Int] -> Int
-- Sum the differences between the smallest values of two lists
sumDifferences xs ys = sum (calculateDifference xs ys)

main :: IO ()
main = do
  -- Read the file and parse the contents. The file is expected to have two columns of integers
  (list1, list2) <- readColumnFile "input"
  -- Calculate the sum of the differences between the smallest values of the two lists
  let result = sumDifferences list1 list2
  print result