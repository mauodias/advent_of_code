import qualified Data.Map as Map
import Utils (readColumnFile)
import Distribution.ModuleName ()

countOccurrences :: (Ord k) => [k] -> Map.Map k Int
countOccurrences = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

main :: IO ()
main = do
  [locations, list2] <- readColumnFile "input"
  let occurrences = countOccurrences list2
  let similarity = foldl (\acc x -> acc + x * Map.findWithDefault 0 x occurrences) 0 locations
  print similarity