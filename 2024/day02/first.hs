import Utils (readRowFile)

checkSafety :: Int -> [Int] -> Bool
checkSafety _ [] = True
checkSafety prev (x:xs)
    | abs (x - prev) >= 1 
      && abs (x - prev) <= 3 
      && (null xs || (x - prev) * (head xs - x) >= 0)
        = checkSafety x xs
    | otherwise = False


checkSafetyWrapper :: [Int] -> Bool
checkSafetyWrapper (x:xs) = checkSafety x xs

countSafe :: [Bool] -> Int
countSafe = length . filter id


main :: IO ()
main = do
    rows <- readRowFile "input"
    let result = countSafe $ map checkSafetyWrapper rows
    print result