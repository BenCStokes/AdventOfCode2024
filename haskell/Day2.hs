differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) xs

safe :: [Int] -> Bool
safe xs =
    let diffs = differences xs in
    all (`elem` [-3 .. -1]) diffs || all (`elem` [1..3]) diffs

part1 :: [[Int]] -> Int
part1 = length . filter safe

parse :: String -> [[Int]]
parse = map (map read . words) . lines

--main :: IO ()
--main = getContents >>= print . part1 . parse

data Diff = Safe Int | Unsafe Int

is_safe :: Diff -> Bool
is_safe (Safe _) = True
is_safe (Unsafe _) = False

dampener_safe_with :: (Int -> Bool) -> [Int] -> Bool
dampener_safe_with diff_safe xs =
    let diffs = map (\d -> if diff_safe d then Safe d else Unsafe d) (differences xs) in
    let go [] = True
        go (Unsafe x : Unsafe y : ds) = diff_safe (x + y) && all is_safe ds
        go (Safe x : Unsafe y : Safe z : ds) =
            (diff_safe (x + y) || diff_safe (y + z)) && all is_safe ds
        go (Unsafe x : ds) = all is_safe ds
        go (Safe x : ds) = go ds in
    go diffs

dampener_safe :: [Int] -> Bool
dampener_safe xs =
    dampener_safe_with (`elem` [-3 .. -1]) xs || dampener_safe_with (`elem` [1..3]) xs

part2 :: [[Int]] -> Int
part2 = length . filter dampener_safe

main = do
    contents <- getContents
    let reports = parse contents
    putStr "Part 1: "
    print (part1 reports)
    putStr "Part 2: "
    print (part2 reports)
