import Data.Array.Unboxed
import Control.Monad (replicateM, guard)
import Data.Ix (inRange)

type Point = (Int, Int)
type Map = UArray Point Char

parse :: String -> Map
parse input =
    let input_lines = lines input in
    let height = length input_lines in
    let width = length (head input_lines) in
    let array_contents = concat input_lines in
    let array_bounds = ((0, 0), (height - 1, width - 1)) in
    listArray array_bounds array_contents

solve :: (Map -> Point -> Point -> [Point]) -> Map -> Int
solve antinode_candidates input =
    let antinodes = do
          [pos1, pos2] <- replicateM 2 $ indices input
          guard $ pos1 /= pos2
          let at1 = input ! pos1
          let at2 = input ! pos2
          guard $ at1 /= '.' && at1 == at2
          takeWhile (inRange $ bounds input) $ antinode_candidates input pos1 pos2 in
    let antinode_at :: UArray Point Bool
        antinode_at = accumArray (\_ _ -> True) False (bounds input) $ map (,()) antinodes in
    foldlArray' (\count present -> if present then count + 1 else count) 0 antinode_at

distance :: Point -> Point -> (Int, Int)
distance (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

step :: (Int, Int) -> Point -> Point
step (x, y) (x0, y0) = (x + x0, y + y0)

part1 :: Map -> Int
part1 = solve $ \input pos1 pos2 -> [step (distance pos1 pos2) pos2]

part2 :: Map -> Int
part2 = solve $ \input pos1 pos2 -> iterate (step $ distance pos1 pos2) pos1

main :: IO ()
main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    print (part2 input)
