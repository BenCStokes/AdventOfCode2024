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

part1 :: Map -> Int
part1 input =
    let antinodes = do
          [pos1, pos2] <- replicateM 2 $ indices input
          guard $ pos1 /= pos2
          let at1 = input ! pos1
          let at2 = input ! pos2
          guard $ at1 /= '.' && at1 == at2
          let antinode = twice (step $ distance pos1 pos2) pos1
          guard $ inRange (bounds input) antinode
          return antinode in
    let antinode_at :: UArray Point Bool
        antinode_at = accumArray (\_ _ -> True) False (bounds input) $ map (,()) antinodes in
    foldlArray' (\count present -> if present then count + 1 else count) 0 antinode_at
  where distance (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
        step (x, y) (x0, y0) = (x + x0, y + y0)
        twice f = f . f

main :: IO ()
main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    putStrLn "TODO"
