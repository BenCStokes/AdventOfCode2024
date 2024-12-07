import Data.Array.Unboxed
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Ix (range)
import Data.Functor ((<&>))
import Prelude hiding (Left, Right)
import Data.List (genericLength)

type Point = (Word, Word)
type Map = UArray Point Bool

parse :: String -> (Map, Point)
parse input =
    let input_lines = lines input in
    let height = genericLength input_lines in
    let width = genericLength (head input_lines) in
    let array_contents = map (== '#') $ concat input_lines in
    let array_bounds = ((0, 0), (height - 1, width - 1)) in
    let start_pos = case [pos | (pos, '^') <- zip (range array_bounds) (concat input_lines)] of
          [pos] -> pos
          _ -> error "Expected only one start position (^)" in
    (listArray array_bounds array_contents, start_pos)

data Facing = Up | Right | Down | Left

step :: Facing -> Point -> Point
step Up (x, y) = (x - 1, y)
step Right (x, y) = (x, y + 1)
step Down (x, y) = (x + 1, y)
step Left (x, y) = (x, y - 1)

part1 :: (Map, Point) -> Word
part1 (layout, start_pos) =
    let start_state =
          (start_pos, cycle [Up, Right, Down, Left], HashSet.singleton start_pos) in
    let advance (pos, facing, visited) =
          let pos' = step (head facing) pos in
          layout !? pos' <&> \blocked -> if blocked then
             (pos, tail facing, visited)
          else
             (pos', facing, HashSet.insert pos' visited) in
    let end_state state = case advance state of
          Nothing -> state
          Just next -> end_state next in
    let (_, _, visited) = end_state start_state in
    fromIntegral $ HashSet.size visited

main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
