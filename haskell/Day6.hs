import Data.Array.Unboxed
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Ix (range, index)
import Prelude hiding (Left, Right)
import Data.List (genericLength)
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Containers.ListUtils (nubIntOn)

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
    deriving (Eq, Enum)

instance Hashable Facing where
    hashWithSalt = hashUsing fromEnum

step :: Facing -> Point -> Point
step Up (x, y) = (x - 1, y)
step Right (x, y) = (x, y + 1)
step Down (x, y) = (x + 1, y)
step Left (x, y) = (x, y - 1)

data Path = Loop [Point] | Exit [Point]
    deriving (Show)

path :: Map -> Point -> Path
path layout start_pos = go (start_pos, cycle [Up, Right, Down, Left], HashSet.empty, [start_pos])
  where go (pos, facing, visited, trace)
          | HashSet.member (pos, head facing) visited = Loop trace
          | otherwise =
              let pos' = step (head facing) pos in
              case layout !? pos' of
                  Nothing -> Exit trace
                  Just True -> go (pos, tail facing, visited, trace)
                  Just False ->
                      go (pos', facing, HashSet.insert (pos, head facing) visited, pos':trace)

part1 :: (Map, Point) -> Word
part1 (layout, start_pos) =
    case path layout start_pos of
        Exit trace -> genericLength $ nubIntOn (index $ bounds layout) trace
        _ -> error "Guard does not leave map"

part2 :: (Map, Point) -> Word
part2 (layout, start_pos) = genericLength $ do
    (obstruction_pos, False) <- assocs layout
    -- The below line is slow, because (//) is O(size of array). This can be solved e.g. by using
    -- a mutable vector from the vector package, but it still runs in reasonable time as-is,
    -- so I'm not going to put effort into optimising further.
    -- In fact, possible locations for an obstruction which don't actually obstruct the original
    -- path should also be pruned immediately, so plenty of room for speeding this up.
    let with_obstruction = layout // [(obstruction_pos, True)]
    Loop _ <- return $ path with_obstruction start_pos
    return ()

main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    print (part2 input)
