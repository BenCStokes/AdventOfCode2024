import Data.Array.Unboxed
import Data.Ix (inRange, range)
import Data.List (genericLength, sort)

import Parse (allOccurrences, skip)

type Wordsearch = UArray (Word, Word) Char

ix_bounds :: Word -> Word -> ((Word, Word), (Word, Word))
ix_bounds width height = ((0, 0), (height - 1, width - 1))

parse :: String -> (Wordsearch, Word, Word)
parse input =
    let rows = lines input in
    let width = genericLength (head rows) in
    let height = genericLength rows in
    (listArray (ix_bounds width height) (concat rows), width, height)

rows :: Word -> Word -> [[(Word, Word)]]
rows width height = [map (x,) [0 .. width - 1] | x <- [0 .. height - 1]]

cols :: Word -> Word -> [[(Word, Word)]]
cols width height = [map (,y) [0 .. height - 1] | y <- [0 .. width - 1]]

diags_from :: ((Word, Word) -> (Word, Word)) -> [(Word, Word)] -> [[(Word, Word)]]
diags_from step = zipWith take [1..] . map (iterate step)

up_diags :: Word -> Word -> [[(Word, Word)]]
up_diags width height =
    diags_from step [(x, 0) | x <- [0 .. height - 1]]
    ++ diags_from step [(height - 1, y) | y <- [width - 1, width - 2 .. 1]]
  where step (x, y) = (x - 1, y + 1)

down_diags :: Word -> Word -> [[(Word, Word)]]
down_diags width height =
    diags_from step [(x, 0) | x <- [height - 1, height - 2 .. 0]]
    ++ diags_from step [(0, y) | y <- [width - 1, width - 2 .. 1]]
  where step (x, y) = (x + 1, y + 1)

part1 :: (Wordsearch, Word, Word) -> Word
part1 (wordsearch, width, height) =
    let search_lines gen_lines =
         let as_strs = map (map (wordsearch !)) $ gen_lines width height in
         let count_xmas str = genericLength $
              allOccurrences (skip "XMAS") str ++ allOccurrences (skip "SAMX") str in
         sum $ map count_xmas as_strs in
    sum $ map search_lines [rows, cols, up_diags, down_diags]

-- Input: upper left, upper right, lower left, lower right
test_corners :: Char -> Char -> Char -> Char -> Bool
test_corners ul ur ll lr = ends ul lr && ends ur ll
  where ends 'M' 'S' = True
        ends 'S' 'M' = True
        ends  _   _  = False

-- Input: centre of the (3x3) square
test_square :: Wordsearch -> (Word, Word) -> Bool
test_square wordsearch (x, y) =
    at x y == 'A' && test_corners
        (at (x - 1) (y - 1)) (at (x - 1) (y + 1)) (at (x + 1) (y - 1)) (at (x + 1) (y + 1))
  where at x y = wordsearch ! (x, y)

part2 :: (Wordsearch, Word, Word) -> Word
part2 (wordsearch, width, height) =
    genericLength . filter (test_square wordsearch) $ range ((1, 1), (height - 2, width - 2))

main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    print (part2 input)
