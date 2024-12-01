import Data.List (sort)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

part1 :: [Word] -> [Word] -> Word
part1 left right = sum . map distance $ (zip `on` sort) left right
  where distance (x, y) | x < y     = y - x
                        | otherwise = x - y

part2 :: [Word] -> [Word] -> Word
part2 left right =
    let update_map map k = HashMap.insertWith (+) k 1 map in
    let right_counts = foldl update_map HashMap.empty right in
    let score k = k * HashMap.findWithDefault 0 k right_counts in
    sum (map score left)

-- Works on a valid input, errors with an unfriendly message on an invalid one.
parse :: String -> ([Word], [Word])
parse input =
    let input_tokens = words <$> lines input in
    let acc_line [x, y] (left, right) = (read x : left, read y : right)
        acc_line _ _                  = error "Bad input (wrong number of tokens on a line)" in
    foldr acc_line ([], []) input_tokens

main :: IO ()
main = do
  contents <- getContents
  let (left, right) = parse contents
  putStr "Part 1: "
  print (part1 left right)
  putStr "Part 2: "
  print (part2 left right)
