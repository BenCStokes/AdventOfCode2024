import Data.Maybe (fromMaybe)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Parse

data Equation = Equation { result :: Word, operands :: [Word] }

type Input = [Equation]

parseWord :: Parse Word
parseWord = unambiguousReads

parse :: String -> Input
parse input =
    let parseEquation = do
          result <- parseWord
          skip ": "
          operands <- parseSplitOnChar ' ' parseWord
          return $ Equation { result, operands } in
    fromMaybe (error "Invalid input") . mapM (parseAll parseEquation) $ lines input

-- Argument order: result -> rhs -> lhs
type InverseOp = Word -> Word -> Maybe Word

inv_add :: InverseOp
inv_add x y = Just (x - y)

inv_mul :: InverseOp
inv_mul x y =
    let (q, r) = x `quotRem` y in
    if r == 0 then Just q else Nothing

inv_cat :: InverseOp
inv_cat x 0 = Just x
inv_cat x y =
    let (xh, xt) = x `quotRem` 10 in
    let (yh, yt) = y `quotRem` 10 in
    if xt == yt then inv_cat xh yh else Nothing

work_backwards :: [InverseOp] -> Word -> Word -> [Word] -> HashSet Word
work_backwards ops least =
    let add_predecessors n set next =
          let try_op op = case op next n of
                Just lhs | lhs >= least -> Just lhs
                _ -> Nothing in
          foldl (\s op -> fromMaybe s $ flip HashSet.insert s <$> try_op op) set ops in
    let step n = HashSet.foldl' (add_predecessors n) HashSet.empty in
    foldr step . HashSet.singleton

type Op = Word -> Word -> Word

cat_words :: Op
cat_words x 0 = x
cat_words x y =
    let (yh, yt) = y `quotRem` 10 in
    10 * cat_words x yh + yt

can_solve :: [Op] -> [InverseOp] -> Equation -> Bool
can_solve ops inverse_ops equation =
    let go slow [] =
          let from_end = work_backwards inverse_ops
                         (head $ operands equation) (result equation) slow in
          any (flip HashSet.member from_end)
        go (x:xs) fast =
          let add_successors n set prev =
                foldl (\s op -> let out = op prev n in
                       if out <= result equation
                       then HashSet.insert out s
                       else s) set ops in
          go xs (drop 2 fast) . HashSet.foldl' (add_successors x) HashSet.empty
        go [] _ = error "This shouldn't be possible" in
    case operands equation of
        [] -> False
        x:xs -> go xs xs $ HashSet.singleton x

{-
brute_force_possibilities :: [Word -> Word -> Word] -> Word -> [Word] -> [Word]
brute_force_possibilities _ start [] = [start]
brute_force_possibilities ops start (x:xs) = do
    op <- ops
    brute_force_possibilities ops (op start x) xs

-- This brute force actually solves part 2 in a few seconds.
can_solve_brute_force :: [Word -> Word -> Word] -> Equation -> Bool
can_solve_brute_force ops (Equation { result, operands }) = elem result
    $ brute_force_possibilities ops (head operands) (tail operands)
-}

part1 :: Input -> Word
part1 = sum . map result . filter (can_solve [(+), (*)] [inv_add, inv_mul])

(.||.) :: Word -> Word -> Word
x .||. y = read $ show x ++ show y

part2 :: Input -> Word
part2 = sum . map result . filter (can_solve [(+), (*), cat_words] [inv_add, inv_mul, inv_cat])

main :: IO ()
main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    print (part2 input)
