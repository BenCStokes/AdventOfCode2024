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

can_solve :: [InverseOp] -> Equation -> Bool
can_solve ops equation =
    case operands equation of
        [] -> False
        x:xs -> HashSet.member x $ work_backwards ops x (result equation) xs

part1 :: Input -> Word
part1 = sum . map result . filter (can_solve [inv_add, inv_mul])

part2 :: Input -> Word
part2 = sum . map result . filter (can_solve [inv_add, inv_mul, inv_cat])

main :: IO ()
main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    print (part2 input)
