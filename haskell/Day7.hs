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

work_backwards :: Word -> Word -> [Word] -> HashSet Word
work_backwards least =
    let add_predecessors n set next =
          let with_add = next - n in
          let set' = if with_add >= least then HashSet.insert with_add set else set in
          let (with_mul, res) = next `quotRem` n in
          if res == 0 && with_mul >= least then HashSet.insert with_mul set' else set' in
    let step n = HashSet.foldl' (add_predecessors n) HashSet.empty in
    foldr step . HashSet.singleton

can_solve :: Equation -> Bool
can_solve equation =
    let go slow [] total =
          let from_end = work_backwards total (result equation) slow in
          any (flip HashSet.member from_end)
        go (x:xs) fast total =
          let add_successors n set prev =
                let with_add = prev + n in
                let set' = if with_add <= result equation
                      then HashSet.insert with_add set
                      else set in
                let with_mul = prev * n in
                if with_mul <= result equation then HashSet.insert with_mul set else set in
          go xs (drop 2 fast) (total + x)
            . HashSet.foldl' (add_successors x) HashSet.empty
        go [] _ _ = error "This shouldn't be possible" in
    case operands equation of
        [] -> False
        x:xs -> go xs xs x $ HashSet.singleton x

part1 :: Input -> Word
part1 = sum . map result . filter can_solve

main :: IO ()
main = do
    contents <- getContents
    let input = parse contents
    putStr "Part 1: "
    print (part1 input)
