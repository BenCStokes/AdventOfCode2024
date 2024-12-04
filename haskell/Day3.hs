import Data.Foldable (foldl')

import Parse

parseInt :: Parse Int
parseInt = unambiguousReads

parseMul :: Parse (Int, Int)
parseMul = do
    skip "mul("
    lhs <- parseInt
    skip ","
    rhs <- parseInt
    skip ")"
    return (lhs, rhs)

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . allOccurrences parseMul

data Instruction = Do | DoNot | Mul Int Int

parseInstruction :: Parse Instruction
parseInstruction = parseAny
  [
    Do <$ skip "do()",
    DoNot <$ skip "don't()",
    uncurry Mul <$> parseMul
  ]

data State = State { total :: !Int, mul_enabled :: !Bool }

runInstruction :: State -> Instruction -> State
runInstruction state instruction = case instruction of
    Do -> state { mul_enabled = True }
    DoNot -> state { mul_enabled = False }
    Mul x y | mul_enabled state -> state { total = total state + x * y }
    Mul _ _ -> state

initialState :: State
initialState = State { total = 0, mul_enabled = True }

part2 :: String -> Int
part2 = total . foldl' runInstruction initialState . allOccurrences parseInstruction

main :: IO ()
main = do
    input <- getContents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    print (part2 input)
