import Data.List (stripPrefix)

newtype Parse a = Parse (String -> Maybe (String, a))

runParse :: Parse a -> String -> Maybe (String, a)
runParse (Parse p) = p

instance Functor Parse where
    fmap f px = do
        x <- px
        return (f x)

instance Applicative Parse where
    pure x = Parse (\s -> Just (s, x))
    pf <*> px = do
        f <- pf
        x <- px
        return (f x)

instance Monad Parse where
    return = pure
    Parse p >>= f = Parse (\s -> p s >>= \(s', x) -> runParse (f x) s')

instance MonadFail Parse where
    fail _ = Parse (const Nothing)

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:xs) = Just xs

dropUntilFirst :: Parse a -> Parse a
dropUntilFirst parse = Parse go
  where go s = case runParse parse s of
                   Nothing -> maybeTail s >>= go
                   found -> found

allOccurrences :: Parse a -> String -> [a]
allOccurrences parse = go
  where go s = case runParse (dropUntilFirst parse) s of
                   Nothing -> []
                   Just (s', x) -> x : go s'

unambiguousReads :: Read a => Parse a
unambiguousReads = Parse $ \s -> case reads s of
    [(x, s')] -> Just (s', x)
    _ -> Nothing

parseInt :: Parse Int
parseInt = unambiguousReads

skip :: String -> Parse ()
skip str = Parse $ fmap (,()) . stripPrefix str

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

parseAny :: [Parse a] -> Parse a
parseAny = Parse . go
  where go [] s = Nothing
        go (p:ps) s = case runParse p s of
            Nothing -> go ps s
            parsed -> parsed

data Instruction = Do | DoNot | Mul Int Int

parseInstruction :: Parse Instruction
parseInstruction = parseAny
  [
    Do <$ skip "do()",
    DoNot <$ skip "don't()",
    uncurry Mul <$> parseMul
  ]

data State = State { total :: Int, mul_enabled :: Bool }

runInstruction :: State -> Instruction -> State
runInstruction state instruction = case instruction of
    Do -> state { mul_enabled = True }
    DoNot -> state { mul_enabled = False }
    Mul x y | mul_enabled state -> state { total = total state + x * y }
    Mul _ _ -> state

initialState :: State
initialState = State { total = 0, mul_enabled = True }

part2 :: String -> Int
part2 = total . foldl runInstruction initialState . allOccurrences parseInstruction

main :: IO ()
main = do
    input <- getContents
    putStr "Part 1: "
    print (part1 input)
    putStr "Part 2: "
    print (part2 input)
