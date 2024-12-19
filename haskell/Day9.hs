import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s -> a
evalState m = fst . runState m

instance Functor (State s) where
    fmap f = (>>= return . f)

instance Applicative (State s) where
    pure x = State (x,)
    sf <*> sx = do
        f <- sf
        x <- sx
        return (f x)

instance Monad (State s) where
    return = pure
    m >>= f = State $ \s ->
        let (x, s') = runState m s in
        runState (f x) s'

data File = File { fid :: Int, blocks :: Int }

data Input = Input { files :: [File], spaces :: [Int] }

data Alternating a = Odd ([a], [a]) | Even ([a], [a])

alternate :: [a] -> Alternating a
alternate = foldr go $ Even ([], [])
  where go x (Even (l, r)) = Odd (x:r, l)
        go x (Odd (l, r)) = Even (x:r, l)

parseInput :: String -> Maybe Input
parseInput input = do
    Odd (fileLengths, spaces) <- Just . alternate $ map digitToInt input
    let files = zipWith ($) (map File [0..]) fileLengths
    return Input { files, spaces }

pop :: State [a] a
pop = State $ \xs -> (head xs, tail xs)

fragmentedDisk :: Input -> [Int]
fragmentedDisk Input { files, spaces } =
    let totalBlocks = sum (blocks <$> files) in
    let listBlocks File { fid, blocks } = replicate blocks fid in
    let genSegment (space, file) =
          replicate space pop ++ (return <$> listBlocks file) in
    let getDisk = sequence . ((return <$> listBlocks (head files)) ++)
                  . (>>= genSegment) . zip spaces $ tail files in
    let initialState = reverse $ files >>= listBlocks in
    take totalBlocks $ evalState getDisk initialState

part1 :: Input -> Int
part1 = sum . zipWith (*) [0..] . fragmentedDisk

main :: IO ()
main = do
    contents <- getLine
    let input = fromMaybe (error "Invalid input") $ parseInput contents
    putStr "Part 1: "
    print (part1 input)
