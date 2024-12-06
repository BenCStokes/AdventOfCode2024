module Parse where

import Data.List (stripPrefix)
import Data.Maybe (catMaybes, listToMaybe)

newtype Parse a = Parse (String -> Maybe (String, a))

runParse :: Parse a -> String -> Maybe (String, a)
runParse (Parse p) = p

instance Functor Parse where
    fmap f = (>>= return . f)

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

skip :: String -> Parse ()
skip str = Parse $ fmap (,()) . stripPrefix str

parseAny :: [Parse a] -> Parse a
parseAny ps = Parse $ \s -> listToMaybe . catMaybes $ map (flip runParse s) ps

parseSplitOnChar :: Char -> Parse a -> Parse [a]
parseSplitOnChar separator parseSection = Parse (go . (separator:))
  where go [] = Just ("", [])
        go s = flip runParse s $ do
            skip [separator]
            x <- parseSection
            xs <- Parse go
            return (x : xs)

parseAll :: Parse a -> String -> Maybe a
parseAll p s = do
    ("", x) <- runParse p s
    return x
