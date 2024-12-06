import Data.HashMap.Strict (HashMap, (!), (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import System.IO (stderr, hPutStrLn)
import Data.Graph (graphFromEdges, topSort)
import Data.List (sortOn)

import Parse

middle :: [a] -> a
middle [] = error "middle of empty list"
middle (x:xs) = go (x:xs) xs
  where go slow fast = case fast of
            [] -> head slow
            [_] -> error "middle of list with even length"
            (_:_:xs) -> go (tail slow) $ tail (tail fast)

data Rule = !Word `Before` !Word

parseWord :: Parse Word
parseWord = unambiguousReads

parseRule :: Parse Rule
parseRule = do
    l <- parseWord
    skip "|"
    r <- parseWord
    return $ l `Before` r

data Input = Input { rules :: [Rule], updates :: [[Word]] }

parseUpdate :: Parse [Word]
parseUpdate = parseSplitOnChar ',' parseWord

parse :: Parse Input
parse = Parse $ \s -> do
    (ruleLines, "":updateLines) <- return . span (not . null) $ lines s
    rules <- mapM (parseAll parseRule) ruleLines
    updates <- mapM (parseAll parseUpdate) updateLines
    return ("", Input { rules, updates })

split_updates_by_in_order :: Input -> ([[Word]], [[Word]])
split_updates_by_in_order input =
    let add_rule m (l `Before` r) = HashMap.alter (Just . (l:) . fromMaybe []) r m in
    let before = foldl add_rule HashMap.empty $ rules input in
    let add_before set page = foldl (flip HashSet.insert) set . fromMaybe [] $ before !? page in
    let forbidden_sets = scanl add_before HashSet.empty in
    let in_order update = not . or . zipWith (HashSet.member) update $ forbidden_sets update in
    let go good bad [] = (good, bad)
        go good bad (update:updates)
            | in_order update = go (update:good) bad updates
            | otherwise = go good (update:bad) updates in
    go [] [] $ updates input

part1 :: Input -> Word
part1 = sum . map middle . fst . split_updates_by_in_order

part2 :: Input -> Word
part2 input =
    let sort_update update =
          let add_rule map (l `Before` r) = HashMap.alter (Just . (r:) . fromMaybe []) l map in
          let relevant (l `Before` r) = l `elem` update && r `elem` update in
          let relevant_rules = filter relevant $ rules input in
          let after = foldl add_rule HashMap.empty relevant_rules in
          let adjlist = [((), page, adj) | (page, adj) <- HashMap.toList after] ++
                [((), page, []) | page <- update, not $ HashMap.member page after] in
          let (graph, nodeFromVertex, _) = graphFromEdges adjlist in
          let page_order = [page | (_, page, _) <- nodeFromVertex <$> topSort graph] in
          let index_map =
                foldl (\m (i, page) -> HashMap.insert page i m) HashMap.empty
                $ zip [0..] page_order in
          sortOn (index_map !) update in -- nth_element opportunity detected, iykyk
    let incorrectly_ordered_updates = snd $ split_updates_by_in_order input in
    sum $ map (middle . sort_update) incorrectly_ordered_updates

main = do
    contents <- getContents
    case parseAll parse contents of
        Just input -> do
            putStr "Part 1: "
            print (part1 input)
            putStr "Part 2: "
            print (part2 input)
        Nothing -> hPutStrLn stderr "Invalid input"
