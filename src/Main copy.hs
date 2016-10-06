module Main where

import Prelude hiding (words)
import Data.List hiding (words)
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.List.Extras.Argmax
import Text.Regex.Posix

import System.IO.Unsafe

words :: String -> [String]
words text = getAllTextMatches $ map toLower text =~ "[a-zA-Z]+" :: [String]

-- TODO optimize, this is super slow :(
-- via http://langref.org/fantom+erlang+haskell/maps/algorithms/histogram
w :: String -> Map.Map String Int
w text = Map.fromList [ (head l, length l) | l <- group (sort $ words text) ]

-- TODo rm text as arg
p :: Map.Map String Int -> String -> Double
p w word =  (/ n) $ fromIntegral $ fromMaybe 0 (Map.lookup word w :: Maybe Int)
  where
    n = fromIntegral $ Map.foldl' (+) 0 w

-- TODO rm w as arg
correction :: Map.Map String Int -> String -> String
correction w word = argmax (p w) $ Set.toList $ candidates w word

-- other way
correction' :: String -> IO String
correction' s = do
    text <- readFile "small.txt"
    let w' = w text
    return $ correction w' s

-- TODO rm w as arg
candidates :: Map.Map String Int -> String -> Set.Set String
candidates w word = Set.unions [known w [word], known w $ Set.toList $ edits1 word, known w $ Set.toList $ edits2 word, Set.singleton word]

-- TODO rm w as arg
known :: Map.Map String Int -> [String] -> Set.Set String
known w words = Set.fromList [ w' | w' <- words, Map.member w' w]

edits1 :: String -> Set.Set String
edits1 word = Set.fromList $ deletes ++ transposes ++ replaces ++ inserts
  where
    letters    = "abcdefghijklmnopqrstuvwxyz"
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ drop 1 r                   | (l,r) <- splits, not (null r) ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : drop 1 r               | (l,r) <- splits, not (null r), c <- letters ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- letters]

edits2 :: String -> Set.Set String
edits2 word = Set.fromList [ e2 | e1 <- Set.toList $ edits1 word, e2 <- Set.toList $ edits1 e1 ]

main :: IO ()
main = do
    text <- readFile "small.txt"
    putStrLn $ concatMap (\(f,s) -> f ++ show s) $ Map.toList $ w text
    print $ dropWhile (\x -> True) $ map edits2 $ words text
    return ()
