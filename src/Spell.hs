-- Haskell implementation of http://norvig.com/spell-correct.html
-- Note: Requires semi-recent version of the following packages:
--       base (duh!), containers, regex-posix, list-extras, data-ordlist
-- Note: This runs quite slow, so generate a file "small.txt" by running "cat
--       big.txt | head -n 1000 > small.txt" in your shell of choice.
-- Usage: ghci SpellCorrect.hs < correction "speling"

module Spell where

import Prelude hiding (words)
import Data.List hiding (words)
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.List.Extras.Argmax
import Text.Regex.Posix
import qualified Data.List.Ordered as Ordered

import System.IO.Unsafe

-- | Note: To simulate the global variable @WORDS@ from the original Python
-- implementation, we'll perform some unsafe IO here. Please put on your safety
-- goggles and take a few steps back.
{-# NOINLINE words #-}
words :: Map.Map String Int
words = Map.fromList [ (l, length l) | l <- Ordered.nub (sort words') ]
  where
    text  = unsafePerformIO $ readFile "small.txt"
    words'= getAllTextMatches $ map toLower text =~ "[a-zA-Z]+" :: [String]

-- | Probability of @word@.
p :: String -> Double
p word =  (/ n) $ fromIntegral $ fromMaybe 0 (Map.lookup word words :: Maybe Int)
  where
    n = fromIntegral $ Map.foldl' (+) 0 words

-- | Most probable spelling correction for @word@.
correction :: String -> String
correction word = argmax p $ Set.toList $ candidates word

-- | Generate possible spelling corrections for @word@.
candidates :: String -> Set.Set String
candidates word = Set.unions [known [word], known $ Set.toList $ edits1 word, known $ Set.toList $ edits2 word, Set.singleton word]

-- | The subset of @words'@ that appear in the dictionary of @words@.
known :: [String] -> Set.Set String
known words' = Set.fromList [ w | w <- words', Map.member w words]

-- All edits that are one edit away from @word@.
edits1 :: String -> Set.Set String
edits1 word = Set.fromList $ deletes ++ transposes ++ replaces ++ inserts
  where
    letters    = "abcdefghijklmnopqrstuvwxyz"
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ drop 1 r                   | (l,r) <- splits, not (null r) ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : drop 1 r               | (l,r) <- splits, not (null r), c <- letters ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- letters]

-- All edits that are two edits away from @word@.
edits2 :: String -> Set.Set String
edits2 word = Set.fromList [ e2 | e1 <- Set.toList $ edits1 word, e2 <- Set.toList $ edits1 e1 ]
