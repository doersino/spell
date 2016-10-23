module Spell where

import Prelude hiding (words)
import Data.List hiding (words)
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.List.Split
import Data.List.Extras.Argmax

import System.IO.Unsafe
import Paths_spell

-- | Note: To simulate the global variable @WORDS@ from the original Python
-- implementation, we'll perform some unsafe IO here. Please put on your safety
-- goggles and take a few steps back.
{-# NOINLINE words #-}
words :: Map.Map String Int
words = Map.fromList [ (head l, length l) | l <- group (sort words') ]
  where
    text   = unsafePerformIO $ getDataFileName "big.txt" >>= readFile
    words' = filter (not . null) . splitWhen (not . isAlpha) $ map toLower text

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
candidates word = head $ filter (not . Set.null) ors
  where
    ors = [ known [word]
           , known $ Set.toList $ edits1 word
           , known $ Set.toList $ edits2 word
           , Set.singleton word
           ]

-- | The subset of @words'@ that appear in the dictionary of @words@.
known :: [String] -> Set.Set String
known words' = Set.fromList [ w | w <- words', Map.member w words]

-- | All edits that are one edit away from @word@.
edits1 :: String -> Set.Set String
edits1 word = Set.fromList $ deletes ++ transposes ++ replaces ++ inserts
  where
    letters    = "abcdefghijklmnopqrstuvwxyz"
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ tail r                     | (l,r) <- splits, not (null r) ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : tail r                 | (l,r) <- splits, not (null r), c <- letters ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- letters]

-- | All edits that are two edits away from @word@.
edits2 :: String -> Set.Set String
edits2 word = Set.fromList [ e2 | e1 <- Set.toList $ edits1 word, e2 <- Set.toList $ edits1 e1 ]
