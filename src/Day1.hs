{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Arrow
import Data.Bifunctor (bimap)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Read qualified as Text

main :: IO ()
main = do
  Text.putStrLn "day 1"
  input <- Text.readFile "text/day1.txt"
  print $ day1a input
  print $ day1b input

-- | Solve Day 1 Part One
-- >>> day1a <$> Text.readFile "text/day1.example.txt"
-- 11
day1a :: Text -> Int
day1a =
  lists
    >>> bimap List.sort List.sort
    >>> uncurry (zipWith (\a b -> abs (a - b)))
    >>> sum

-- | Solve Day 1 Part Two
-- >>> day1b <$> Text.readFile "text/day1.example.txt"
-- 31
day1b :: Text -> Int
day1b =
  lists
    >>> second (Map.fromListWith (+) . fmap (,1))
    >>> (\(xs, hy) -> foldMap (\x -> Sum . (* x) <$> Map.lookup x hy) xs)
    >>> maybe 0 getSum

lists :: Text -> ([Int], [Int])
lists =
  Text.lines
    >>> List.foldl'
      (\(xs, ys) (Text.words -> [Text.read . Text.unpack -> x, Text.read . Text.unpack -> y]) -> (x : xs, y : ys))
      ([], [])
