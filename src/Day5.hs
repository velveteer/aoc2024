{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Arrow
import Data.Function
import Data.Graph qualified as G
import Data.List qualified as List
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  input <- Text.readFile "text/day5.txt"
  print $ day5a input
  print $ day5b input

-- | Solve Day 5 Part One
-- >>> day5a <$> Text.readFile "text/day5.example.txt"
-- 143
day5a :: Text -> Int
day5a = solve id

-- | Solve Day 5 Part Two
-- >>> day5b <$> Text.readFile "text/day5.example.txt"
-- 123
day5b :: Text -> Int
day5b = solve not

solve :: (Bool -> Bool) -> Text -> Int
solve f =
  Text.splitOn "\n\n"
    >>> fmap Text.lines
    >>> take 1 &&& drop 1
    >>> first (concat >>> fmap processRule)
    >>> second
      ( concat
          >>> fmap (Text.splitOn ",")
          >>> (fmap . fmap) (Text.unpack >>> read @Int)
      )
    >>> ( \(rules, upds) ->
            fmap
              ( \upd ->
                  let sorted = sortUpdate (buildOrdering upd rules) upd
                   in (upd == sorted, sorted)
              )
              upds
        )
    >>> filter (f . fst)
    >>> fmap snd
    >>> foldMap (\xs -> Sum $ xs !! (length xs `div` 2))
    >>> getSum

processRule :: Text -> (Int, Int)
processRule (Text.splitOn "|" -> [read @Int . Text.unpack -> x, read @Int . Text.unpack -> y]) = (x, y)

sortUpdate :: [(Int, Int)] -> [Int] -> [Int]
sortUpdate tbl = List.sortBy (comparing (`lookup` tbl))

buildOrdering :: [Int] -> [(Int, Int)] -> [(Int, Int)]
buildOrdering upd (filter (\(x, y) -> x `elem` upd && y `elem` upd) -> es) =
  G.buildG (minimum upd, maximum upd) es
    & G.topSort
    & flip zip [0 ..]
