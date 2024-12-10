{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Arrow
import Data.Foldable as Fold
import Data.Functor
import Data.Maybe
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  input <- Text.readFile "text/day7.txt"
  print $ day7a input
  print $ day7b input

-- | Solve Day 7 Part One
-- >>> day7a <$> Text.readFile "text/day7.example.txt"
-- 3749
day7a :: Text -> Int
day7a = solve [(+), (*)]

-- | Solve Day 7 Part Two
-- >>> day7b <$> Text.readFile "text/day7.example.txt"
-- 11387
day7b :: Text -> Int
day7b = solve [cat, (+), (*)]

solve :: [Int -> Int -> Int] -> Text -> Int
solve ops =
  ( Text.lines
      <&> foldMap
        ( Text.splitOn ":"
            >>> ( \[ Text.unpack >>> read -> a
                    , Text.words >>> fmap (Text.unpack >>> read) -> bs
                    ] -> (a, bs)
                )
            >>> uncurry (sumValid ops)
        )
  )
    >>> fold
    >>> getSum

sumValid :: [Int -> Int -> Int] -> Int -> [Int] -> Maybe (Sum Int)
sumValid _ x [y] | x == y = Just (Sum x)
sumValid ops x (y : z : w)
  | any (\op -> isJust $ sumValid ops x (op y z : w)) ops =
      Just (Sum x)
sumValid _ _ _ = Nothing

cat :: Int -> Int -> Int
cat (show -> x) (show -> y) = read $ x <> y
