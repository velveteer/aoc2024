{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Arrow
import Data.Ix
import Data.Monoid (All (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Read qualified as Text

main :: IO ()
main = do
  Text.putStrLn "day 2"
  input <- Text.readFile "text/day2.txt"
  print $ day2a input
  print $ day2b input

-- | Solve Day 2 Part One
-- >>> day2a <$> Text.readFile "text/day2.example.txt"
-- 2
day2a :: Text -> Int
day2a = solve pure

-- | Solve Day 2 Part Two
-- >>> day2b <$> Text.readFile "text/day2.example.txt"
-- 4
day2b :: Text -> Int
day2b = solve subs

subs :: [a] -> [[a]]
subs xs = flip fmap [0 .. length xs - 1] $ \n -> do
  let (xs', _ : ys) = splitAt n xs
  xs' <> ys

toTuple :: [Int] -> (Set.Set Ordering, All)
toTuple xs = foldMap go (zip xs (drop 1 xs))
 where
  go (x, y) =
    ( Set.singleton (compare x y)
    , All (inRange (1, 3) (abs (x - y)))
    )

isSafe :: Set.Set Ordering -> All -> Bool
isSafe s m = Set.size s == 1 && getAll m

solve :: ([Int] -> [[Int]]) -> Text -> Int
solve f =
  Text.lines
    >>> fmap Text.words
    >>> (fmap . fmap) (Text.read . Text.unpack)
    >>> fmap f
    >>> (fmap . fmap) toTuple
    >>> filter (any (uncurry isSafe))
    >>> length
