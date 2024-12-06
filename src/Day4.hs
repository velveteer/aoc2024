{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad (guard)
import Data.Foldable qualified as Fold
import Data.Ix (inRange, range)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  input <- Text.readFile "text/day4.txt"
  print $ day4a input
  print $ day4b input

-- | Solve Day 4 Part One
-- >>> day4a <$> Text.readFile "text/day4.example.txt"
-- 18
day4a :: Text -> Int
day4a =
  Text.lines
    >>> buildGrid
    >>> match [-1 .. 1] "XMAS"
    >>> length

-- | Solve Day 4 Part Two
-- >>> day4b <$> Text.readFile "text/day4.example.txt"
-- 9
day4b :: Text -> Int
day4b =
  Text.lines
    >>> buildGrid
    >>> match [-1, 1] "MAS" -- Find all the diagonal MAS
    >>> fmap (second (!! 1)) -- Get all the A positions (center of the X)
    >>> Map.fromListWith @_ @Int (+) . fmap (,1) -- Aggregate A's by position
    >>> Map.filter (2 ==) -- Only keep where two A's overlap (making an X)
    >>> length

type Pos = (Int, Int)
type Grid = Map Pos Char

buildGrid :: [Text] -> Grid
buildGrid =
  snd
    . Fold.foldl'
      (\(!n, !m) t -> (succ n, snd $ Text.foldl' go ((n, 0), m) t))
      (0, mempty)
 where
  go ((x, !y), !acc) c =
    ((x, succ y), Map.insert (x, y) c acc)

match :: [Int] -> String -> Grid -> [(String, [Pos])]
match r str g = concatMap (`go` str) paths
 where
  bs = (fst $ Map.findMin g, fst $ Map.findMax g)
  paths = concatMap dirs (range bs)
  next (x, y) (i, j) = (i + x, j + y)
  dirs x =
    [ takeWhile (inRange bs) $ iterate (next (i, j)) x
    | i <- r
    , j <- r
    , i /= 0 || j /= 0
    ]
  go _ [] = [([], [])]
  go xs (t : ts) = do
    (x : xs') <- pure xs
    let c = g Map.! x
    guard $ c == t
    (s, is) <- go xs' ts
    pure (c : s, x : is)
