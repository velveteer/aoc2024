{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Arrow
import Data.Foldable as Fold
import Data.Function
import Data.Ix (inRange)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  input <- Text.readFile "text/day6.txt"
  print $ day6a input
  print $ day6b input

-- | Solve Day 6 Part One
-- >>> day6a <$> Text.readFile "text/day6.example.txt"
-- 41
day6a :: Text -> Int
day6a =
  Text.lines
    >>> buildGrid
    >>> uncurry getPath
    >>> fmap fst
    >>> Set.fromList
    >>> length

-- | Solve Day 6 Part Two
-- >>> day6b <$> Text.readFile "text/day6.example.txt"
-- 6
day6b :: Text -> Int
day6b =
  Text.lines
    >>> buildGrid
    >>> id &&& uncurry getPath
    >>> (\(g, ps) -> (g, concatMap (futureWalls (fst g)) ps))
    >>> second Set.fromList
    >>> (\((g, s), pos) -> fmap (\p -> getPath (Map.insert p '#' g) s) (Set.toList pos))
    >>> fmap hasDuplicate
    >>> filter id
    >>> length

futureWalls :: Grid -> Guard -> [Pos]
futureWalls g ((x, y), (i, j))
  = iterate (\(!x', !y') -> (x' + i, y' + j)) (x, y)
  & takeWhile (\p -> g Map.!? p == Just '.')

hasDuplicate :: [Guard] ->  Bool
hasDuplicate = go Set.empty
  where
    go _ [] = False
    go seen (x:xs)
      | x `Set.member` seen = True
      | otherwise = go (Set.insert x seen) xs

type Pos = (Int, Int)
type Dir = (Int, Int)
type Grid = Map Pos Char
type Guard = (Pos, Dir)

getPath :: Grid -> Guard -> [Guard]
getPath g =
  iterate (next g)
    >>> takeWhile (fst >>> inRange (fst $ Map.findMin g, fst $ Map.findMax g))

buildGrid :: [Text] -> (Grid, Guard)
buildGrid =
  snd
    . Fold.foldl'
      (\(!n, (!m, s)) t -> (succ n, snd $ Text.foldl' go ((n, 0), (m, s)) t))
      (0, (mempty, ((0, 0), (0, 0))))
 where
  go ((x, !y), (!acc, start)) c =
    let s = if c `Text.elem` "<^>v" then ((x, y), dir c) else start
     in ((x, succ y), (Map.insert (x, y) c acc, s))

dir :: Char -> Dir
dir '<' = (0, -1)
dir '^' = (-1, 0)
dir '>' = (0, 1)
dir 'v' = (1, 0)

next :: Grid -> Guard -> Guard
next g ((!x, !y), (!i, !j)) =
  case g Map.!? (x + i, y + j) of
    Just '#' -> ((x, y), (j, negate i))
    _ -> ((x + i, y + j), (i, j))
