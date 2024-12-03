{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Attoparsec.Text qualified as AT
import Data.Either (rights)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = do
  input <- Text.readFile "text/day3.txt"
  print $ day3a input
  print $ day3b input

-- | Solve Day 3 Part One
-- >>> day3a <$> Text.readFile "text/day3.example.txt"
-- 161
day3a :: Text -> Int
day3a =
  Text.splitOn "mul"
    >>> fmap (AT.parseOnly parseTuple)
    >>> rights
    >>> foldMap (Sum . uncurry (*))
    >>> getSum

-- | Solve Day 3 Part Two
-- >>> day3b <$> Text.readFile "text/day3.example.txt"
-- 48
day3b :: Text -> Int
day3b =
  Text.splitOn "don't()"
    >>> fmap (Text.breakOn "do()")
    >>> (fmap fst . take 1 &&& fmap snd)
    >>> uncurry (<>)
    >>> Text.concat
    >>> day3a

parseTuple :: AT.Parser (Int, Int)
parseTuple =
  AT.char '('
    *> ((,) <$> AT.decimal <* AT.char ',' <*> AT.decimal)
    <* AT.char ')'
