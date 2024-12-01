# Advent of Code 2024

Solutions to [https://adventofcode.com](https://adventofcode.com/2024).

## Development

1. [Get ghcup](https://www.haskell.org/ghcup/)
2. Install `ghc`
```sh
ghcup install ghc --set 9.10
```
3. Install `cabal-install`
```sh
ghcup install cabal
```
4. (Optional) Install ghciwatch
```sh
cargo install ghciwatch
```

### Fire up GHCi

```sh
cabal repl
```

### Or fire up ghciwatch

```sh
ghciwatch --test-ghci 'Test.DocTest.doctest ["src"]'
```
This will reload the interpreter and run doctests on every file save.
