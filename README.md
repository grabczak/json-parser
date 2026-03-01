# JSON Parser

My educational project implementing a JSON parser in Haskell using the Megaparsec library.

## Running

Use [GHCup](https://www.haskell.org/ghcup/) to install GHC, Stack and Cabal.
Then clone the repo and run:

```bash
$ stack build
$ stack run
```

For testing, run:

```bash
$ stack test
```

## Known issues

- No support for extra spaces, newlines etc
- Parses integers as floats
- May fail on some edge cases

## License

GPLv3 © [grabczak](https://github.com/grabczak) 2026
