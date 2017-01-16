# Text.Comma
The `comma` package implements parsing and producing of the CSV format.

## Build & install
There are two standard ways of obtaining the module:
 * by cloning the git repository: `git clone https://github.com/lovasko/comma`
 * by using the central Hackage server: `cabal install comma`

### Dependencies
Main part, the library itself, depends on two packages:
 * `attoparsec`
 * `text`

In addition, the testing component of the project depends on the `QuickCheck`
package.

## API
The `Text.Comma` module exports only two functions: `comma` and `uncomma`,
parsing and producing CSV respectively. The function prototypes are as follows:

```haskell
-- | Parse a block of text into a CSV table.
comma :: T.Text                   -- ^ CSV text
      -> Either String [[T.Text]] -- ^ error | table
```

```haskell
-- | Render a table of texts into a valid CSV output.
uncomma :: [[T.Text]] -- ^ table
        -> T.Text     -- ^ CSV text
```

### Example
The following example loads any CSV file and prepends a column that contains
row numbers:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Comma
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Prepend a number in front of each row.
number :: [[T.Text]] -> [[T.Text]]
number = zipWith (:) (map (T.pack . show) [0..])

main :: IO ()
main = T.readFile "table.csv" >>= (\file -> case comma file of
  Left err  -> T.putStrLn ("ERROR: " <> T.pack err)           >> exitFailure
  Right csv -> T.writeFile "table.csv" (uncomma $ number csv) >> exitSuccess)
```

Example usage:
```bash
$ cat table.csv
name,surname,language
Dennis,Ritchie,C
Larry,Wall,Perl
John,McCarthy,Lisp
$ ./number && cat table.csv
0,name,surname,language
1,Dennis,Ritchie,C
2,Larry,Wall,Perl
3,John,McCarthy,Lisp
```

## Standards
Both producing and parsing of CSV files strictly follows the
[RFC4180](https://tools.ietf.org/html/rfc4180) document.

## License
The `comma` package is licensed under the terms of the [2-clause BSD
license](LICENSE). In case you need any other license, feel free to contact the
author.

## Author
Daniel Lovasko <daniel.lovasko@gmail.com>
