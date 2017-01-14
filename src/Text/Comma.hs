{- |
Module      : Text.Comma
Description : CSV Parser & Producer
Copyright   : (c) Daniel Lovasko, 2017
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Comma
( comma
, uncomma
) where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T


-- | Parse a field of a record.
field :: A.Parser T.Text -- ^ parser
field = fmap T.concat quoted <|> normal A.<?> "field"
  where
    normal  = A.takeWhile (A.notInClass "\n\r,\"")     A.<?> "normal field"
    quoted  = A.char '"' *> many between <* A.char '"' A.<?> "quoted field"
    between = A.takeWhile1 (/= '"') <|> (A.string "\"\"" *> pure "\"")

-- | Parse a block of text into a CSV table.
comma :: T.Text                   -- ^ CSV text
      -> Either String [[T.Text]] -- ^ error | table
comma text = A.parseOnly table (T.stripEnd text)
  where
    table  = A.sepBy1 record A.endOfLine A.<?> "table"
    record = A.sepBy1 field (A.char ',') A.<?> "record"

-- | Render a table of texts into a valid CSV output.
uncomma :: [[T.Text]] -- ^ table
        -> T.Text     -- ^ CSV text
uncomma table = T.unlines (map convRecord table)
  where
    convRecord r = T.concat $ intersperse (T.singleton ',') (map convField r)
    convField f 
      | elem '"' (T.unpack f) = T.concat ["\"", T.replace "\"" "\"\"" f, "\""]
      | otherwise             = f
