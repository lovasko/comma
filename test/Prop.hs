import Control.Monad
import Text.Comma
import Test.QuickCheck
import qualified Data.Text as T


newtype CSV = CSV [[T.Text]] deriving (Show)

-- | Generate the content of a field.
genField :: Gen T.Text
genField = fmap T.pack (replicateM 5 $ elements alphabet)
  where alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ "\"\n" 

-- | Random-generated instances of CSV.
instance Arbitrary CSV where
  arbitrary = do
    nRows <- elements [1..10]
    nCols <- elements [1..3]
    cells <- replicateM (nRows * nCols) genField
    return $ CSV (init $ chunksOf nCols cells)

-- | Split a list into chunks of a specified length.
chunksOf :: Int   -- ^ chunk length
         -> [a]   -- ^ list
         -> [[a]] -- ^ list of chunks
chunksOf _ [] = [[]]
chunksOf n xs = take n xs : chunksOf n (drop n xs)
  
-- | The 'comma' and 'uncomma' function have to form identity when composed.
test :: CSV  -- ^ CSV table
     -> Bool -- ^ result
test (CSV csv) = case comma (uncomma csv) of
  Left _     -> False
  Right csv' -> csv == csv'

-- | Run the identity property test.
main :: IO ()
main = quickCheckWith stdArgs {maxSuccess=10000} (property test)
