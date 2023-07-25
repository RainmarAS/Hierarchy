module Subject (Subject (..)) where
import Data.Char
data Subject = Subject { name ::  [Char]} deriving (Eq, Show)