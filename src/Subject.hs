module Subject (Subject (..)) where
import Data.Char
import Graphics.Gloss
data Subject = Subject { name ::  [Char] , picture :: Picture} deriving (Eq, Show)