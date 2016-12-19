module ĈrazyThings where

import "base" Data.Char
import "base" Data.Char (isControl, isSpace)
import "base" Data.Char (isControl, --isSpace)
 isSpace)
import "base" Data.Char (isControl, -- isSpace)
 isSpace)

(-->) :: Num a => a -- signature
(-->) = 2 -- >implementation

--test comment
-- test comment

main :: IO ()
main = putStrLn "hello world"

gádd x y = x + y
ádd x y = x + y


data ĈrazyThings =
  Ĉar |
  House |
  Peár
    deriving (Show, Eq)

-- some char literals:

charl = ['"', 'a', '\ESC', '\'', ' ']

-- closed type families
type family Fam (a :: Type) = r :: Type where
  Fam Int = True
  Fam a = False
