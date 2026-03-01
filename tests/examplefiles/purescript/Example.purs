module Main where

import Prelude

import Data.Array (head, tail, (..), length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.String as S
import Data.Foldable (foldl, foldr)
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Maybe (Maybe(..)) as M
import Prim (class Partial)

-- | A simple type alias
type Name = String

-- | A newtype wrapper
newtype UserId = UserId Int

-- | An ADT with multiple constructors
data Shape
  = Circle Number
  | Rectangle Number Number
  | Triangle Number Number Number

-- | A type class
class Area a where
  area :: a -> Number

-- | Instance for Shape
instance Area Shape where
  area (Circle r) = 3.14159 * r * r
  area (Rectangle w h) = w * h
  area (Triangle a b c) =
    let s = (a + b + c) / 2.0
    in sqrt (s * (s - a) * (s - b) * (s - c))

-- | A derived instance
derive instance Eq UserId

-- | Foreign import
foreign import sqrt :: Number -> Number

-- | Pattern matching with case
describe :: Shape -> String
describe shape = case shape of
  Circle r -> "Circle with radius " <> show r
  Rectangle w h -> "Rectangle " <> show w <> "x" <> show h
  Triangle _ _ _ -> "A triangle"

-- | Do notation
main :: Effect Unit
main = do
  let shapes = [Circle 5.0, Rectangle 3.0 4.0, Triangle 3.0 4.0 5.0]
  log "Shapes and their areas:"
  for_ shapes \shape -> do
    log (describe shape)
    logShow (area shape)

-- | Ado notation
readConfig :: Effect { host :: String, port :: Int }
readConfig = ado
  host <- pure "localhost"
  port <- pure 8080
  in { host, port }

-- | Typed holes
incomplete :: Int -> String
incomplete x = ?todo

-- | Infix declarations
infixl 6 add as +
infixr 5 append as <>

-- | Forall
identity :: forall a. a -> a
identity x = x

-- | Unicode operators
identity' :: ∀ a. a → a
identity' x = x

compose :: ∀ a b c. (b → c) → (a → b) → a → c
compose f g x = f (g x)

-- | Numeric literals
integers :: Array Int
integers = [0, 1, 42, 1000]

hexadecimals :: Array Int
hexadecimals = [0x00, 0xFF, 0xDEAD]

floats :: Array Number
floats = [0.0, 3.14, 1.0e10, 2.5e-3, 1.0e+5]

-- | String literals
greeting :: String
greeting = "Hello, world!\n"

-- | Triple-quoted (raw) strings
rawString :: String
rawString = """
  This is a raw string.
  It can contain "quotes" without escaping.
  And spans multiple lines.
"""

-- | Char literal
aChar :: Char
aChar = 'a'

escapedChar :: Char
escapedChar = '\n'

-- | Record syntax
type Config =
  { host :: String
  , port :: Int
  , debug :: Boolean
  }

defaultConfig :: Config
defaultConfig =
  { host: "localhost"
  , port: 8080
  , debug: false
  }

-- | Where clause
fibonacci :: Int -> Int
fibonacci n = go n 0 1
  where
    go 0 a _ = a
    go n' a b = go (n' - 1) b (a + b)

-- | Let expression
quadratic :: Number -> Number -> Number -> Number -> Number
quadratic a b c x =
  let
    term1 = a * x * x
    term2 = b * x
  in
    term1 + term2 + c

-- | Guards and operators
abs :: Int -> Int
abs n
  | n < 0 = -n
  | otherwise = n

-- | Multi-line comment
{- This is a
   multi-line comment
   {- with nesting -}
-}

-- | Constructor operators
data List a = Nil | Cons a (List a)

infixr 6 Cons as :

-- | Backtick operator usage
divides :: Int -> Int -> Boolean
divides a b = b `mod` a == 0

-- | If-then-else
clamp :: Int -> Int -> Int -> Int
clamp lo hi x =
  if x < lo then lo
  else if x > hi then hi
  else x

-- Boolean keywords
booleans :: Array Boolean
booleans = [true, false]

-- | Hex unicode escape in string
unicodeStr :: String
unicodeStr = "\x0041\x0042\x0043"
