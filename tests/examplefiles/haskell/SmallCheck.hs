---------------------------------------------------------------------
-- SmallCheck: another lightweight testing library.
-- Colin Runciman, August 2006
-- Version 0.2 (November 2006)
--
-- After QuickCheck, by Koen Claessen and John Hughes (2000-2004).
---------------------------------------------------------------------

module SmallCheck (
  smallCheck, depthCheck,
  Property, Testable,
  forAll, forAllElem,
  exists, existsDeeperBy, thereExists, thereExistsElem,
  (==>),
  Series, Serial(..),
  (\/), (><), two, three, four,
  cons0, cons1, cons2, cons3, cons4,
  alts0, alts1, alts2, alts3, alts4,
  N(..), Nat, Natural,
  depth, inc, dec
  ) where

import Data.List (intersperse)
import Control.Monad (when)
import System.IO (stdout, hFlush)

------------------ <Series of depth-bounded values> -----------------

-- Series arguments should be interpreted as a depth bound (>=0)
-- Series results should have finite length

type Series a = Int -> [a]

-- sum
infixr 7 \/
(\/) :: Series a -> Series a -> Series a
s1 \/ s2 = \d -> s1 d ++ s2 d

-- product
infixr 8 ><
(><) :: Series a -> Series b -> Series (a,b)
s1 >< s2 = \d -> [(x,y) | x <- s1 d, y <- s2 d]

------------------- <methods for type enumeration> ------------------

-- enumerated data values should be finite and fully defined
-- enumerated functional values should be total and strict

-- bounds:
-- for data values, the depth of nested constructor applications
-- for functional values, both the depth of nested case analysis
-- and the depth of results
 
class Serial a where
  series   :: Series a
  coseries :: Serial b => Series (a->b)

instance Serial () where
  series   _ = [()]
  coseries d = [ \() -> b
               | b <- series d ]

instance Serial Int where
  series   d = [(-d)..d]
  coseries d = [ \i -> if i > 0 then f (N (i - 1))
                       else if i < 0 then g (N (abs i - 1))
                       else z
               | z <- alts0 d, f <- alts1 d, g <- alts1 d ]

instance Serial Integer where
  series   d = [ toInteger (i :: Int)
               | i <- series d ]
  coseries d = [ f . (fromInteger :: Integer->Int)
               | f <- series d ]

newtype N a = N a

instance Show a => Show (N a) where
  show (N i) = show i

instance (Integral a, Serial a) => Serial (N a) where
  series   d = map N [0..d']
               where
               d' = fromInteger (toInteger d)
  coseries d = [ \(N i) -> if i > 0 then f (N (i - 1))
                           else z
               | z <- alts0 d, f <- alts1 d ]

type Nat = N Int
type Natural = N Integer

instance Serial Float where
  series d   = [ encodeFloat sig exp
               | (sig,exp) <- series d,
                 odd sig || sig==0 && exp==0 ]
  coseries d = [ f . decodeFloat
               | f <- series d ]
             
instance Serial Double where
  series   d = [ frac (x :: Float)
               | x <- series d ]
  coseries d = [ f . (frac :: Double->Float)
               | f <- series d ]

frac :: (Real a, Fractional a, Real b, Fractional b) => a -> b
frac = fromRational . toRational

instance Serial Char where
  series d   = take (d+1) ['a'..'z']
  coseries d = [ \c -> f (N (fromEnum c - fromEnum 'a'))
               | f <- series d ]

instance (Serial a, Serial b) =>
         Serial (a,b) where
  series   = series >< series
  coseries = map uncurry . coseries

instance (Serial a, Serial b, Serial c) =>
         Serial (a,b,c) where
  series   = \d -> [(a,b,c) | (a,(b,c)) <- series d]
  coseries = map uncurry3 . coseries

instance (Serial a, Serial b, Serial c, Serial d) =>
         Serial (a,b,c,d) where
  series   = \d -> [(a,b,c,d) | (a,(b,(c,d))) <- series d]
  coseries = map uncurry4 . coseries

uncurry3 :: (a->b->c->d) -> ((a,b,c)->d)
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a->b->c->d->e) -> ((a,b,c,d)->e)
uncurry4 f (w,x,y,z) = f w x y z

two   :: Series a -> Series (a,a)
two   s = s >< s

three :: Series a -> Series (a,a,a)
three s = \d -> [(x,y,z) | (x,(y,z)) <- (s >< s >< s) d]

four  :: Series a -> Series (a,a,a,a)
four  s = \d -> [(w,x,y,z) | (w,(x,(y,z))) <- (s >< s >< s >< s) d]

cons0 :: 
         a -> Series a
cons0 c _ = [c]

cons1 :: Serial a =>
         (a->b) -> Series b
cons1 c d = [c z | d > 0, z <- series (d-1)]

cons2 :: (Serial a, Serial b) =>
         (a->b->c) -> Series c
cons2 c d = [c y z | d > 0, (y,z) <- series (d-1)]

cons3 :: (Serial a, Serial b, Serial c) =>
         (a->b->c->d) -> Series d
cons3 c d = [c x y z | d > 0, (x,y,z) <- series (d-1)]

cons4 :: (Serial a, Serial b, Serial c, Serial d) =>
         (a->b->c->d->e) -> Series e
cons4 c d = [c w x y z | d > 0, (w,x,y,z) <- series (d-1)]

alts0 ::  Serial a =>
            Series a
alts0 d = series d

alts1 ::  (Serial a, Serial b) =>
            Series (a->b)
alts1 d = if d > 0 then series (dec d)
          else [\_ -> x | x <- series d]

alts2 ::  (Serial a, Serial b, Serial c) =>
            Series (a->b->c)
alts2 d = if d > 0 then series (dec d)
          else [\_ _ -> x | x <- series d]

alts3 ::  (Serial a, Serial b, Serial c, Serial d) =>
            Series (a->b->c->d)
alts3 d = if d > 0 then series (dec d)
          else [\_ _ _ -> x | x <- series d]

alts4 ::  (Serial a, Serial b, Serial c, Serial d, Serial e) =>
            Series (a->b->c->d->e)
alts4 d = if d > 0 then series (dec d)
          else [\_ _ _ _ -> x | x <- series d]

instance Serial Bool where
  series     = cons0 True \/ cons0 False
  coseries d = [ \x -> if x then b1 else b2
               | (b1,b2) <- series d ]

instance Serial a => Serial (Maybe a) where
  series     = cons0 Nothing \/ cons1 Just
  coseries d = [ \m -> case m of
                       Nothing -> z
                       Just x  -> f x
               |  z <- alts0 d ,
                  f <- alts1 d ]

instance (Serial a, Serial b) => Serial (Either a b) where
  series     = cons1 Left \/ cons1 Right
  coseries d = [ \e -> case e of
                       Left x  -> f x
                       Right y -> g y
               |  f <- alts1 d ,
                  g <- alts1 d ]

instance Serial a => Serial [a] where
  series     = cons0 [] \/ cons2 (:)
  coseries d = [ \xs -> case xs of
                        []      -> y
                        (x:xs') -> f x xs'
               |   y <- alts0 d ,
                   f <- alts2 d ]

-- Warning: the coseries instance here may generate duplicates.
instance (Serial a, Serial b) => Serial (a->b) where
  series = coseries
  coseries d = [ \f -> g [f x | x <- series d]
               | g <- series d ]              

-- For customising the depth measure.  Use with care!

depth :: Int -> Int -> Int
depth d d' | d >= 0    = d'+1-d
           | otherwise = error "SmallCheck.depth: argument < 0"

dec :: Int -> Int
dec d | d > 0     = d-1
      | otherwise = error "SmallCheck.dec: argument <= 0"

inc :: Int -> Int
inc d = d+1

-- show the extension of a function (in part, bounded both by
-- the number and depth of arguments)
instance (Serial a, Show a, Show b) => Show (a->b) where
  show f = 
    if maxarheight == 1
    && sumarwidth + length ars * length "->;" < widthLimit then
      "{"++(
      concat $ intersperse ";" $ [a++"->"++r | (a,r) <- ars]
      )++"}"
    else
      concat $ [a++"->\n"++indent r | (a,r) <- ars]
    where
    ars = take lengthLimit [ (show x, show (f x))
                           | x <- series depthLimit ]
    maxarheight = maximum  [ max (height a) (height r)
                           | (a,r) <- ars ]
    sumarwidth = sum       [ length a + length r 
                           | (a,r) <- ars]
    indent = unlines . map ("  "++) . lines
    height = length . lines
    (widthLimit,lengthLimit,depthLimit) = (80,20,3)::(Int,Int,Int)

---------------- <properties and their evaluation> ------------------

-- adapted from QuickCheck originals: here results come in lists,
-- properties have depth arguments, stamps (for classifying random
-- tests) are omitted, existentials are introduced

newtype PR = Prop [Result]

data Result = Result {ok :: Maybe Bool, arguments :: [String]}

nothing :: Result
nothing = Result {ok = Nothing, arguments = []}

result :: Result -> PR
result res = Prop [res]

newtype Property = Property (Int -> PR)

class Testable a where
  property :: a -> Int -> PR

instance Testable Bool where
  property b _ = Prop [Result (Just b) []]

instance Testable PR where
  property prop _ = prop

instance (Serial a, Show a, Testable b) => Testable (a->b) where
  property f = f' where Property f' = forAll series f

instance Testable Property where
  property (Property f) d = f d

evaluate :: Testable a => a -> Series Result
evaluate x d = rs where Prop rs = property x d

forAll :: (Show a, Testable b) => Series a -> (a->b) -> Property
forAll xs f = Property $ \d -> Prop $
  [ r{arguments = show x : arguments r}
  | x <- xs d, r <- evaluate (f x) d ]

forAllElem :: (Show a, Testable b) => [a] -> (a->b) -> Property
forAllElem xs = forAll (const xs)

thereExists :: Testable b => Series a -> (a->b) -> Property
thereExists xs f = Property $ \d -> Prop $
  [ Result
      ( Just $ or [ all pass (evaluate (f x) d)
                  | x <- xs d ] )
      [] ] 
  where
  pass (Result Nothing _)  = True
  pass (Result (Just b) _) = b

thereExistsElem :: Testable b => [a] -> (a->b) -> Property
thereExistsElem xs = thereExists (const xs)

exists :: (Serial a, Testable b) =>
            (a->b) -> Property
exists = thereExists series

existsDeeperBy :: (Serial a, Testable b) =>
                    (Int->Int) -> (a->b) -> Property
existsDeeperBy f = thereExists (series . f)
 
infixr 0 ==>

(==>) :: Testable a => Bool -> a -> Property
True ==>  x = Property (property x)
False ==> x = Property (const (result nothing))

--------------------- <top-level test drivers> ----------------------

-- similar in spirit to QuickCheck but with iterative deepening

-- test for values of depths 0..d stopping when a property
-- fails or when it has been checked for all these values
smallCheck :: Testable a => Int -> a -> IO String
smallCheck d = iterCheck 0 (Just d)

depthCheck :: Testable a => Int -> a -> IO String
depthCheck d = iterCheck d (Just d)

iterCheck :: Testable a => Int -> Maybe Int -> a -> IO String
iterCheck dFrom mdTo t = iter dFrom
  where
  iter :: Int -> IO String
  iter d = do
    let Prop results = property t d
    (ok,s) <- check (mdTo==Nothing) 0 0 True results
    maybe (iter (d+1))
          (\dTo -> if ok && d < dTo
                        then iter (d+1)
                        else return s)
          mdTo

check :: Bool -> Int -> Int -> Bool -> [Result] -> IO (Bool, String)
check i n x ok rs | null rs = do
  let s = "  Completed "++show n++" test(s)"
      y = if i then "." else " without failure."
      z | x > 0     = "  But "++show x++" did not meet ==> condition."
        | otherwise = ""
  return (ok, s ++ y ++ z)

check i n x ok (Result Nothing _ : rs) = do
  progressReport i n x
  check i (n+1) (x+1) ok rs

check i n x f (Result (Just True) _ : rs) = do
  progressReport i n x
  check i (n+1) x f rs

check i n x f (Result (Just False) args : rs) = do
  let s = "  Failed test no. "++show (n+1)++". Test values follow."
      s' = s ++ ": " ++ concat (intersperse ", " args)
  if i then
      check i (n+1) x False rs
    else
      return (False, s')

progressReport :: Bool -> Int -> Int -> IO ()
progressReport _ _ _ = return ()
