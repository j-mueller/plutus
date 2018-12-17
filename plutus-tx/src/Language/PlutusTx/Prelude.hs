{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE TemplateHaskell     #-}
module Language.PlutusTx.Prelude (
    -- $prelude
    -- * String and tracing functions
    toPlutusString,
    trace,
    traceH,
    -- * Error
    error,
    -- * Boolean operators
    and,
    or,
    not,
    -- * Numbers
    min,
    max,
    -- * Rational numbers
    Ratio(..),
    fromRational,
    timesR,
    plusR,
    minusR,
    roundR,
    fromIntR,
    gtR,
    -- * Maybe
    isJust,
    isNothing,
    maybe,
    -- * Lists
    map,
    foldr,
    length,
    all,
    any,
    -- * Hashes
    ByteString,
    sha2_256,
    sha3_256,
    equalsByteString
    ) where

import           Data.ByteString.Lazy       (ByteString)        
import           Data.Ratio                 (Rational, denominator, numerator)
import           Prelude                    (Functor(..), Bool (..), Int, Maybe (..), Show, String, (<), (>), (+), (*), (-), (>=), fromInteger, div, rem)

import qualified Language.PlutusTx.Builtins as Builtins
import           Language.PlutusTx.Lift     (makeLift)

import           Language.Haskell.TH

-- $prelude
-- The PlutusTx Prelude is a collection of useful functions that work with 
-- builtin Haskell data types such as 'Maybe' and @[]@ (list).
--
-- Functions from the Prelude can be used with the the typed Template Haskell 
-- splice operator @$$()@:
--
-- @
--   import qualified Language.PlutusTx.Prelude as P
--   
--   [||  $$(P.traceH) "plutus" ... ||]
-- @

-- | Terminate the evaluation of the script with an error message
error :: Q (TExp (() -> a))
error = [|| Builtins.error ||]

-- | Convert a Haskell 'String' into a PlutusTx 'Builtins.String'.
toPlutusString :: Q (TExp (String -> Builtins.String))
toPlutusString =
    [||
    let f str = case str of
            [] -> Builtins.emptyString
            (c:rest) -> Builtins.charToString c `Builtins.appendString` f rest
    in f
    ||]

-- | Emit the given string as a trace message before evaluating the argument.
trace :: Q (TExp (Builtins.String -> a -> a))
trace = [||
         -- The builtin trace is just a side-effecting function that returns unit, so
         -- we have to be careful to make sure it actually gets evaluated, and not
         -- thrown away by GHC or the PIR compiler.
         \str a -> case (Builtins.trace str) of () -> a
         ||]

-- | A version of 'trace' that takes a Haskell 'String'.
traceH :: Q (TExp (String -> a -> a))
traceH = [|| \str a -> $$(trace) ($$(toPlutusString) str) a||]

-- | Logical AND
--
--   >>> $$([|| $$(and) True False ||])
--   False
--
and :: Q (TExp (Bool -> Bool -> Bool))
and = [|| \(l :: Bool) (r :: Bool) -> if l then r else False ||]

-- | Logical OR
--
--   >>> $$([|| $$(or) True False ||])
--   True
--
or :: Q (TExp (Bool -> Bool -> Bool))
or = [|| \(l :: Bool) (r :: Bool) -> if l then True else r ||]

-- | Logical negation
--
--   >>> $$([|| $$(not) True ||])
--   False
--
not :: Q (TExp (Bool -> Bool))
not = [|| \(a :: Bool) -> if a then False else True  ||]

-- | The smaller of two 'Int's
--
--   >>> $$([|| $$(min) 10 5 ||])
--   5
--
min :: Q (TExp (Int -> Int -> Int))
min = [|| \(a :: Int) (b :: Int) -> if a < b then a else b ||]

-- | The larger of two 'Int's
--
--   >>> $$([|| $$(max) 10 5 ||])
--   10
--
max :: Q (TExp (Int -> Int -> Int))
max = [|| \(a :: Int) (b :: Int) -> if a > b then a else b ||]

-- | A rational number consisting of numerator and denominator.
--   Construct 'Ratio' @Int@s with 'fromIntR' and convert to 
--   'Int's with 'roundR'.
data Ratio a = a :% a 
    deriving (Functor, Show)

makeLift ''Ratio

-- | Convert a 'Data.Ratio.Rational' to a 'Ratio' @Int@.
fromRational :: Rational -> Ratio Int
fromRational rt = fmap fromInteger (n :% d) where
    n = numerator rt
    d = denominator rt

-- | Multiply two 'Ratio' @Int@s.
--
-- >>> $$([|| roundR (1 :% 2) ||])
-- 1
-- >>> $$([|| roundR (1 :% 3) ||])
-- 0
--
timesR :: Q (TExp (Ratio Int -> Ratio Int -> Ratio Int))
timesR = [|| \(x :% y) (x' :% y') -> (x*x') :% (y*y') ||]

-- | Add two 'Ratio' @Int@s.
--
-- >>> $$([|| plusR (1 :% 2) (2 :% 3) ||])
-- 7 :% 6
--
plusR :: Q (TExp (Ratio Int -> Ratio Int -> Ratio Int))
plusR = [|| \(x :% y) (x' :% y') -> (x*y' + x'*y) :% (y*y') ||]

-- | Subtract a 'Ratio' @Int@s from another one.
--
-- >>> $$([|| minusR (1 :% 2) (2 :% 3) ||])
-- (-1) :% 6
--
minusR :: Q (TExp (Ratio Int -> Ratio Int -> Ratio Int))
minusR = [|| \(x :% y) (x' :% y') -> (x*y' - x'*y) :% (y*y') ||]

-- | Round a 'Ratio' @Int@ to the nearest integer. 0.5 is rounded
--   towards positive infinity.
--
-- >>> $$([|| roundR (1 :% 2) ||])
-- 1
-- >>> $$([|| roundR (1 :% 3) ||])
-- 0
--
roundR :: Q (TExp (Ratio Int -> Int))
roundR = [|| \(x :% y) -> 
    let i = x `div` y
        rm = x `rem` y
    in
        if (2 * rm >= y) then i + 1 else i
     ||]

-- | Check if a 'Ratio' @Int@ is greater than another one
gtR :: Q (TExp (Ratio Int -> Ratio Int -> Bool))
gtR = [|| \(x :% y) (x' :% y') ->
        -- x'' :% y'' = (x :% y) - (x' :% y')
        let x'' = x * y' - x'*y
            y'' = y * y'
        in
            if x'' > 0 
            then y'' > 0
            else y'' < 0
    ||]

-- | Convert an 'Int' to a 'Ratio' @Int@.
--
-- >>> $$([|| fromIntR 1 ||])
-- 1 :% 1
--
fromIntR :: Q (TExp (Int -> Ratio Int))
fromIntR = [|| \i -> i :% 1 ||]

-- | Check if a 'Maybe' @a@ is @Just a@
--
--   >>> $$([|| $$(isJust) Nothing ||])
--   False
--   >>> $$([|| $$(isJust) (Just "plutus") ||])
--   True
--
isJust :: Q (TExp (Maybe a -> Bool))
isJust = [|| \m -> case m of { Just _ -> True; _ -> False; } ||]

-- | Check if a 'Maybe' @a@ is @Nothing@
--
--   >>> $$([|| $$(isNothing) Nothing ||])
--   True
--   >>> $$([|| $$(isNothing) (Just "plutus") ||])
--   False
--
isNothing :: Q (TExp (Maybe a -> Bool))
isNothing = [|| \m -> case m of { Just _ -> False; _ -> True; } ||]

-- | PlutusTx version of 'Prelude.maybe'.
--
--   >>> $$([|| $$(maybe) "platypus" (\s -> s) (Just "plutus") ||])
--   "plutus"
--   >>> $$([|| $$(maybe) "platypus" (\s -> s) Nothing ||])
--   "platypus"
--
maybe :: Q (TExp (b -> (a -> b) -> Maybe a -> b))
maybe = [|| \b f m ->
    case m of
        Nothing -> b
        Just a  -> f a ||]

-- | PlutusTx version of 'Data.List.map'.
--
--   >>> $$([|| $$(map) (\i -> i + 1) [1, 2, 3] ||])
--   [2,3,4]
--
map :: Q (TExp ((a -> b) -> [a] -> [b]))
map = [||
    \f l ->
        let go ls = case ls of
                x:xs -> f x : go xs
                _    -> []
        in go l
        ||]

-- | PlutusTx version of 'Data.List.foldr'.
--
--   >>> $$([|| $$(foldr) (\i s -> s + i) 0 [1, 2, 3, 4] ||])
--   10
--
foldr :: Q (TExp ((a -> b -> b) -> b -> [a] -> b))
foldr = [||
    \f b l ->
        let go cur as = case as of
                []    -> cur
                a:as' -> go (f a cur) as'
        in go b l
    ||]

-- | 'length' @xs@ is the number of elements in @xs@.
--
--   >>> $$([|| $$(length) [1, 2, 3, 4] ||])
--   4
--
length :: Q (TExp ([a] -> Int))
length = [||
    \l ->
        -- it would be nice to define length in terms of foldr,
        -- but we can't, due to staging restrictions.
        let go lst = case lst of
                []   -> 0::Int
                _:xs -> 1 + go xs
        in go l
    ||]

-- | PlutusTx version of 'Data.List.all'.
--
--   >>> $$([|| $$(all) (\i -> i > 5) [6, 8, 12] ||])
--   True
-- 
all :: Q (TExp ((a -> Bool) -> [a] -> Bool))
all = [||
    \pred l ->
        let and' a b = if a then b else False
            go lst = case lst of
                []   -> True
                x:xs -> pred x `and'` go xs
        in go l
    ||]

-- | PlutusTx version of 'Data.List.any'.
--
--   >>> $$([|| $$(any) (\i -> i < 5) [6, 8, 12] ||])
--   False
-- 
any :: Q (TExp ((a -> Bool) -> [a] -> Bool))
any = [||
    \pred l ->
        let or' a b = if a then True else b
            go lst = case lst of
                []   -> False
                x:xs -> pred x `or'` go xs
        in go l
    ||]
    

-- | The double SHA256 hash of a 'ByteString'
sha2_256 :: Q (TExp (ByteString -> ByteString))
sha2_256 = [|| Builtins.sha2_256 ||]

-- | The triple SHA256 hash of a 'ByteString'
sha3_256 :: Q (TExp (ByteString -> ByteString))
sha3_256 = [|| Builtins.sha3_256 ||]

-- | Check if two 'ByteString's are equal
equalsByteString :: Q (TExp (ByteString -> ByteString -> Bool))
equalsByteString = [|| Builtins.equalsByteString ||]
