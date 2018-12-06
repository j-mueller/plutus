{-# LANGUAGE TemplateHaskell     #-}
-- this is in the default extensions, but for some reason when loading it during doctests it gets
-- confused if we don't have it
{-# LANGUAGE ScopedTypeVariables #-}
module Language.PlutusTx.Prelude (
    -- $prelude
    -- * String and tracing functions
    toPlutusString,
    trace,
    traceH,
    -- error is the only builtin that people are likely to want to use directly
    -- * Re-exported builtins
    error,
    -- * Boolean operators
    and,
    or,
    not,
    -- * Numbers
    min,
    max,
    -- * Maybe
    isJust,
    isNothing,
    maybe,
    -- * Lists
    map,
    foldr,
    length,
    all) where

import           Prelude                    (Bool (..), Int, Maybe (..), String, (<), (>), (+))

import qualified Language.PlutusTx.Builtins as Builtins
import           Language.PlutusTx.Builtins (error)

import           Language.Haskell.TH

-- $prelude
-- The PlutusTx Prelude is a collection of useful functions that work with 
-- builtin Haskell data types such as `Maybe` and `[]` (list).
--
-- Functions from the Prelude can be used with the @$$()@ operator:
--
-- @
--   import qualified Language.PlutusTx.Prelude as P
--   
--   [||  $$(P.traceH) "plutus" ... ||]
-- @

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

-- | The smaller of two `Int`s
--
--   >>> $$([|| $$(min) 10 5 ||])
--   5
--
min :: Q (TExp (Int -> Int -> Int))
min = [|| \(a :: Int) (b :: Int) -> if a < b then a else b ||]

-- | The larger of two `Int`s
--
--   >>> $$([|| $$(max) 10 5 ||])
--   10
--
max :: Q (TExp (Int -> Int -> Int))
max = [|| \(a :: Int) (b :: Int) -> if a > b then a else b ||]

-- | Check if a `Maybe` @a@ is @Just a@
--
--   >>> $$([|| $$(isJust) Nothing ||])
--   False
--   >>> $$([|| $$(isJust) (Just "plutus") ||])
--   True
--
isJust :: Q (TExp (Maybe a -> Bool))
isJust = [|| \m -> case m of { Just _ -> True; _ -> False; } ||]

-- | Check if a `Maybe` @a@ is @Nothing@
--
--   >>> $$([|| $$(isNothing) Nothing ||])
--   True
--   >>> $$([|| $$(isNothing) (Just "plutus") ||])
--   False
--
isNothing :: Q (TExp (Maybe a -> Bool))
isNothing = [|| \m -> case m of { Just _ -> False; _ -> True; } ||]

-- | The maybe function takes a default value, a function, and a `Maybe` value. 
--   If the `Maybe` value is `Nothing`, the function returns the default value. 
--   Otherwise, it applies the function to the value inside the `Just` and 
--   returns the result.
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

-- | `map` @f xs@ is the list obtained by applying @f@ to each element of @xs@
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

-- | `foldr`, applied to a binary operator, a starting value (typically the 
--   right-identity of the operator), and a list, reduces the list using the 
--   binary operator, from right to left:
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

-- | `length` @xs@ is the number of elements in @xs@.
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

-- | Applied to a predicate and a list, `all` determines if all elements of the 
--   list satisfy the predicate.
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

