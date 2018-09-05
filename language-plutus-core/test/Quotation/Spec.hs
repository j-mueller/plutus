{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Quotation.Spec (tests) where

import           Language.PlutusCore

import qualified Data.ByteString.Lazy as BSL
import           Data.Text.Encoding   (encodeUtf8)

import           Test.Tasty
import           Test.Tasty.Golden

tests :: TestTree
tests = testGroup "quasiquoter" [
  asGolden (runQuote unit) "test/Quotation/unit.plc",
  asGolden (runQuote one) "test/Quotation/one.plc",
  asGolden (runQuote bool) "test/Quotation/bool.plc",
  asGolden (runQuote true) "test/Quotation/true.plc",
  asGolden (runQuote false) "test/Quotation/false.plc",
  asGolden (runQuote free) "test/Quotation/free.plc",
  asGolden (runQuote eitherIUnit) "test/Quotation/eitherI.plc",
  asGolden (runQuote eitherEUnit) "test/Quotation/eitherE.plc",
  asGolden (runQuote $ unit >>= maybeI) "test/Quotation/maybeI.plc",
  asGolden (runQuote $ unit >>= contract) "test/Quotation/contract.plc"
  ]

asGolden :: PrettyCfg a => a -> TestName -> TestTree
asGolden a file = goldenVsString file (file ++ ".golden") (pure $ showTest a)

showTest :: PrettyCfg a => a -> BSL.ByteString
showTest = BSL.fromStrict . encodeUtf8 . debugText

unit :: Quote (Type TyName ())
unit = [plcType|(all a (type) (fun a a))|]

one :: Quote (Term TyName Name ())
one = [plcTerm|(abs a (type) (lam x a x))|]

bool :: Quote (Type TyName ())
bool = do
    u <- unit
    [plcType|(all a (type) (fun (fun u a) (fun (fun u a) a))) |]

true :: Quote (Term TyName Name ())
true = do
    u <- unit
    o <- one
    [plcTerm|(abs a (type) (lam x (fun u a) (lam y (fun u a) [x o])))|]

false :: Quote (Term TyName Name ())
false = do
    u <- unit
    o <- one
    [plcTerm|(abs a (type) (lam x (fun u a) (lam y (fun u a) [y o])))|]

free :: Quote (Term TyName Name ())
free = do
  -- both occurences should be the same variable
  f <- TyVar () <$> freshTyName () "free"
  [plcTerm|[(lam x f x) (lam x f x)]|]

-- `forall u . (a -> u) -> (b -> u) -> u`
eitherI :: Type TyName () -> Type TyName () -> Quote (Type TyName ())
eitherI a b = [plcType| (all u (type) (fun (fun a u) (fun (fun b u) u))) |]

eitherIUnit :: Quote (Type TyName ())
eitherIUnit = unit >>= \u1 -> unit >>= \u2 -> eitherI u1 u2

-- `(a -> c) -> (b -> c) -> eitherI a b -> c`
eitherE :: Type TyName () -> Type TyName () -> Type TyName () -> Quote (Type TyName ())
eitherE f g e = [plcType| [e f g] |]

eitherEUnit :: Quote (Type TyName ())
eitherEUnit = do
    u1 <- unit
    u2 <- unit
    e <- eitherI u1 u2
    b <- bool
    f <- [plcType| (fun u1 b) |]
    g <- [plcType| (fun u2 b) |]
    eitherE f g e

-- `forall u . (a -> u) -> u -> u`
maybeI :: Type TyName () -> Quote (Type TyName ())
maybeI a = [plcType| (all u (type) (fun (fun a u) (fun u u))) |]

-- (type of) a contract that expects inputs of the given type and produces 
-- either unit (when it is finished) or a new contract.
-- newtype σ = δ -> (Either ()  σ)
contract :: Type TyName () -> Quote (Type TyName ())
contract delta = do
    un <- unit
    [plcType| (fix sigma (fun delta (all u (type) (fun (fun un u) (fun (fun sigma u) u))))) |]