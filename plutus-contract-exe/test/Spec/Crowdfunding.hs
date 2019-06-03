module Spec.Crowdfunding(tests) where

import           Control.Monad                                 (void)
import           Data.Either                                   (isRight)
import           Data.Foldable                                 (traverse_)
import qualified Data.Map                                      as Map

import           Hedgehog                                      (Property, forAll, property)
import qualified Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog                           (testProperty)
import qualified Test.Tasty.HUnit                              as HUnit

import qualified Ledger
import qualified Ledger.Ada                                    as Ada
import qualified Ledger.Value                                  as Value
import           Wallet.Emulator
import qualified Wallet.Emulator.Generators                    as Gen
import qualified Wallet.Generators                             as Gen

tests :: TestTree
tests = testGroup "crowdfunding" [
    HUnit.testCase "contribute" (pure ())
    ]
  