module Spec.Game where

import           Control.Monad                                 (void)
import           Data.Either                                   (isRight)
import           Data.Foldable                                 (traverse_)
import qualified Data.Map                                      as Map

import           Hedgehog                                      (Property, forAll, property)
import qualified Hedgehog
import qualified Spec.Size                                     as Size
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
tests = testGroup "game" [
    HUnit.testCase "lock and unlock"
    ]

