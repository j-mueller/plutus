module Spec.MultiSig(tests) where

import           Data.Either                                       (isLeft, isRight)
import qualified Data.Map                                          as Map

import           Test.Tasty
import qualified Test.Tasty.HUnit                                  as HUnit

import           Language.PlutusTx.Coordination.Contracts.MultiSig as MS
import qualified Ledger.Ada                                        as Ada
import qualified Wallet.Emulator                                   as EM

tests :: TestTree
tests = testGroup "multisig" [
    HUnit.testCase "three out of five" (nOutOfFive 3),
    HUnit.testCase "two out of five" (nOutOfFive 2)
    ]

nOutOfFive :: Int -> HUnit.Assertion
nOutOfFive i = do
    let initialState = EM.emulatorStateInitialDist (Map.singleton (EM.walletPubKey (EM.Wallet 1)) (Ada.adaValueOf 10))
        (result, _) = EM.runEmulator initialState (MS.threeOutOfFive i)
        isOk = if i < 3 then isLeft result else isRight result
    HUnit.assertBool "transaction failed to validate" isOk
