module Spec.Swap where

import           Control.Monad                                     (void)
import           Data.Either                                       (isRight)
import           Data.Foldable                                     (traverse_)
import qualified Data.Map                                          as Map

import           Hedgehog                                          (Property, forAll, property)
import qualified Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog                               (testProperty)

import qualified Language.PlutusTx.Prelude                         as P
import qualified Ledger
import           Ledger.Validation                                 (OracleValue (..))
import           Wallet.API                                        (PubKey (..))
import           Wallet.Emulator
import qualified Wallet.Generators                                 as Gen

import           Language.PlutusTx.Coordination.Contracts.Swap     (SwapParams (..), enterFixed, enterFloating)
import           Language.PlutusTx.Coordination.Contracts.Swap.TH0 (OracleLookup (..))

tests :: TestTree
tests = testGroup "swap" [
    testProperty "begin" enterFixedProp
    ]

enterBoth :: Trace MockWallet ()
enterBoth = do
    void $ walletAction w1 $ enterFixed lkp swp (PubKey 2)
    void $ walletAction w2 $ enterFloating lkp swp (PubKey 1)
    updateAll

exchange :: Trace MockWallet ()
exchange = do
    void $ walletAction w1 $ enterFixed lkp swp (PubKey 2)
    updateAll

enterFixedProp :: Property
enterFixedProp = checkTrace $ do
    enterBoth
    traverse_ (uncurry assertOwnFundsEq) [
        (w1, startingBalance - swapMarginPenalty swp),
        (w2, startingBalance - swapMarginPenalty swp)]

exchangePaymentsProp :: Property
exchangePaymentsProp = checkTrace $ do
    enterBoth
    exchange
    traverse_ (uncurry assertOwnFundsEq) [
        (w1, startingBalance - swapMarginPenalty swp),
        (w2, startingBalance - swapMarginPenalty swp)]

-- | Funds available to wallets at the beginning.
startingBalance :: Ledger.Value
startingBalance = 1000000

swp :: SwapParams
swp = SwapParams
    { swapNotionalAmount   = 50000
    , swapFirstObservation = 5
    , swapObservationTimes = [10, 15, 20]
    , swapFixedRate        = P.fromRational 0.045
    , swapMarginPenalty    = 1000
    , swapOracle           = oracle
    }

lkp :: OracleLookup
lkp = OracleLookup $ \h -> OracleValue oracle h (P.fromRational 0.050)

-- | Wallet 1
w1 :: Wallet
w1 = Wallet 1

-- | Wallet 2
w2 :: Wallet
w2 = Wallet 2

oracle :: PubKey
oracle = PubKey 17

checkTrace :: Trace MockWallet () -> Property
checkTrace t = property $ do
    let
        ib = Map.fromList [
            (PubKey 1, startingBalance),
            (PubKey 2, startingBalance)]
        model = Gen.generatorModel { Gen.gmInitialBalance = ib }
    (result, st) <- forAll $ Gen.runTraceOn model (updateAll >> t)
    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == _txPool st)

-- | Validate all pending transactions and notify all wallets
updateAll :: Trace MockWallet ()
updateAll =
    processPending >>= void . walletsNotifyBlock [w1, w2]
