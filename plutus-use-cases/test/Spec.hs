{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=Language.Plutus.CoreToPLC.Plugin -fplugin-opt Language.Plutus.CoreToPLC.Plugin:dont-typecheck #-}
module Main(main) where

import           Data.Bifunctor                                      (Bifunctor (..))
import           Data.Either                                         (isLeft, isRight)
import qualified Data.Map                                            as Map
import           Hedgehog                                            (Property, forAll, property)
import qualified Hedgehog
import qualified Hedgehog.Gen                                        as Gen
import qualified Hedgehog.Range                                      as Range
import           Lens.Micro
import           Test.Tasty
import           Test.Tasty.Hedgehog                                 (testProperty)

import           Wallet.API                                          (PubKey (..))
import           Wallet.Emulator                                     hiding (Value)
import           Wallet.Generators                                   (Mockchain (..))
import qualified Wallet.Generators                                   as Gen

import           Language.Plutus.Coordination.Contracts.CrowdFunding (Campaign (..), CampaignActor, CampaignPLC (..),
                                                                      contribute, refund)
import qualified Language.Plutus.Coordination.Contracts.CrowdFunding as CF
import           Language.Plutus.Coordination.Plutus                 (Hash (..), PendingTx (..), PendingTxIn (..),
                                                                      PendingTxOut (..), PendingTxOutRef (..), Value)
import           Language.Plutus.CoreToPLC.Plugin                    (plc)
import qualified Wallet.UTXO                                         as UTXO

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "use cases" [
    testGroup "crowdfunding" [
        testProperty "make a contribution" makeContribution,
        testProperty "make contributions and collect" successfulCampaign,
        testProperty "cannot collect money too early" cantCollectEarly,
        testProperty "cannot collect money too late" cantCollectLate,
        testProperty "can claim a refund" canRefund
        ]
    ]

-- | Example campaign
c1 :: CampaignPLC
c1 = CampaignPLC $ plc Campaign {
    campaignDeadline = 10,
    campaignTarget   = 1000,
    campaignCollectionDeadline = 15,
    campaignOwner              = PubKey 1
    }

-- | Make a contribution to the campaign from a wallet. Returns the reference
--   to the transaction output that is locked by the campaign's validator
--   script (and can be collected by the campaign owner)
contrib :: Wallet -> CampaignPLC -> Value -> Trace TxOutRef'
contrib w c v = exContrib <$> walletAction w (contribute c v) where
    exContrib = snd . head . filter (isPayToScriptOut . fst) . txOutRefs . head

-- | Collect the contributions of a crowdfunding campaign
collect :: Wallet -> CampaignPLC -> [(TxOutRef', Wallet, UTXO.Value)] -> Trace [Tx]
collect w c contributions = walletAction w $ CF.collect c ins where
    ins = first (PubKey . getWallet) <$> contributions

-- | Generate a transaction that contributes some funds to a campaign.
--   NOTE: This doesn't actually run the validation script. The script
--         will be run when the funds are retrieved (TBD)
makeContribution :: Property
makeContribution = property $ do
    let model = Gen.generatorModel
        w = Wallet 1
        contribution = 600
        rest = fromIntegral $ 100000 - contribution
    (result, st) <- forAll $ Gen.runTraceOn model $ do
        blockchainActions >>= walletNotifyBlock w
        contrib w c1 contribution
        blockchainActions >>= walletNotifyBlock w
        assertOwnFundsEq w rest

    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == emTxPool st)

-- | Run a campaign with two contributions where the campaign owner collects
--   the funds at the end
successfulCampaign :: Property
successfulCampaign = property $ do
    let [w1, w2, w3] = Wallet <$> [1..3]
        model = Gen.generatorModel { Gen.gmInitialBalance = Map.fromList [
            (PubKey 1, 0),
            (PubKey 2, 1000),
            (PubKey 3, 1000)] }
        updateAll = blockchainActions >>= walletsNotifyBlock [w1, w2, w3]
    (result, st) <- forAll $ Gen.runTraceOn model $ do
        updateAll
        con2 <- contrib w2 c1 600
        con3 <- contrib w3 c1 800
        updateAll
        setValidationData $ ValidationData $ plc PendingTx {
            pendingTxCurrentInput = (PendingTxIn (PendingTxOutRef 100 1) (), 600),
            pendingTxOtherInputs  = (PendingTxIn (PendingTxOutRef 200 1) (), 800):[],
            pendingTxOutputs      = []::[PendingTxOut CampaignActor],
            pendingTxForge        = 0,
            pendingTxFee          = 0,
            pendingTxBlockHeight  = 11
            }
        collect w1 c1 [(con2, w2, 600), (con3, w3, 800)]
        updateAll
        mapM_ (uncurry assertOwnFundsEq) [(w2, 400), (w3, 200), (w1, 1400)]
    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == emTxPool st)


-- | Check that the campaign owner cannot collect the monies before the campaign deadline
cantCollectEarly :: Property
cantCollectEarly = property $ do
    let [w1, w2, w3] = Wallet <$> [1..3]
        model = Gen.generatorModel { Gen.gmInitialBalance = Map.fromList [
            (PubKey 1, 0),
            (PubKey 2, 1000),
            (PubKey 3, 1000)] }
        updateAll = blockchainActions >>= walletsNotifyBlock [w1, w2, w3]
    (result, st) <- forAll $ Gen.runTraceOn model $ do
        updateAll
        con2 <- contrib w2 c1 600
        con3 <- contrib w3 c1 800
        updateAll
        setValidationData $ ValidationData $ plc PendingTx {
            pendingTxCurrentInput = (PendingTxIn (PendingTxOutRef 100 1) (), 600),
            pendingTxOtherInputs  = (PendingTxIn (PendingTxOutRef 200 1) (), 800):[],
            pendingTxOutputs      = []::[PendingTxOut CampaignActor],
            pendingTxForge        = 0,
            pendingTxFee          = 0,
            pendingTxBlockHeight  = 8
            }
        collect w1 c1 [(con2, w2, 600), (con3, w3, 800)]
        updateAll
        mapM_ (uncurry assertOwnFundsEq) [(w2, 400), (w3, 200), (w1, 0)]
    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == emTxPool st)


-- | Check that the campaign owner cannot collect the monies after the collection deadline
cantCollectLate :: Property
cantCollectLate = property $ do
    let [w1, w2, w3] = Wallet <$> [1..3]
        model = Gen.generatorModel { Gen.gmInitialBalance = Map.fromList [
            (PubKey 1, 0),
            (PubKey 2, 1000),
            (PubKey 3, 1000)] }
        updateAll = blockchainActions >>= walletsNotifyBlock [w1, w2, w3]
    (result, st) <- forAll $ Gen.runTraceOn model $ do
        updateAll
        con2 <- contrib w2 c1 600
        con3 <- contrib w3 c1 800
        updateAll
        setValidationData $ ValidationData $ plc PendingTx {
            pendingTxCurrentInput = (PendingTxIn (PendingTxOutRef 100 1) (), 600),
            pendingTxOtherInputs  = (PendingTxIn (PendingTxOutRef 200 1) (), 800):[],
            pendingTxOutputs      = []::[PendingTxOut CampaignActor],
            pendingTxForge        = 0,
            pendingTxFee          = 0,
            pendingTxBlockHeight  = 17
            }
        collect w1 c1 [(con2, w2, 600), (con3, w3, 800)]
        updateAll
        mapM_ (uncurry assertOwnFundsEq) [(w2, 400), (w3, 200), (w1, 0)]
    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == emTxPool st)


-- | Run a successful campaign that ends with a refund
canRefund :: Property
canRefund = property $ do
    let [w1, w2, w3] = Wallet <$> [1..3]
        model = Gen.generatorModel { Gen.gmInitialBalance = Map.fromList [
            (PubKey 1, 0),
            (PubKey 2, 1000),
            (PubKey 3, 1000)] }
        updateAll = blockchainActions >>= walletsNotifyBlock [w1, w2, w3]
    (result, st) <- forAll $ Gen.runTraceOn model $ do
        updateAll
        con2 <- contrib w2 c1 600
        con3 <- contrib w3 c1 800
        updateAll
        setValidationData $ ValidationData $ plc PendingTx {
            pendingTxCurrentInput = (PendingTxIn (PendingTxOutRef 100 1) (), 600),
            pendingTxOtherInputs  = []::[(PendingTxIn (), Value)],
            pendingTxOutputs      = []::[PendingTxOut CampaignActor],
            pendingTxForge        = 0,
            pendingTxFee          = 0,
            pendingTxBlockHeight  = 18
            }
        walletAction w2 (refund c1 con2 600)
        walletAction w3 (refund c1 con3 800)
        updateAll
        mapM_ (uncurry assertOwnFundsEq) [(w2, 1000), (w3, 1000), (w1, 0)]
    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == emTxPool st)
