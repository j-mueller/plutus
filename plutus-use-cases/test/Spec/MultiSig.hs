module Spec.MultiSig(tests) where

import           Control.Lens
import           Control.Monad                                     (void)
import           Data.Either                                       (isLeft, isRight)
import qualified Data.Map                                          as Map

import           Test.Tasty
import qualified Test.Tasty.HUnit                                  as HUnit

import           Language.PlutusTx.Coordination.Contracts.MultiSig as MS
import qualified Ledger.Ada                                        as Ada
import qualified Wallet.Emulator                                   as EM

tests :: TestTree
tests = testGroup "multisig" [
    HUnit.testCase "three out of five" (nOutOfFiveTest 3),
    HUnit.testCase "two out of five" (nOutOfFiveTest 2)
    ]

nOutOfFiveTest :: Int -> HUnit.Assertion
nOutOfFiveTest i = do
    let initialState = EM.emulatorStateInitialDist (Map.singleton (EM.walletPubKey (EM.Wallet 1)) (Ada.adaValueOf 10))
        (result, _) = EM.runEmulator initialState (MS.threeOutOfFive i)
        isOk = if i < 3 then isLeft result else isRight result
    HUnit.assertBool "transaction failed to validate" isOk

-- | A mockchain trace that locks funds in a three-out-of-five multisig
--   contract, and attempts to spend them using the given number of signatures.
--   @threeOutOfFive 3@ passes, @threeOutOfFive 2@ results in an
--   'EM.AssertionError'.
threeOutOfFive :: (EM.MonadEmulator m) => Int -> m ()
threeOutOfFive n = do
    -- a 'MultiSig' contract that requires three out of five signatures
    let
        w1 = EM.Wallet 1
        w2 = EM.Wallet 2
        ms = MultiSig
                { signatories = EM.walletPubKey . EM.Wallet <$> [1..5]
                , requiredSignatures = 3
                }
        addr = msAddress ms

    _ <- EM.processEmulated $ do
            void $ EM.addBlocksAndNotify [w1] 1
            void $ EM.walletAction w1 (lock ms $ Ada.adaValueOf 10)
            void EM.processPending

    -- get the UTXO set of the chain (this is a workaround because the emulator
    -- doesn't allow us to run 'unlockTx' in the context of a wallet. So we
    -- have use our omniscient powers to look at the mockchain in its entirety.)
    allOutputs <- use (EM.index . to getIndex)
    let relevantOutputs = Map.filter ((==) addr . txOutAddress) allOutputs

        -- The unlocking transaction, paying the funds to wallet 2.
        -- 'tx' does not have any signatures, so we need to sign
        -- it before wallet 2 can submit it.
        tx = unlockTx ms relevantOutputs (EM.walletPubKey w2)

        -- | Attach a signature to a transaction.
        attachSignature :: PrivateKey -> Tx -> Tx
        attachSignature pk tx' =
            let sig = Crypto.signTx (hashTx tx') pk
            in  tx' & signatures . at (toPublicKey pk) ?~ sig

        -- The first @n@ wallets' private keys will be used to sign 'tx'
        signingKeys = take n (EM.walletPrivKey . EM.Wallet <$> [1..])

        signedTx = foldr attachSignature tx signingKeys

    EM.processEmulated $ do
        void $ EM.walletAction w2 (submitTxn signedTx)
        void EM.processPending
        EM.assertIsValidated signedTx
