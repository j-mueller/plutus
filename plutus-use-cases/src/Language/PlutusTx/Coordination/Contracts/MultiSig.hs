{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- | Implements an n-out-of-m multisig contract.
module Language.PlutusTx.Coordination.Contracts.MultiSig
    ( MultiSig(..)
    , msValidator
    , msDataScript
    , msRedeemer
    , msAddress
    , lock
    , initialise
    , unlockTx
    -- * Emulator traces
    , threeOutOfFive
    ) where

import           Control.Lens
import           Control.Monad                (void)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.Foldable                (fold)
import qualified Ledger.Ada                   as Ada
import qualified Ledger.Crypto                as Crypto
import qualified Ledger.Value                 as Value
import qualified Language.PlutusTx            as P
import           Ledger                       as Ledger hiding (initialise, to)
import           Ledger.Validation            as V
import           Wallet.API                   as WAPI
import qualified Wallet.Emulator.Types        as EM

data MultiSig = MultiSig 
                { signatories :: [Ledger.PubKey]
                -- ^ List of public keys of people who may sign the transaction
                , requiredSignatures :: Int
                -- ^ Minimum number of signatures required to unlock
                --   the output (should not exceed @length signatories@)
                }
P.makeLift ''MultiSig

msValidator :: MultiSig -> ValidatorScript
msValidator sig = 
  ValidatorScript (Ledger.applyScript mkValidator (Ledger.lifted sig)) where
    mkValidator = Ledger.fromCompiledCode ($$(P.compile [||
      let 
        validate :: MultiSig -> () -> () -> PendingTx -> ()
        validate (MultiSig keys num) () () p = 
            let 
                present = $$(P.length) ($$(P.filter) ($$(V.txSignedBy) p) keys)
            in
                if $$(P.geq) present num
                then ()
                else $$(P.error) ($$(P.traceH) "WRONG!" ())

      in
        validate
      ||]))

-- | Multisig data script (unit value).
msDataScript :: DataScript
msDataScript = DataScript $ Ledger.lifted ()

-- | Multisig redeemer (unit value).
msRedeemer :: RedeemerScript
msRedeemer = RedeemerScript $ Ledger.lifted ()

-- | The address of a 'MultiSig' contract.
msAddress :: MultiSig -> Address
msAddress = Ledger.scriptAddress . msValidator

-- | Lock some funds in a 'MultiSig' contract.
lock :: (WalletAPI m, WalletDiagnostics m) => MultiSig -> Value -> m ()
lock ms vl = payToScript_ defaultSlotRange (msAddress ms) vl msDataScript

-- | Instruct the wallet to start watching the contract address
initialise :: (WalletAPI m) => MultiSig -> m ()
initialise = startWatching . msAddress

-- | Create a transaction that spends the outputs at the contract address.
--   This transaction needs to be signed by @n@ of the signatories.
unlockTx 
    :: MultiSig 
    -- ^ List of signatories and required signatures
    -> Map.Map TxOutRef TxOut
    -- ^ Outputs at the contract address
    -> PubKey
    -- ^ Recipient of the funds
    -> Ledger.Tx
unlockTx ms utxos pk = 
    let 
        validator = msValidator ms

        mkIn :: TxOutRef -> TxIn
        mkIn r = Ledger.scriptTxIn r validator msRedeemer

        ins = Set.map mkIn (Map.keysSet utxos)
        val = fold (Ledger.txOutValue . snd <$> Map.toList utxos)

        output = pubKeyTxOut val pk

    in Ledger.Tx
            { txInputs = ins
            , txOutputs = [output]
            , txForge = Value.zero
            , txFee   = Ada.zero
            , txValidRange = defaultSlotRange
            , txSignatures = Map.empty
            }

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
            void $ EM.processPending

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
            in  tx' & signatures . at (toPublicKey pk) .~ Just sig

        -- The first @n@ wallets' private keys will be used to sign 'tx'
        signingKeys = take n (EM.walletPrivKey . EM.Wallet <$> [1..])

        signedTx = foldr attachSignature tx signingKeys

    EM.processEmulated $ do
        void $ EM.walletAction w2 (submitTxn signedTx)
        void $ EM.processPending
        EM.assertIsValidated signedTx    
