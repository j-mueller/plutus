-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=Language.Plutus.CoreToPLC.Plugin #-}
module Contracts.CrowdFunding (-- * Functionality for campaign contributors
                                contribute
                              , refund
                              , refundTrigger
                              -- * Functionality for campaign owners
                              , collect
                              , collectFundsTrigger
                              ) where

import           Control.Monad       (unless)

import           Language.Plutus.TH
import           Language.PlutusCore.TH (plcType)
import qualified Language.PlutusCore                      as PC
import           Plutus

data Campaign = Campaign
    { campaignDeadline           :: !BlockHeight
    , campaignTarget             :: !Value
    , campaignCollectionDeadline :: !BlockHeight
    , campaignOwner              :: !PubKey
    }

-- | Contribute funds to the campaign (contributor)
--
contribute :: Campaign -> Value -> TxM [TxOutRef]
contribute c value = do
    assert (value > 0)
    contributorPubKey <- lookupMyPubKey
    myPayment         <- createPayment (value + standardTxFee)
    submitTransaction Tx
      { txInputs  = [myPayment]
      , txOutputs = [TxOutScript
                       value
                       (contributionScript c (Just contributorPubKey))
                       (PlutusTx $$(plutusT [|| contributorPubKey ||])) -- data script: ought to be lifted into PLC at coordination runtime
                    ]
      }
    -- the transaction above really ought to be merely a transaction *template* and the transaction fee ought to be
    -- added by the Wallet API Plutus library on the basis of the size and other costs of the transaction

contributionScript ::
       Campaign
    -> Maybe PubKey
    -> PlutusTx
contributionScript cp pk  = result where

    -- PLC types for the arguments that are provided by the coordination code
    campaignTp :: PC.Type PC.TyName () -- Campaign
    campaignTp = undefined
    keyTp :: PC.Type PC.TyName () -- Maybe PubKey
    keyTp = undefined

    -- Here we need to convert `cp` and `pk` to their PLC representations and
    -- apply them to the `inner` lambda.
    result = undefined

    -- A PLC validator function with 5 arguments. The coordination code is 
    -- expected to supply two arguments when a transaction is created. The
    -- remaining 3 arguments are supplied by the Cardano node that validates the
    -- transaction.
    inner = $$(plutusT [||  (\rd _ PendingTx{..} Campaign{..} contribPubKey ->
        let pledgedFunds = sum
                            $ txOutRefValue . txInOutRef
                            <$> txInputs pendingTxTransaction
            -- Check that a refund transaction only spends the amount that was
            -- pledged by the contributor identified by `contribPubKey`
            contributorOnly = flip (maybe False) contribPubKey $ \k ->
                                all (flip signedBy k  . txInRedeemer)
                                $ txInputs pendingTxTransaction
            signedBy :: Hash -> PubKey -> Bool
            signedBy _ _ = undefined
            redHash :: Redeemer -> Hash
            redHash = undefined
            payToOwner   = pendingTxBlockHeight > campaignDeadline &&
                           pendingTxBlockHeight <= campaignCollectionDeadline &&
                           pledgedFunds >= campaignTarget &&
                           signedBy (redHash rd) campaignOwner
            -- In case of a refund, we can only collect the funds that
            -- were committed by this contributor
            refundable   = pendingTxBlockHeight > campaignCollectionDeadline &&
                           contributorOnly &&
                           maybe False (signedBy (redHash rd)) contribPubKey
        in
        unless (payToOwner || refundable) Nothing) ||])


-- | Given the campaign data
refundTrigger :: Campaign -> [TxOutRef] -> TxM EventTrigger
refundTrigger c = undefined

-- | Given the public key of the campaign owner, generate an event trigger that
-- fires when the funds can be collected.
collectFundsTrigger :: Campaign -> TxM EventTrigger
collectFundsTrigger c = undefined

refund :: TxOutRef -> TxM [TxOutRef]
refund ref = do
    kp <- lookupMyKeyPair
    submitTransaction $ Tx {
      txInputs = [txInSign ref kp],
      txOutputs = [TxOutPubKey value (pubKeyAddress kp)]
    } where
      value = txOutRefValue ref + standardTxFee -- TODO: Should the fee be subtracted? I thought the fee was (Sum of all input values) - (Sum of all output values)

-- TODO
-- refund: automatically triggered by an event
-- triggering collection: register for event that signals successful funding

-- | Collect all campaign funds (campaign owner)
--
-- NB: Simplifing assumption: the number of contributions doesn’t
--     exceed the number of inputs that we can put into a single
--     transaction.
--
collect :: [TxOutRef] -> TxM [TxOutRef]
collect outRefs = do
    assert (not $ null outRefs)
    ownerKeyPair <- lookupMyKeyPair
    submitTransaction Tx
      { txInputs  = [txInSign outRef ownerKeyPair | outRef <- outRefs]
      , txOutputs = [TxOutPubKey value (pubKeyAddress ownerKeyPair)]
      }
    where
      value = sum [txOutRefValue outRef | outRef <- outRefs] + standardTxFee
