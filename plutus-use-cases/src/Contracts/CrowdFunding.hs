-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Contracts.CrowdFunding (-- * Functionality for campaign contributors
                                contribute
                              , refund
                              , refundTrigger
                              -- * Functionality for campaign owners
                              , collect
                              , collectFundsTrigger
                              ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (unless)
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
                       (PlutusTx [| contributorPubKey |]) -- data script: ought to be lifted into PLC at coordination runtime
                    ]
      }
    -- the transaction above really ought to be merely a transaction *template* and the transaction fee ought to be
    -- added by the Wallet API Plutus library on the basis of the size and other costs of the transaction

contributionScript ::
       Campaign
    -> Maybe PubKey
    -> PlutusTx (Validator ())
contributionScript Campaign{..} contribPubKey =
    mkValidator $ Validator $ \rd _ PendingTx{..} ->
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
        unless (payToOwner || refundable) empty
    -- needs a splice adding the function triggering the Plutus Tx compiler plugin (compiled at coordination compile time)
    --
    -- What data is available inside the validator script?
    --
    -- The transaction references its input via their hashes, so we need to
    -- de-reference them to be able to look at the coin values of their
    -- outpus. De-referencing of a hash should probably also cost money
    -- (otherwise we could jam the validator by asking for data all the way
    -- back to the beginning of the chain)
    --
    -- (see https://github.com/input-output-hk/cardano-sl/blob/develop/docs/block-processing/types.md for a description of the data types that exist in a block)
    --
    -- [| \redeemer contributorPubKey ->
    --   let payToOwner = currentBlockHeight > paymentDeadline &&
    --                    currentBlockHeight < collectionDeadline &&
    --                    -- TODO: is there only a single address where the funds
    --                    -- go (which is what this line seems to imply)
    --                    -- or are the multiple addresses, one for each
    --                    -- contributor (which is what the signature of `collect`
    --                    -- seems to imply)
    --                    fundsAtAddress ownScriptAddress > fundingGoal &&
    --                    checkSig redeemer ownerPubKey
    --       refundable = currentBlockHeight > collectionDeadline &&
    --                    checkSig redeemer contributorPubKey
    --   in
    --     payToOwner || refundable
    -- |]


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
