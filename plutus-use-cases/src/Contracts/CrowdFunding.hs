-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE TemplateHaskell #-}
module Contracts.CrowdFunding (-- * Functionality for campaign contributors
                                contribute
                              , refund
                              , refundTrigger
                              -- * Functionality for campaign owners
                              , collect
                              , collectFundsTrigger
                              -- * Etc.
                              , await
                              ) where

import           Plutus

-- | Contribute funds to the campaign (contributor)
--
contribute :: PubKey -> Value -> Address -> TxM [TxOutRef]
contribute ownerPubKey value refundAddress = do
    assert (value > 0)
    contributorPubKey <- lookupMyPubKey
    myPayment         <- createPayment (value + standardTxFee)
    submitTransaction Tx
      { txInputs  = [myPayment]
      , txOutputs = [TxOutScript
                       value
                       (contributionScript ownerPubKey)
                       [| contributorPubKey |] -- data script: ought to be lifted into PLC at coordination runtime
                    ]
      }
    -- the transaction above really ought to be merely a transaction *template* and the transaction fee ought to be
    -- added by the Wallet API Plutus library on the basis of the size and other costs of the transaction

contributionScript :: PubKey -> PlutusTx
contributionScript ownerPubKey = 
    -- needs a splice adding the function triggering the Plutus Tx compiler plugin (compiled at coordination compile time)
    [| \redeemer contributorPubKey ->
      let payToOwner = currentBlockHeight > paymentDeadline &&
                       currentBlockHeight < collectionDeadline &&
                       -- TODO: is there only a single address where the funds
                       -- go (which is what this line seems to imply)
                       -- or are the multiple addresses, one for each
                       -- contributor (which is what the signature of `collect`
                       -- seems to imply)
                       fundsAtAddress ownScriptAddress > fundingGoal &&
                       checkSig witness ownerPubKey
          refundable = currentBlockHeight > collectionDeadline &&
                       checkSig witness contributorPubKey
      in
        payToOwner || refundable
    |]

-- | We can turn a validation script (belonging to an address) into an event
-- trigger (specific to a wallet) by partially evaluating the script.
--
-- For example, as a contributor, the `payToOwner` branch of the script above
-- is never true because we don't have the owner's private key and
-- `checkSig redeemer ownerPubKey` will always be false. On the other hand,
-- `checkSig redeemer contributorPubKey` can be made true (because we know the
-- private key) so we are left with `currentBlockHeight > collectionDeadline`
-- which can be expressed in terms of an `EventTrigger`.
--
-- Applying the same procedure on the owner's side, we get an event trigger
-- that looks at block height and funds at address.
--
await :: Validator -> TxM EventTrigger
await = undefined

-- Given the public key of the campaign owner, generate an event trigger that
-- fires when a refund is possible.
refundTrigger :: PubKey -> TxM EventTrigger
refundTrigger = await . contributionScript

-- Given the public key of the campaign owner, generate an event trigger that
-- fires when the funds can be collected.
collectFundsTrigger :: PubKey -> TxM EventTrigger
collectFundsTrigger = await . contributionScript

refund :: TxOutRef -> TxM [TxOutRef]
refund ref = do
    kp <- lookupMyKeyPair
    submitTransaction $ Tx {
      txInputs = [txInSign ref kp],
      txOutputs = [TxOutPubKey value (pubKeyAddress kp)]
    } where
      value = txOutRefValue ref + standardTxFee

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
