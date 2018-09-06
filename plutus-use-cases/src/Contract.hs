-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE TemplateHaskell #-}
module Contract (-- * Functionality for campaign contributors
                  contribute
                , refund
                -- * Functionality for campaign owners
                , collect
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
                       fundsAtAddress ownScriptAddress > fundingGoal &&
                       checkSig witness ownerPubKey
          refundable = currentBlockHeight > collectionDeadline &&
                       checkSig witness contributorPubKey
      in
        payToOwner || refundable
    |]

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
