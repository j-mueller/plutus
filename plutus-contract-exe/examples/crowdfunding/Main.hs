{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}
-- | Contract interface for the crowdfunding contract
module Main where

import           Control.Lens                                          ((&), (.~))
import           Control.Monad                                         (void)
import           Network.Wai.Handler.Warp                              (run)

import           Language.Plutus.Contract                              (PlutusContract, both, endpoint,
                                                                        fundsAtAddressGt, slotGeq, writeTx)
import qualified Language.Plutus.Contract                              as C
import           Language.Plutus.Contract.Servant                      (contractApp)
import           Language.Plutus.Contract.Transaction                  (collectFromScript, payToScript, validityRange)
import           Language.PlutusTx.Coordination.Contracts.CrowdFunding (Campaign (..))
import qualified Language.PlutusTx.Coordination.Contracts.CrowdFunding as CF

import           Ledger                                                (PubKey, Value)
import qualified Ledger                                                as L
import qualified Ledger.Ada                                            as Ada
import           Ledger.Scripts                                        (DataScript (..))
import qualified Wallet.Emulator                                       as Emulator

main :: IO ()
main = run 8080 (contractApp crowdfunding)

crowdfunding :: PlutusContract ()
crowdfunding = contribute theCampaign

theCampaign :: Campaign
theCampaign = Campaign
    { campaignDeadline = 20
    , campaignTarget   = Ada.adaValueOf 100
    , campaignCollectionDeadline = 30
    , campaignOwner = Emulator.walletPubKey (Emulator.Wallet 1)
    }

contribute :: Campaign -> PlutusContract ()
contribute cmp = do
    (ownPK :: PubKey, contribution :: Value) <- endpoint "contribute"
    let ds = DataScript (L.lifted ownPK)
        tx = payToScript contribution (CF.campaignAddress cmp) ds
                & validityRange .~ L.interval 1 (campaignDeadline cmp)
    writeTx tx

scheduleCollection :: Campaign -> PlutusContract ()
scheduleCollection cmp = do
    () <- endpoint "schedule collection"
    let trg = fst <$> both
                (fundsAtAddressGt (CF.campaignAddress cmp) (campaignTarget cmp) )
                (slotGeq (CF.campaignDeadline cmp))
    void $ flip C.until (CF.campaignCollectionDeadline cmp) $ do
        inputs <- trg
        let
            redeemerScript = L.RedeemerScript (L.lifted CF.Collect)
            tx = collectFromScript inputs (CF.contributionScript cmp) redeemerScript
                    & validityRange .~ CF.collectionRange cmp
        writeTx tx
