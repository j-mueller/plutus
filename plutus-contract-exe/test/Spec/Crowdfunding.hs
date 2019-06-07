module Spec.Crowdfunding(tests) where

import           Test.Tasty

import qualified Ledger.Ada                        as Ada
import qualified Wallet.Emulator                   as EM

import           Examples.Crowdfunding
import           Language.Plutus.Contract.Contract as Con
import qualified Language.Plutus.Contract.Step     as Step

import           Spec.HUnit

tests :: TestTree
tests = testGroup "crowdfunding" [
    checkPredicate "Expose 'contribute' and 'scheduleCollection' endpoints"
        (endpointAvailable "contribute" <> endpointAvailable "schedule collection")
        $ pure . Step.step . fst $ Con.drain crowdfunding

    , checkPredicate "'contribute' endpoint submits a transaction"
        (anyTx <> interestingAddress (campaignAddress theCampaign)) $
        let key = EM.walletPubKey w1
            contribution = Ada.adaValueOf 10
        in fst <$> callEndpoint w1 "contribute" (key, contribution) crowdfunding

    , checkPredicate "'scheduleCollection' starts watching campaign address and waits for deadline"
        (waitingForSlot (campaignDeadline theCampaign) <> interestingAddress (campaignAddress theCampaign))
        (fst <$> callEndpoint w1 "schedule collection" () crowdfunding)
    ]

w1, w2 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
