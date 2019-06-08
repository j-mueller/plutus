module Spec.Crowdfunding(tests) where

import           Control.Monad                     (void)
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
        crowdfunding
        (endpointAvailable "contribute" <> endpointAvailable "schedule collection")
        $ void drain_

    , checkPredicate "'contribute' endpoint submits a transaction"
        crowdfunding
        (anyTx <> interestingAddress (campaignAddress theCampaign))
        $ let key = EM.walletPubKey w1
              contribution = Ada.adaValueOf 10
          in callEndpoint w1 "contribute" (key, contribution)

    , checkPredicate "'scheduleCollection' starts watching campaign address and waits for deadline"
        crowdfunding
        (waitingForSlot (campaignDeadline theCampaign) <> interestingAddress (campaignAddress theCampaign))
        $ callEndpoint w1 "schedule collection" ()
    ]

w1 :: EM.Wallet
w1 = EM.Wallet 1
