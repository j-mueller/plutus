{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind #-}
module Spec.Crowdfunding(tests) where

import qualified Spec.Lib                                              as Lib
import           Test.Tasty
import qualified Test.Tasty.HUnit                                      as HUnit

import           Language.Plutus.Contract.Test
import qualified Language.PlutusTx                                     as PlutusTx
import           Language.PlutusTx.Coordination.Contracts.CrowdFunding
import qualified Ledger.Ada                                            as Ada

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

tests :: TestTree
tests = testGroup "crowdfunding"
    [ checkPredicate "Expose 'contribute' and 'scheduleCollection' endpoints"
        crowdfunding
        (endpointAvailable w1 "contribute" <> endpointAvailable w1 "schedule collection")
        $ pure ()

    , checkPredicate "'contribute' endpoint submits a transaction"
        crowdfunding
        (interestingAddress w1 (campaignAddress theCampaign) <> walletFundsChange w1 (Ada.adaValueOf (-10)))
        $ let key = walletPubKey w1
              contribution = Ada.adaValueOf 10
        in callEndpoint w1 "contribute" (key, contribution)
                >> handleBlockchainEvents w1
                >> notifyInterestingAddresses w1

    , checkPredicate "'scheduleCollection' starts watching campaign address and waits for deadline"
        crowdfunding
        (waitingForSlot w1 (campaignDeadline theCampaign) <> interestingAddress w1 (campaignAddress theCampaign))
        $ callEndpoint w1 "schedule collection" ()
    , Lib.goldenPir "test/Spec/crowdfunding.pir" $$(PlutusTx.compile [|| mkValidator ||])
    ,   let
            deadline = 10
            target = Ada.adaValueOf 1000
            collectionDeadline = 15
            owner = w1
            cmp = mkCampaign deadline target collectionDeadline owner
        in HUnit.testCase "script size is reasonable" (Lib.reasonable (contributionScript cmp) 50000)
    ]
