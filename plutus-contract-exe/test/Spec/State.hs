{-# LANGUAGE TypeApplications #-}
module Spec.State where

import           Test.Tasty

import           Control.Monad                     (void)
import qualified Data.Aeson                        as Aeson
import           Data.Either                       (isLeft, isRight)
import           Examples.Game
import           Language.Plutus.Contract          as Con
import           Language.Plutus.Contract.Contract as Con
import qualified Language.Plutus.Contract.Event    as Event
import qualified Language.Plutus.Contract.Hooks    as Hooks
import qualified Ledger.Ada                        as Ada
import qualified Wallet.Emulator                   as EM

import           Spec.HUnit
import qualified Test.Tasty.HUnit                  as HUnit

import qualified Language.Plutus.Contract          as Con
import qualified Language.Plutus.Contract.State    as S

import qualified Debug.Trace                       as Trace

tests :: TestTree
tests = testGroup "stateful contract"
    [ HUnit.testCase "construct initial state" $ do
        let con = Con.endpoint @String "endpoint"
            initial = Left (S.initialise @Event.Event @Hooks.BalancedHooks con)
            inp = Event.endpoint "endpoint" (Aeson.toJSON "asd")
            res = S.insertAndUpdate con initial inp
        HUnit.assertBool "init" (isRight res)
    , HUnit.testCase "construct two parallel branches" $ do
        let con = 
                let ep = Con.endpoint @String "endpoint"
                    a  = S.checkpoint ((,) <$> ep <*> ep)
                in Trace.trace (S.prtty a) a
            initial = Left (S.initialise @Event.Event @Hooks.BalancedHooks con)
            inp = Event.endpoint "endpoint" (Aeson.toJSON "asd")
            res = S.insertAndUpdate con initial inp
        HUnit.assertBool "para" (isRight $ Trace.traceShowId res)
    ]
