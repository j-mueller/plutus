{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Spec.Crowdfunding
import qualified Spec.Game
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "plutus-contract-exe" [
    Spec.Crowdfunding.tests,
    Spec.Game.tests
    ]
