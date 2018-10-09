-- | A pyramid scheme as a PLC contract
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fplugin=Language.Plutus.CoreToPLC.Plugin -fplugin-opt Language.Plutus.CoreToPLC.Plugin:dont-typecheck #-}
module Language.Plutus.Coordination.Contracts.Pyramid (
    PyramidScheme(..),
    PyramidData (..),
    PyramidPLC(..),
    pyramidValidator
    ) where

import qualified Language.Plutus.CoreToPLC.Primitives as Prim
import           Language.Plutus.Runtime              (PendingTx (..), PendingTxIn (..), PendingTxOut (..),
                                                       PendingTxOutType (..), PubKey, Value)
import           Language.Plutus.TH                   (PlcCode, applyPlc, plutus)
import           Prelude                              hiding ((&&), (||))
import           Wallet.UTXO                          (Validator (..))

-- | A pyramid scheme. The scheme is organised as a tree (the "pyramid"). Later
--   entrants to the scheme have to pay an amount proportional to the depth of
--   the pyramid. Each node between the new entrant and the root of the tree can
--   take out some of those funds but only if they foward the remainder further
--   up the chain.
--   This ensures that everyone except those at the bottom of the pyramid
--   profits, and those closer to the top profit more.
newtype PyramidScheme = PyramidScheme {
    pyramidSchemeIncreaseAmount :: Value
    }

data PyramidData = PyramidData {
    pyramidDataHeight :: Int,
    pyramidDataParent :: PubKey
    }

newtype PyramidPLC = PyramidPLC PlcCode

-- | Lock funds for use in a pyramid scheme
pyramidValidator :: PyramidPLC -> Validator
pyramidValidator (PyramidPLC v) = Validator val where
    val = applyPlc inner v
    inner = $(plutus [| \PyramidScheme{..} () PyramidData{..} (p :: PendingTx () PyramidData) ->
        let

            -- | Returns true if the pending transaction was signed by the
            --   owner of the public key
            --   TODO: Implement when signature info is added to Pending Tx
            txSignedBy :: PubKey -> Bool
            txSignedBy _ = True

            infixr 3 ||
            (||) :: Bool -> Bool -> Bool
            (||) l r = if l then True else r

            infixr 3 &&
            (&&) :: Bool -> Bool -> Bool
            (&&) l r = if l then r else False

            PendingTx _ (_::[(PendingTxIn (), Value)]) os _ _ _ = p

            parentHeight = if pyramidDataHeight > 0
                           then pyramidDataHeight - 1
                           else 0

            outputsValid = case os of
                (PendingTxOut v' (Just d') DataTxOut::PendingTxOut PyramidData):(_::[PendingTxOut PyramidData]) -> case d' of
                    PyramidData h _ ->
                        h == parentHeight
                        && v' == h * pyramidSchemeIncreaseAmount
                _   -> False

            inputsValid = txSignedBy pyramidDataParent

            isValid = (parentHeight == 0 || outputsValid) && inputsValid
        in
        if isValid then () else Prim.error () |])

