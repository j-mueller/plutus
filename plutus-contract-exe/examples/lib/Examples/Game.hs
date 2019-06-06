{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
-- | Contract interface for the guessing game
module Examples.Game(
      game
    , LockParams(..)
    , GuessParams(..)
    , gameAddress
    ) where

import           Control.Lens                                  (at, (^.))
import qualified Data.Aeson                                    as Aeson
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (fromMaybe)
import           GHC.Generics                                  (Generic)

import           Language.Plutus.Contract                      (PlutusContract, endpoint, nextTransactionAt, writeTx)
import           Language.Plutus.Contract.Transaction          (unbalancedTx)
import           Language.PlutusTx.Coordination.Contracts.Game (gameAddress, gameDataScript, gameRedeemerScript,
                                                                gameValidator)
import qualified Ledger                                        as L
import           Ledger.Ada                                    (Ada)
import qualified Ledger.Ada                                    as Ada
import qualified Ledger.AddressMap                             as AM

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: String
    , amount     :: Ada
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: String
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

guess :: PlutusContract ()
guess = do
    st <- nextTransactionAt gameAddress
    let mp = AM.fromTxOutputs st
    GuessParams theGuess <- endpoint "guess"
    let
        outputs  = Map.toList . fromMaybe Map.empty $ mp ^. at gameAddress
        redeemer = gameRedeemerScript theGuess
        inp      = (\o -> (L.scriptTxIn (fst o) gameValidator redeemer, L.txOutValue (snd o))) <$> outputs
        tx       = unbalancedTx inp []
    writeTx tx

lock :: PlutusContract ()
lock = do
    LockParams secret amt <- endpoint "lock"
    let
        vl         = Ada.toValue amt
        dataScript = gameDataScript secret
        output = L.TxOutOf gameAddress vl (L.PayToScript dataScript)
        tx     = unbalancedTx [] [output]
    writeTx tx

game :: PlutusContract ()
game = guess <> lock
