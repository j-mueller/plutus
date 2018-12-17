{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.Coordination.Contracts.Swap.TH0(
    SwapOwners(..),
    SwapMarginAccounts(..),
    SwapParams(..),
    SwapAction(..),
    SwapState(..),
    Role(..),
    Spread(..),
    margin,
    payment,
    firstExchange,
    lastExchange,
    paymentDates,
    receiver,
    -- * Etc.
    OracleLookup(..)
    ) where
  
import           Data.List.NonEmpty            (NonEmpty(..))
import           Data.Semigroup.Foldable.Class (Foldable1(..))
import           Data.Semigroup                (Min(..), Max(..))
import qualified Language.PlutusTx             as PlutusTx 
import qualified Language.PlutusTx.Prelude     as P
import           Language.PlutusTx.Prelude     (Ratio)
import           Language.Haskell.TH           (Q, TExp)
import           Ledger                        (Height (..), PubKey, Value(..))
import           Ledger.Validation             (OracleValue)

data SwapOwners = SwapOwners {
  swapOwnersFixedLeg :: PubKey,
  swapOwnersFloating :: PubKey
  }

PlutusTx.makeLift ''SwapOwners

data SwapMarginAccounts = SwapMarginAccounts {
  marginAccFixed :: Value,
  marginAccFloating :: Value
  }

PlutusTx.makeLift ''SwapMarginAccounts

data SwapState = Ongoing SwapOwners SwapMarginAccounts | Settled

PlutusTx.makeLift ''SwapState

data SwapParams = SwapParams {
  swapNotionalAmount   :: Value,
  swapFirstObservation :: Height,
  swapObservationTimes :: [Height],
  swapFixedRate        :: Ratio Int,
  swapMarginPenalty    :: Value,
  swapOracle           :: PubKey
  }

PlutusTx.makeLift ''SwapParams

-- | Oracle queries performed by the client (wallet). 
--
--   This type exists to make it easier to test the swap contract. In the future
--   "querying an oracle" should be an effect in the wallet API.
newtype OracleLookup = OracleLookup { lookupOracle :: Height -> OracleValue (Ratio Int) }

-- | Dates at which payments are exchanged
paymentDates :: SwapParams -> NonEmpty Height
paymentDates p = swapFirstObservation p :| swapObservationTimes p

-- | The block height at which the first payment is exchanged
firstExchange :: SwapParams -> Height
firstExchange = getMin . foldMap1 Min . paymentDates

-- | The block height at which the last payment is exchanged
lastExchange :: SwapParams -> Height
lastExchange = getMax . foldMap1 Max . paymentDates

data SwapAction = 
  Exchange (OracleValue (Ratio Int))
  | AdjustMarginFixedLeg (OracleValue (Ratio Int))
  | AdjustMarginFloatingLeg (OracleValue (Ratio Int))
  | ChangeOwnerFixedLeg PubKey
  | ChangeOwnerFloatingLeg PubKey
  | NoAction

PlutusTx.makeLift ''SwapAction

data Role = Fixed | Floating
    deriving (Eq, Ord, Show)

PlutusTx.makeLift ''Role

data Spread = Spread {
    fixedRate    :: Ratio Int,
    floatingRate :: Ratio Int,
    amount       :: Value,
    penalty      :: Value
    }

PlutusTx.makeLift ''Spread

-- | The 'Role' that receives the payment (difference between the two rates),
--   for a given floating rate
receiver :: Q (TExp (SwapParams -> Ratio Int -> Role))
receiver = [|| \swp floatingRate ->
        let SwapParams _ _ _ fixedRate _ _ = swp
        in 
            if $$(P.gtR) floatingRate fixedRate
            then Floating
            else Fixed
    ||]

-- | The payment for a given floating rate.
--   Always returns a positive value; the role that receives the payment
--   is determined by 'receiver'
payment :: Q (TExp (SwapParams -> Ratio Int -> Value))
payment = [|| \swp floatingRate ->
        let 
            SwapParams amt _ _ fixedRate _ _ = swp
            amt' = let Value v' = amt in $$(P.fromIntR) v'

            -- difference between the two rates
            rtDiff = $$(P.minusR) floatingRate fixedRate

            -- amount of money that changes hands in this exchange.
            delta = $$(P.roundR) ($$(P.timesR) amt' rtDiff)
        in
            if delta > 0 then Value delta else Value (-1 * delta)

    ||]

-- | The margin required for a spread between fixed and floating rate. This 
--   function will be used both in on-chain and in off-chain code.
margin :: Q (TExp (Role -> Spread -> Value))
margin = [|| \rol (Spread fixed floating (Value amt) (Value pnlty)) ->
    let
        amt' :: Ratio Int
        amt' = $$(P.fromIntR) amt

        marginFx :: Int
        marginFx = $$(P.roundR) ($$(P.timesR) amt' ($$(P.minusR) floating fixed))

        marginFl :: Int
        marginFl = $$(P.roundR) ($$(P.timesR) amt' ($$(P.minusR) fixed floating))

    in
        case rol of
            Fixed    -> Value (pnlty + ($$(P.max) 0 marginFx))
            Floating -> Value (pnlty + ($$(P.max) 0 marginFl))

      ||]