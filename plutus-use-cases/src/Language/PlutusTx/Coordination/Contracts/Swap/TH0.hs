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

{- note [Data types in swap transaction outputs]

The swap is contract between two actors. One actor plays the 'Fixed' role and 
the other actor plays the 'Floating' role. Actors are indentified by their 
public key. During the lifetime of a contract an actor may transfer their stake 
in the contract to a different actor. The distinction between actor and role is 
important: The two roles stay the same during the entire lifetime of the 
contract, but a role may be played by different actors over time.

The swap is defined by its parameters, 'SwapParams'. Each swap (that is, each 
value of 'SwapParams') has *two* addresses, one for each role. This is achieved 
by parameterising the validator script with the product @(Role, SwapParams)@. 
The purpose of this construction is to allow actors to vary the details of 
their own role -- margins and actor identities -- independently, without 
needing the other actor's approval. At the same time, actions that involve the 
entire contract -- exchanging the payments -- can be performed by 
either actor because the addresses of the contract are known and don't change.

In terms of transaction outputs, we can visualise the contract as two paths of 
transactions connected by input-output pairs:

t1 -- t3 --+      +-- t5 --> utxo1
           +- t4 -+
t2 --------+      +--> utxo2


In this diagram, the top and bottom rows correspond to the two addresses in the 
contract. The swap is initialised by transactions t1 and t2. t3 and t5 are 
instances of margin payments. t4 is a payment on the interest rate (the actual 
swap), consuming from and producing to both addresses.

-}

data Role = Fixed | Floating
    deriving (Eq, Ord, Show)
PlutusTx.makeLift ''Role

newtype SwapOwner = SwapOwner { getSwapOwner :: PubKey }
    deriving (Eq, Ord, Show)
PlutusTx.makeLift ''SwapOwners

data OngoingSwap = OngoingSwap 
    { ongoingSwapOwner             :: SwapOwner
    , ongoingSwapFixedLegAddress   :: ValidatorHash
    , ongingSwapFloatingLegAddress :: ValidatorHash
    }

data SwapState = 
    Ongoing SwapOwner 
    | Settled
    deriving (Eq, Ord, Show)
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
  | AdjustMargin (OracleValue (Ratio Int))
  | ChangeOwner PubKey
  | NoAction

PlutusTx.makeLift ''SwapAction

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