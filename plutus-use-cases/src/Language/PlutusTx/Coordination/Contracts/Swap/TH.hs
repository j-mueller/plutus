{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.Coordination.Contracts.Swap.TH(
    -- * Types
    SwapParams(..),
    SwapOwners(..),
    SwapMarginAccounts(..),
    SwapAction(..),
    SwapState(..),
    -- * Pieces of the state machine
    swapStep,
    swapStateEq,
    swapStateFinished
    ) where

import qualified Language.PlutusTx.Prelude                         as TH
import           Language.PlutusTx.Prelude                        (Ratio)
import qualified Language.PlutusTx                                as PlutusTx
import           Language.Haskell.TH                              (Q, TExp)
import           Ledger                                           (Height(..), Value(..))
import           Ledger.Validation                                (PendingTx', PendingTx(..), PendingTxOut(..))
import qualified Ledger.Validation                                as TH

import           Language.PlutusTx.Coordination.Contracts.Swap.TH0 as TH0

import Prelude   hiding ((&&))

swapStep :: Q (TExp ((Role, SwapParams) -> PendingTx' -> SwapState -> SwapAction -> SwapState))
swapStep = [|| \(role, swp) p st action -> 
    let
        SwapParams amt obsTime obsTimes fxr pnlty oraclePk = swp

        PendingTx _ _ _ _ currentHeight _ (vlh, _, _) = p

        SwapOwner fx = case st of
            Ongoing so -> so
            Settled -> $$(PlutusTx.error) () 

        infixr 3 &&
        (&&) :: Bool -> Bool -> Bool
        (&&) = $$(TH.and)

        currentMargin :: Role -> Ratio Int -> Value
        currentMargin rl flr = 
            let s = Spread {
                        fixedRate    = fxr,
                        floatingRate = flr,
                        amount       = amt,
                        penalty      = pnlty
                    }
            in $$(TH0.margin) rl s

        totalValOut = $$(TH.foldr) (\(PendingTxOut (Value v') _ _) v -> v + v') 0 ($$(TH.outputsOwnAddress) p)

        -- The last payment covered by the swap. 
        lastPaymentDate = 
            let Height start = obsTime in
            $$(TH.foldr) (\(Height h') h -> $$(TH.max) h' h) start obsTimes

        -- Whether a payment is scheduled for the current block
        canExchangeNow = $$(TH.any) ($$(TH.eqHeight) currentHeight) (obsTime:obsTimes)

        -- | Whether the transaction was signed by the current owner of the fixed leg of the contract
        signedFixed = $$(TH.txSignedBy) p fx

        -- | Whether the transaction was signed by the current owner of the floating leg of the contract
        signedFloating = $$(TH.txSignedBy) p fl

    in

    case action of
        Exchange ov -> 
            let                    
                rt = $$(TH.extractVerifyAt) ov oraclePk currentHeight
                
                -- difference between the two rates
                rtDiff :: Ratio Int
                rtDiff = $$(TH.minusR) rt fxr

                amt' :: Ratio Int
                amt' = let Value v' = amt in $$(TH.fromIntR) v'

                -- amount of money that changes hands in this exchange.
                delta :: Int
                delta = $$(TH.roundR) ($$(TH.timesR) amt' rtDiff)

                clamp :: Int -> Int -> Int -> Int
                clamp low high x = $$(TH.max) low ($$(TH.min) high x)

                -- TODO: Use direction, payment from TH0

                fixedMargin' = clamp 0 fixedMargin (fixedMargin + delta)
                floatingMargin' = clamp 0 floatingMargin (floatingMargin - delta)

                isLast = let Height c = currentHeight in c == lastPaymentDate

            in
                if canExchangeNow && totalValOut == fixedMargin' + floatingMargin'
                then 
                    if isLast 
                    then Settled 
                    else Ongoing 
                            (SwapOwners fx fl) 
                            (SwapMarginAccounts (Value fixedMargin') (Value floatingMargin'))

                else $$(PlutusTx.traceH) "Cannot exchange payments now" ($$(PlutusTx.error) ())


        AdjustMarginFixedLeg ov -> 
            let                     
                rt = $$(TH.extractVerifyAt) ov oraclePk currentHeight
                fixedMargin' = totalValOut - floatingMargin
                Value minFixedMargin = currentMargin Fixed rt 
            in
                if fixedMargin' >= minFixedMargin
                then Ongoing 
                        (SwapOwners fx fl) 
                        (SwapMarginAccounts (Value fixedMargin') (Value floatingMargin))

                else $$(PlutusTx.traceH) "AdjustMarginFixedLeg invalid" ($$(PlutusTx.error) ())

        AdjustMarginFloatingLeg ov -> 
            let 
                rt = $$(TH.extractVerifyAt) ov oraclePk currentHeight
                floatingMargin' = totalValOut - fixedMargin
                Value minFloatingMargin = currentMargin Floating rt 
            in
                if floatingMargin' >= minFloatingMargin
                then Ongoing 
                        (SwapOwners fx fl) 
                        (SwapMarginAccounts (Value fixedMargin) (Value floatingMargin'))

                else $$(PlutusTx.traceH) "AdjustMarginFixedLeg invalid" ($$(PlutusTx.error) ())

        ChangeOwnerFixedLeg fx' -> 
            -- to change the owner of the fixed leg of the 
            -- swap, 
            -- 1. the txn needs to be signed by the current owner
            -- 2. the new owner should be reflected in the new state
            if signedFixed && totalValOut == fixedMargin + floatingMargin
            then Ongoing 
                    (SwapOwners fx' fl) 
                    (SwapMarginAccounts (Value fixedMargin) (Value floatingMargin))

            else $$(PlutusTx.traceH) "ChangeOwnerFixedLeg invalid" ($$(PlutusTx.error) ())
                
        ChangeOwnerFloatingLeg fl' -> 
            -- to change the owner of the floating leg of the 
            -- swap, 
            -- 1. the txn needs to be signed by the current owner
            -- 2. the new owner should be reflected in the new state
            if signedFloating && totalValOut == fixedMargin + floatingMargin
            then Ongoing 
                    (SwapOwners fx fl') 
                    (SwapMarginAccounts (Value fixedMargin) (Value floatingMargin))

            else $$(PlutusTx.traceH) "ChangeOwnerFloatingLeg invalid" ($$(PlutusTx.error) ())
        NoAction -> $$(PlutusTx.traceH) "NoAction is not an action" ($$(PlutusTx.error) ())
                
    ||]

swapStateEq :: Q (TExp (SwapState -> SwapState -> Bool))
swapStateEq = [|| \l r -> 
        case (l, r) of
            (Settled, Settled) -> True

            (Ongoing (SwapOwners fxl fll) (SwapMarginAccounts (Value fxmL) (Value flmL)), Ongoing (SwapOwners fxr flr) (SwapMarginAccounts (Value fxmR) (Value flmR))) ->
                let and_ = $$(TH.and) in
                ($$(TH.eqPubKey) fxl fll) `and_` ($$(TH.eqPubKey) fxr flr) `and_` (fxmL == fxmR) `and_` (flmL == flmR)

            _ -> False
                
     ||]

swapStateFinished :: Q (TExp (SwapState -> Bool))
swapStateFinished = [|| \case Settled -> True; _ -> False ||]