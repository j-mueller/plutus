{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}
-- | A version of 'Language.Plutus.Contract.Contract' that
--   writes checkpoints
module Language.Plutus.Contract.State where

import           Control.Monad.State
import           Data.Aeson                        (Value)
import qualified Data.Aeson                        as Aeson
import           Data.Foldable                     (toList)
import           Data.Sequence                     (Seq, (|>))
import qualified Data.Sequence                     as Seq
import           GHC.Generics                      (Generic)

import           Language.Plutus.Contract.Contract
import           Language.Plutus.Contract.Event    as Event
import           Language.Plutus.Contract.Hooks    as Hooks

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data BinPath a = L !(BinPath a) | R !(BinPath a) | H !a
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

newtype ContractState e = ContractState { getContractState :: Either (Seq e) (BinTree Value) }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

initialState :: ContractState i
initialState = ContractState (Left mempty)

data StatefulContract i o a =
  StatefulContract {
      finalState  :: ContractState i
    , theContract :: Contract i o (BinPath a)
  } deriving (Functor)

mkStateful :: Contract i o a -> StatefulContract i o a
mkStateful = StatefulContract initialState . fmap H

applyInput :: StatefulContract i o a -> i -> Maybe (StatefulContract i o a)
applyInput sc i =
  case finalState sc of
    ContractState (Right _) -> Nothing
    ContractState (Left es) ->
      let es' = es |> i
      in Just (sc { finalState = ContractState (Left es') })

applyInputJSON :: (Monoid o, Aeson.ToJSON a) => StatefulContract i o a -> i -> Maybe (StatefulContract i o a)
applyInputJSON sc i =
  case finalState sc of
    ContractState (Right _) -> Nothing
    ContractState (Left es) ->
      let es' = es |> i
      in case drain (applyInputs (toList es') (theContract sc)) of
        (_, Pure a) ->
          Just (sc { finalState = ContractState (Right $ Leaf $ Aeson.toJSON a) })
        _ -> Just (sc { finalState = ContractState (Left es') })

drain' :: Monoid o => StatefulContract i o a -> (o, StatefulContract i o a)
drain' sc = undefined