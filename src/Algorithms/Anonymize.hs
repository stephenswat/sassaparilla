{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Algorithms.Anonymize (anonymizeInstruction) where

import Control.Monad (forM)
import Control.Monad.State (State, evalState, get)
import Data.Map (Map, lookup, empty, insert)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.))
import Lens.Micro.Mtl ((%=))

import Sass (Predicate(..), Instruction(..), Operand(..))

data AnonymizeState = AnonymizeState
    { _regularRegisterMap :: Map Int Int
    , _regularRegisterIndex :: Int
    , _predicateRegisterMap :: Map Int Int
    , _predicateRegisterIndex :: Int
    , _uniformRegisterMap :: Map Int Int
    , _uniformRegisterIndex :: Int
    , _barrierMap :: Map Int Int
    , _barrierIndex :: Int
    }

makeLenses ''AnonymizeState

anonymizePredicateState :: Predicate -> State AnonymizeState Predicate
anonymizePredicateState Predicate { inverse=i, register=r } = do
    st <- get
    case Data.Map.lookup r (st^.predicateRegisterMap) of
        Just x -> return Predicate{inverse=i, register=x}
        Nothing -> do
            let nr = st^.predicateRegisterIndex
            predicateRegisterMap %= (insert r nr)
            predicateRegisterIndex %= (+ 1)
            return Predicate{inverse=i, register=nr}

anonymizeOperandState :: Bool -> Operand -> State AnonymizeState Operand
anonymizeOperandState _ Register { register=_ } = do
    return Register { register=0 }
    {-
    st <- get
    case Data.Map.lookup r (st^.regularRegisterMap) of
        Just x -> return Register{register=x}
        Nothing -> do
            let nr = st^.regularRegisterIndex
            regularRegisterMap %= (insert r nr)
            regularRegisterIndex %= (+ 1)
            return Register { register=nr }
            -}
anonymizeOperandState _ PredicateRegister { } = do
    return PredicateRegister { register=0 }
anonymizeOperandState _ UniformRegister { } = do
    return UniformRegister { register=0 }
anonymizeOperandState _ EffectiveAddress {address=addr, offset=offs} = do
    newAddr <- anonymizeOperandState False addr
    newOffset <- forM offs (anonymizeOperandState False)
    return EffectiveAddress{address=newAddr, offset=newOffset}
anonymizeOperandState _ Barrier{} = do
    return Barrier{identifier=0}
anonymizeOperandState b Negative{operand=i} = do
    newOp <- anonymizeOperandState b i
    return Negative{operand=newOp}
anonymizeOperandState b Negate{operand=i} = do
    newOp <- anonymizeOperandState b i
    return Negate{operand=newOp}
anonymizeOperandState b Tilde{operand=i} = do
    newOp <- anonymizeOperandState b i
    return Tilde{operand=newOp}
anonymizeOperandState b Absolute{operand=i} = do
    newOp <- anonymizeOperandState b i
    return Absolute{operand=newOp}
anonymizeOperandState _ TextAddress{} = do
    return TextAddress{label=""}
anonymizeOperandState _ ConstantMemory {arg1=i1, arg2=i2} = do
    newOp1 <- anonymizeOperandState False i1
    newOp2 <- anonymizeOperandState False i2
    return ConstantMemory {arg1=newOp1, arg2=newOp2}
anonymizeOperandState b Immediate { value=v } = do
    return Immediate { value=if b then v else 0 }
anonymizeOperandState _ FloatImmediate { } = do
    return FloatImmediate {fvalue=0}
anonymizeOperandState _ o = do
    return o

anonymizeInstructionState :: Instruction -> State AnonymizeState Instruction
anonymizeInstructionState Instruction {predicate = pdf, op=opp, operands=ops} = do
    newPredicate <- case pdf of
        Just x -> do
            npdf <- anonymizePredicateState x
            return (Just npdf)
        Nothing -> return Nothing
    newOperands <- forM ops (anonymizeOperandState False)
    return Instruction {predicate=newPredicate, op=opp, operands=newOperands}

anonymizeInstruction :: Instruction -> Instruction
anonymizeInstruction i = evalState (anonymizeInstructionState i) initialState
    where
        initialState :: AnonymizeState
        initialState = AnonymizeState
            { _regularRegisterMap = Data.Map.empty
            , _regularRegisterIndex = 0
            , _predicateRegisterMap = Data.Map.empty
            , _predicateRegisterIndex = 0
            , _uniformRegisterMap = Data.Map.empty
            , _uniformRegisterIndex = 0
            , _barrierMap = Data.Map.empty
            , _barrierIndex = 0
            }
