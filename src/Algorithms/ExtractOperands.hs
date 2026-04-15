{-# LANGUAGE DuplicateRecordFields #-}

module Algorithms.ExtractOperands (extractOperandsFromInstruction) where

import Data.Set (Set, insert, empty)
import Control.Monad.State (State, execState, modify)
import Control.Monad (forM_)

import Sass

extractOperandsFromPredicateState :: Predicate -> State (Set Operand) ()
extractOperandsFromPredicateState Predicate { register=reg } = do
    modify (insert (PredicateRegister { register=reg }))

extractOperandsFromOperandState :: Operand -> State (Set Operand) ()
extractOperandsFromOperandState o@Register {} = do
    modify (insert o)
extractOperandsFromOperandState o@PredicateRegister {} = do
    modify (insert o)
extractOperandsFromOperandState o@UniformRegister {} = do
    modify (insert o)
extractOperandsFromOperandState o@Barrier {} = do
    modify (insert o)
extractOperandsFromOperandState o@EffectiveAddress {address=addr, offset=offs} = do
    modify (insert o)
    extractOperandsFromOperandState addr
    case offs of
        Just x -> do extractOperandsFromOperandState x
        Nothing -> return ()
extractOperandsFromOperandState o@Immediate {} = do
    modify (insert o)
extractOperandsFromOperandState Negative { operand=o } = do
    extractOperandsFromOperandState o
extractOperandsFromOperandState Negate { operand=o } = do
    extractOperandsFromOperandState o
extractOperandsFromOperandState Tilde { operand=o } = do
    extractOperandsFromOperandState o
extractOperandsFromOperandState Absolute { operand=o } = do
    extractOperandsFromOperandState o
extractOperandsFromOperandState o@ConstantMemory {arg1=i1, arg2=i2} = do
    modify (insert o)
    extractOperandsFromOperandState i1
    extractOperandsFromOperandState i2
extractOperandsFromOperandState _ = return ()

extractOperandsFromInstructionState :: Instruction -> State (Set Operand) ()
extractOperandsFromInstructionState Instruction { predicate=pdc, operands=opnds } = do
    case pdc of
        Just x -> extractOperandsFromPredicateState x 
        Nothing -> return ()
    forM_ opnds extractOperandsFromOperandState

extractOperandsFromInstruction :: Instruction -> Set Operand
extractOperandsFromInstruction i = execState (extractOperandsFromInstructionState i) Data.Set.empty
