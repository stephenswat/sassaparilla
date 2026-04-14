{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Lens.Micro ((^.))
import Lens.Micro.TH
import Lens.Micro.Mtl ((%=), (.=), use)
import System.Environment
import System.Exit (exitFailure)
import System.IO
import Data.ByteString.Lazy (ByteString, empty, readFile)
import Data.Csv
import Text.Parsec (ParseError)
import Data.Either (partitionEithers)
import Data.Algorithm.Diff
import Control.Monad
import Data.Map (Map, fromList, lookup, empty, insert)
import Data.Function (on)
import Data.Maybe (fromJust)
import Control.Monad.State (State, evalState, get)
import Data.Bifunctor (bimap)
import Data.Foldable (toList)

import Tui (startTui)
import Sass
import Utils
import Parse

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


readLines :: String -> IO [[String]]
readLines f = do
  putStrLn ("Reading profile from " ++ f)
  csvData <- Data.ByteString.Lazy.readFile f
  case decode HasHeader csvData of
      Left err -> do
          putStrLn ("Profile failed to parse: " ++ err);
          exitFailure
      Right v -> do
          putStrLn "Successfully read profile!"
          return (toList v)



filterParseResult :: Either ParseError Instruction -> IO Instruction
filterParseResult (Left a) = do
  putStrLn (show a)
  exitFailure
filterParseResult (Right a) = return a

diffToRows :: [PolyDiff a a] -> [(Maybe a, Maybe a)]
diffToRows = map go
  where
    go (Both x y) = (Just x, Just y)
    go (First x) = (Just x, Nothing)
    go (Second y) = (Nothing, Just y)

readInput :: String -> IO [(Instruction, Map String String)]
readInput fn = do
    (hdr:rws) <- readLines fn
    let mapRws = map (fromList . filter (\(x, _) -> elem x usedMetrics || x == "Source") . zip hdr) rws
    forM mapRws (\x -> do
        pr <- filterParseResult . parseInstruction . fromJust . Data.Map.lookup "Source" $ x
        return (pr, x))

main :: IO ()
main = do
  args <- getArgs;

  f1 <- readInput (args !! 0)
  f2 <- readInput (args !! 1)

  let zl = map (\m@(_, (i,_)) -> (anonymizeInstruction i, m)) (zip [1..] f1)
  let zr = map (\m@(_, (i,_)) -> (anonymizeInstruction i, m)) (zip [1..] f2)

  let diff = diffToRows . fmap (bimap snd snd) . getDiffBy ((==) `on` (fst)) zl $ zr

  startTui (args !! 0) (args !! 1) diff


