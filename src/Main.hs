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
import Data.ByteString (ByteString, hGetSome, empty)
import Data.Csv.Incremental
import Text.Parsec (ParseError)
import Data.Either (partitionEithers)
import Data.Algorithm.Diff
import Control.Monad
import Data.Map (Map, fromList, lookup, empty, insert)
import Data.Function (on)
import Data.Maybe (fromJust)
import Control.Monad.State.Lazy (State, evalState, get, modify)

import Tui (startTui)
import Sass
import Parse

data AnonymizeState = AnonymizeState
    { _regularRegisterMap :: Map Integer Integer
    , _regularRegisterIndex :: Integer
    , _predicateRegisterMap :: Map Integer Integer
    , _predicateRegisterIndex :: Integer
    , _uniformRegisterMap :: Map Integer Integer
    , _uniformRegisterIndex :: Integer
    , _barrierMap :: Map Integer Integer
    , _barrierIndex :: Integer
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
anonymizeOperandState _ Register { register=r } = do
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
    return Immediate { value=if b then 0 else v }
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
    newOperands <- forM ops (anonymizeOperandState True)
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


feed :: (ByteString -> Data.Csv.Incremental.Parser [String]) -> Handle -> IO (Data.Csv.Incremental.Parser [String])
feed k csvFile = do
  hIsEOF csvFile >>= \case
    True  -> return $ k Data.ByteString.empty
    False -> do
      x <- hGetSome csvFile 4096
      k <$> (return x)

getCsvCells :: String -> IO [Either String [String]]
getCsvCells fn = do
  withFile fn ReadMode $ \ csvFile -> do
    let loop !_ (Fail _ errMsg) = do putStrLn errMsg; exitFailure
        loop acc (Many rs k)    = loop (acc ++ rs) =<< feed k csvFile
        loop acc (Done rs)      = do return (acc ++ rs)
    loop [] (decode HasHeader)

readLines :: String -> IO [[String]]
readLines f = do
  putStrLn ("Reading profile from " ++ f)
  errorCells <- getCsvCells f
  let (errors, cells) = partitionEithers errorCells
  if not (Prelude.null errors) then do
      putStrLn "Profile failed to parse";
      exitFailure
  else do
      putStrLn "Successfully read profile!"
      return cells


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
    let mapRws = map (fromList . zip hdr) rws
    let c = map (fromJust . Data.Map.lookup "Source") mapRws
    forM mapRws (\x -> do
        pr <- filterParseResult . parseInstruction . fromJust . Data.Map.lookup "Source" $ x
        return (pr, x))

main :: IO ()
main = do
  args <- getArgs;

  f1 <- readInput (args !! 0)
  f2 <- readInput (args !! 1)

  let diff = diffToRows $ getDiffBy ((==) `on` (anonymizeInstruction . fst . snd)) (zip [1..] f1) (zip [1..] f2)

  startTui (args !! 0) (args !! 1) diff


