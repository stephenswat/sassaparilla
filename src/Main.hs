{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import System.Environment
import System.Exit (exitFailure)
import System.IO
import Data.ByteString (ByteString, hGetSome, empty)
import Data.Csv.Incremental
import Text.Parsec (ParseError)
import Data.Either (partitionEithers)
import Data.Algorithm.Diff
import Control.Monad
import Data.Map (Map, fromList, lookup)
import Data.Function (on)
import Data.Maybe (fromJust)

import Tui (startTui)
import Sass
import Parse

anonymizePredicate :: Predicate -> Predicate
anonymizePredicate Predicate { inverse=i } = Predicate{inverse=i, register=0}

anonymizeOperand :: Operand -> Operand
anonymizeOperand Register { } = Register { register=0 }
anonymizeOperand PredicateRegister { } = PredicateRegister { register=0 }
anonymizeOperand UniformRegister { } = UniformRegister { register=0 }
anonymizeOperand EffectiveAddress {address=addr, offset=offs} = EffectiveAddress{address=anonymizeOperand addr, offset=(fmap anonymizeOperand offs)}
anonymizeOperand Barrier{} = Barrier{identifier=0}
anonymizeOperand Negative{operand=i} = Negative{operand=anonymizeOperand i}
anonymizeOperand Negate{operand=i} = Negate{operand=anonymizeOperand i}
anonymizeOperand Tilde{operand=i} = Tilde{operand=anonymizeOperand i}
anonymizeOperand Absolute{operand=i} = Absolute{operand=anonymizeOperand i}
anonymizeOperand TextAddress{} = TextAddress{label=""}
anonymizeOperand ConstantMemory {arg1=i1, arg2=i2} = ConstantMemory {arg1= anonymizeOperand i1, arg2= anonymizeOperand i2}
anonymizeOperand Immediate { } = Immediate { value=0 }
anonymizeOperand FloatImmediate { } = FloatImmediate {fvalue=0}
anonymizeOperand o = o

anonymizeInstruction :: Instruction -> Instruction
anonymizeInstruction Instruction {predicate = pdf, op=opp, operands=ops} = 
  Instruction {predicate = (fmap anonymizePredicate $ pdf), op=opp, operands=map anonymizeOperand ops}


feed :: (ByteString -> Data.Csv.Incremental.Parser [String]) -> Handle -> IO (Data.Csv.Incremental.Parser [String])
feed k csvFile = do
  hIsEOF csvFile >>= \case
    True  -> return $ k empty
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


