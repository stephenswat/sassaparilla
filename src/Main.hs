{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import System.Environment
import System.Exit (exitFailure)
import Text.Parsec (ParseError)
import Data.Algorithm.Diff
import Control.Monad
import Data.Map (Map, fromList, lookup)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)
import Data.Tuple.Extra (both)

import Sassaparilla.Tui.Core (startTui)
import Sassaparilla.Sass
import Sassaparilla.Utils
import Sassaparilla.Parse
import Sassaparilla.Algorithms.Anonymize (anonymizeInstruction)
import Sassaparilla.CsvIo (readLines)

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
    let mkMetricMap 
            = fromList 
            . filter (\(x, _) -> elem x usedMetrics || x == "Source") 
            . zip hdr
    let mapRws = map mkMetricMap rws
    forM mapRws (\x -> do
        pr <- filterParseResult 
           . parseInstruction 
           . fromJust 
           . Data.Map.lookup "Source" 
           $ x
        return (pr, x))

main :: IO ()
main = do 
    { args <- getArgs;

    ; f1 <- readInput (args !! 0);
    ; f2 <- readInput (args !! 1);

    ; let prepDiff = map (\m@(_, (i,_)) -> (anonymizeInstruction i, m)) . zip [1..]
    ; let (zl, zr) = both prepDiff (f1, f2)

    ; let diff = diffToRows . fmap (bimap snd snd) . getDiffBy ((==) `on` (fst)) zl $ zr

    ; startTui (args !! 0) (args !! 1) diff
    }


