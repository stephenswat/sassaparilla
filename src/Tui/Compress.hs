module Tui.Compress (compressRows, noCompressRows) where

import Tui.Types (RowData)
import Data.Maybe (isJust, fromJust)
import Data.Sequence (Seq(..), (|>), empty, null, filter, singleton, splitAt)
import Data.Functor (unzip)
import Data.Map (lookup)
import Data.Foldable (toList)

rowToBeKept :: (Maybe RowData, Maybe RowData) -> Bool
rowToBeKept (Just a, Just b) = frac <= 0.95 || frac >= 1.05
    where
        iExA = (getInstExec a)
        iExB = (getInstExec b)
        frac = ((fromInteger iExA) :: Float) / ((fromInteger iExB) :: Float)
rowToBeKept (Just a, Nothing) = (getInstExec a) > 0
rowToBeKept (Nothing, Just a) = (getInstExec a) > 0
rowToBeKept (Nothing, Nothing) = error "Invalid state"

getInstExec :: RowData -> Integer
getInstExec (_, (_, m)) = read . fromJust . Data.Map.lookup "Instructions Executed" $ m

compressRows :: Integer -> [(Maybe RowData, Maybe RowData)] -> [Either (Maybe RowData, Maybe RowData) (Int, Int, Int, Int)]
compressRows m q = go True q Data.Sequence.empty
    where
        toSubrange (Empty) = (0, 0)
        toSubrange (h:<|Empty) = 
            ( fromInteger . fst . fromJust $ h
            , fromInteger . fst . fromJust $ h
            )
        toSubrange (h:<|(_:|>t)) =
            ( fromInteger . fst . fromJust $ h
            , fromInteger . fst . fromJust $ t
            )
        toRange xs = (lb, le, rb, re)
            where
                (ls, rs) = Data.Functor.unzip xs
                fls = Data.Sequence.filter isJust ls
                frs = Data.Sequence.filter isJust rs
                (lb, le) = toSubrange fls
                (rb, re) = toSubrange frs
        go f [] c
            | Data.Sequence.null c = []
            | otherwise =
                (map Left . toList $ cpre) ++ 
                (if (length cpost > 1) then [Right (toRange cpost)] else (map Left . toList $ cpost))
            where
                (cpre, cpost) = Data.Sequence.splitAt (if f then 0 else fromInteger m) c
        go f (x:xs) c
            | Data.Sequence.null c = case rowToBeKept x of
                False -> go f xs (Data.Sequence.singleton x)
                True -> (Left x):(go False xs Data.Sequence.empty)
            | otherwise = case rowToBeKept x of
                False -> go f xs (c |> x)
                True -> 
                    (map Left . toList $ cpre) ++ 
                    (if midLen > 1 then [Right . toRange $ cmid] else (map Left . toList $ cmid)) ++
                    (map Left . toList $ cpost) ++
                    [Left x] ++
                    (go False xs Data.Sequence.empty)
            where
                (cbuff, cpost) = Data.Sequence.splitAt ((\i -> i - (fromInteger m)) . length $ c) c
                (cpre, cmid) = Data.Sequence.splitAt (if f then 0 else fromInteger m) cbuff
                midLen = toInteger . length $ cmid


noCompressRows :: [(Maybe RowData, Maybe RowData)] -> [Either (Maybe RowData, Maybe RowData) (Int, Int, Int, Int)]
noCompressRows = map Left
