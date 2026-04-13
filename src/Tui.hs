{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}

module Tui (startTui) where

import Lens.Micro ((^.))
import Lens.Micro.TH
import Lens.Micro.Mtl ((%=), (.=), use)
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe
import Text.Printf
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Table as Table
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Data.List (intersperse)
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<=>)
  , str
  , hLimit
  , vBox
  , hBox
  , withDefAttr
  )
import Brick.Widgets.Border (hBorder)
import Brick.Util (on)
import Data.Map (Map, lookup)

import Sass

type RowData = (Integer, (Instruction, Map String String))
type InstrData = Maybe RowData

data Row 
  = TextRow String String String String String String
  | InstrRow InstrData InstrData
  | HalfWidthText String String
  | FullWidthText String


allMetrics :: [String]
allMetrics = 
    [ "Instructions Executed"
    , "Predicated-On Thread Instructions Executed"
    , "Thread Instructions Executed"
    , "Warp Stall Sampling (All Samples)"
    , "Warp Stall Sampling (Not-issued Samples)"
    ]


data AppState =
    AppState { _rawList :: [(InstrData, InstrData)]
             , _tabularList :: L.List () Row
             , _fn1 :: String
             , _fn2 :: String
             , _showRelativeMetrics :: Bool
             , _hideRows :: Bool
             , _metricIndex :: Int
             , _totalMetricValue1 :: Integer
             , _totalMetricValue2 :: Integer
             }


makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI s = [ui]
    where
        l = s^.tabularList
        lbl = str $ "Row " <> cur <> " / " <> (show . length $ s^.tabularList)
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i -> show (i + 1)
        box = B.borderWithLabel lbl $
              hLimit totalWidth $
              listDrawElement s False (HalfWidthText (s^.fn1) (s^.fn2)) <=>
              listDrawElement s False headerRow <=>
              hBorder <=>
              L.renderList (listDrawElement s) True l <=>
              hBorder <=>
              str ("[M]etric: " ++ (allMetrics !! (s^.metricIndex))) <=>
              str ("[R]elative metrics: " ++ (if s^.showRelativeMetrics then "Yes" else "No")) <=>
              str ("[H]ide rows: " ++ (if s^.hideRows then "Yes" else "No"))
        ui = C.vCenter $ vBox [ C.hCenter box ]

toggleRelativeMetrics :: T.EventM () AppState ()
toggleRelativeMetrics = showRelativeMetrics %= not

toggleHideRows :: T.EventM () AppState ()
toggleHideRows = do
    hideRows %= not
    oldList <- use tabularList
    let oldIndex = case (L.listSelectedElement oldList) of
            (Just (0, _)) -> 0
            (Just (n, (InstrRow a b))) -> n
            _ -> 0
    let clipRow r = case r of
            (InstrRow a b) -> Just (fmap fst $ a, fmap fst $ b)
            _ -> Nothing
    let oldFixedElement = clipRow ((L.listElements oldList) `Vec.unsafeIndex` oldIndex)
    newHideRows <- use hideRows
    theRawList <- use rawList
    tabularList .= (L.list () (Vec.fromList (map renderRow . (if newHideRows then (compressRows 2) else noCompressRows) $ theRawList)) 1)
    newList <- use tabularList
    case L.listFindFirst ((== oldFixedElement) . (\e -> clipRow e)) newList of
        Just (x, _) -> tabularList %= (L.listMoveTo x)
        Nothing -> return ()

changeMetric :: Bool -> T.EventM () AppState ()
changeMetric b = do
    metricIndex %= (\x -> (x + (length allMetrics) + (if b then 1 else -1)) `rem` (length allMetrics))
    newMetricIndex <- use metricIndex
    rl <- use rawList
    totalMetricValue1 .= (sum . map (read . fromJust . Data.Map.lookup (allMetrics !! newMetricIndex) . snd . snd) . catMaybes . map fst $ rl)
    totalMetricValue2 .= (sum . map (read . fromJust . Data.Map.lookup (allMetrics !! newMetricIndex) . snd . snd) . catMaybes . map snd $ rl)
    return () 

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey (V.KChar 'q') [] -> M.halt
        V.EvKey (V.KChar 'Q') [] -> M.halt
        V.EvKey (V.KChar 'r') [] -> toggleRelativeMetrics
        V.EvKey (V.KChar 'R') [] -> toggleRelativeMetrics
        V.EvKey (V.KChar 'h') [] -> toggleHideRows
        V.EvKey (V.KChar 'H') [] -> toggleHideRows
        V.EvKey (V.KChar 'm') [] -> changeMetric True
        V.EvKey (V.KChar 'M') [] -> changeMetric False
        ev -> T.zoom tabularList $ L.handleListEvent ev
appEvent _ = return ()

makeColumns as ws ts = Table.alignColumns (intersperse Table.AlignLeft as) (intersperse 1 ws) (intersperse (str " ") ts)

listDrawElement :: AppState -> Bool -> Row -> Widget ()
listDrawElement st _ (TextRow a b c d e f) =
    hLimit totalWidth $
    hBox $
    makeColumns (columnHeaderAlignments ++ columnHeaderAlignments) (perSourceColumnWidths ++ perSourceColumnWidths) [str a, str b, str c, str d, str e, str f]
listDrawElement st sel (InstrRow a b) =
    let ws = [str (renderLineNr a), str (renderInstr a), str (renderProfileData (allMetrics !! (st^.metricIndex)) (st^.totalMetricValue1) (st^.showRelativeMetrics) a), str (renderLineNr b), str (renderInstr b), str (renderProfileData (allMetrics !! (st^.metricIndex)) (st^.totalMetricValue1) (st^.showRelativeMetrics) b)]
        maybeSelect es = selectCell <$> zip [0..] es
        selectCell (i, w) = case (sel, isNothing a, isNothing b, i) of
          (True, False, False, _) -> withDefAttr selectedCellAttr w
          (True, True, True, _) -> withDefAttr selectedCellAttr w
          (True, True, False, 8) -> withDefAttr loneSelectedCellAttr w
          (True, True, False, 2) -> withDefAttr missingSelectedCellAttr w
          (True, False, True, 8) -> withDefAttr missingSelectedCellAttr w
          (True, False, True, 2) -> withDefAttr loneSelectedCellAttr w
          (False, True, False, 8) -> withDefAttr loneCellAttr w
          (False, True, False, 2) -> withDefAttr missingCellAttr w
          (False, False, True, 8) -> withDefAttr missingCellAttr w
          (False, False, True, 2) -> withDefAttr loneCellAttr w
          (True, _, _, _) -> withDefAttr selectedCellAttr w
          _ -> w
    in hLimit totalWidth $
       hBox $
       maybeSelect $
       makeColumns (columnAlignments ++ columnAlignments) (perSourceColumnWidths ++ perSourceColumnWidths) ws
listDrawElement st sel (FullWidthText s) =
    hLimit totalWidth $
    hBox $
    maybeSelect $
    makeColumns [Table.AlignCenter] [totalWidth + 2 * (length perSourceColumnWidths)] [str s]
    where
        maybeSelect es = selectCell <$> zip [0..] es
        selectCell (_, w) = case (sel) of
            True -> withDefAttr annotationSelectedCellAttr w
            False -> withDefAttr annotationCellAttr w
listDrawElement st _ (HalfWidthText a b) =
    hLimit totalWidth $
    hBox $
    makeColumns [Table.AlignLeft, Table.AlignLeft] [l, l] [str a, str b]
    where
        l = (sum perSourceColumnWidths) + (length perSourceColumnWidths) - 1

selectedCellAttr :: A.AttrName
selectedCellAttr = A.attrName "selectedCell"

missingCellAttr :: A.AttrName
missingCellAttr = A.attrName "missingCell"

loneCellAttr :: A.AttrName
loneCellAttr = A.attrName "loneCell"

missingSelectedCellAttr :: A.AttrName
missingSelectedCellAttr = A.attrName "missingSelectedCell"

loneSelectedCellAttr :: A.AttrName
loneSelectedCellAttr = A.attrName "loneSelectedCell"

annotationCellAttr :: A.AttrName
annotationCellAttr = A.attrName "annotationCell"

annotationSelectedCellAttr :: A.AttrName
annotationSelectedCellAttr = A.attrName "annotationSelectedCell"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.black)
    , (selectedCellAttr,      V.black `on` V.white)
    , (missingCellAttr,       V.red   `on` V.black)
    , (loneCellAttr,          V.green `on` V.black)
    , (annotationCellAttr, V.cyan `on` V.black)
    , (annotationSelectedCellAttr, V.white `on` V.cyan)
    , (missingSelectedCellAttr, V.white   `on` V.red)
    , (loneSelectedCellAttr,          V.white `on` V.green)
    ]

sourceCellWidth :: Int
sourceCellWidth = 50

perSourceColumnWidths :: [Int]
perSourceColumnWidths = [6, sourceCellWidth, 8]

totalWidth :: Int
totalWidth = 2 * (sum perSourceColumnWidths) + 2 * (length perSourceColumnWidths) - 1

headerRow :: Row
headerRow = TextRow "Line" "Source" "Exec." "Line" "Source" "Exec."

columnHeaderAlignments :: [Table.ColumnAlignment]
columnHeaderAlignments = [Table.AlignRight, Table.AlignLeft, Table.AlignRight]

columnAlignments :: [Table.ColumnAlignment]
columnAlignments = [Table.AlignRight, Table.AlignLeft, Table.AlignRight]


renderLineNr :: Maybe RowData -> String
renderLineNr (Just (x, _)) = (show x)
renderLineNr Nothing = " "

renderInstr :: Maybe RowData -> String
renderInstr (Just (_, (x, _))) = show x
renderInstr Nothing = " "

renderProfileData :: String -> Integer -> Bool -> Maybe RowData -> String
renderProfileData n t r (Just (_, (_, m)))
    | r = printf "%.2f%%" (100.0 * ((fromInteger raw) / (fromInteger t)) :: Float)
    | raw > 0 = show raw
    | otherwise = " "
    where
        raw = read . fromJust . Data.Map.lookup n $ m :: Integer
renderProfileData _ _ _ Nothing = " "

renderRow :: (Either (Maybe RowData, Maybe RowData) Integer) -> Row
renderRow (Left (a, b)) = InstrRow a b
renderRow (Right n) = FullWidthText ("Hiding " ++ (show n) ++ " rows")

getInstExec :: RowData -> Integer
getInstExec (_, (_, m)) = read . fromJust . Data.Map.lookup "Instructions Executed" $ m

rowToBeKept :: (Maybe RowData, Maybe RowData) -> Bool
rowToBeKept (Just a, Just b) = frac <= 0.95 || frac >= 1.05
    where
        iExA = (getInstExec a)
        iExB = (getInstExec b)
        frac = ((fromInteger iExA) :: Float) / ((fromInteger iExB) :: Float)
rowToBeKept (Just a, Nothing) = (getInstExec a) > 0
rowToBeKept (Nothing, Just a) = (getInstExec a) > 0
rowToBeKept _ = True

compressRows :: Integer -> [(Maybe RowData, Maybe RowData)] -> [Either (Maybe RowData, Maybe RowData) Integer]
compressRows m q = go True q []
    where
        go f [] c
            | null c = []
            | otherwise = (map Left $ cpre) ++ [Right (toInteger . length $ cpost)]
            where
                (cpre, cpost) = splitAt (if f then 0 else fromInteger m) c
        go f (x:xs) c
            | null c = case rowToBeKept x of
                False -> go f xs [x]
                True -> (Left x):(go False xs [])
            | otherwise = case rowToBeKept x of
                False -> go f xs (c ++ [x])
                True -> (map Left $ cpre) ++ (if midLen > 0 then [Right midLen] else [])++(map Left $ cpost)++[Left x]++(go False xs [])
            where
                (cbuff, cpost) = splitAt ((\x -> x - (fromInteger m)) . length $ c) c
                (cpre, cmid) = splitAt (if f then 0 else fromInteger m) cbuff
                midLen = toInteger . length $ cmid


noCompressRows :: [(Maybe RowData, Maybe RowData)] -> [Either (Maybe RowData, Maybe RowData) Integer]
noCompressRows = map Left


startTui :: String -> String -> [(Maybe RowData, Maybe RowData)] -> IO ()
startTui fn1 fn2 is = void $ M.defaultMain theApp initialState
  where
    theApp = M.App { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const theMap
            }
    compressedRows = compressRows 2 is
    initialRows = map renderRow compressedRows
    rawInitialState =
      AppState { _rawList = is
               , _tabularList = L.list () (Vec.fromList initialRows) 1
               , _fn1=fn1
               , _fn2=fn2
               , _showRelativeMetrics=False
               , _hideRows=True
               , _metricIndex=0
               , _totalMetricValue1=(sum . map (read . fromJust . Data.Map.lookup (allMetrics !! 0) . snd . snd) . catMaybes . map fst $ is)
               , _totalMetricValue2=(sum . map (read . fromJust . Data.Map.lookup (allMetrics !! 0) . snd . snd) . catMaybes . map snd $ is)
               }
    initialState = rawInitialState
