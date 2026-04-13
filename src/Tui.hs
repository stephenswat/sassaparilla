{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Tui (startTui) where

import Lens.Micro ((^.))
import Lens.Micro.TH
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe
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
data InstrData = InstrData (Maybe RowData)

data Row 
  = TextRow String String String String String String
  | InstrRow InstrData InstrData
  | HalfWidthText String String
  | FullWidthText String

data AppState =
    AppState { _tabularList :: L.List () Row
             , _fn1 :: String
             , _fn2 :: String
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
              listDrawElement False (HalfWidthText (s^.fn1) (s^.fn2)) <=>
              listDrawElement False headerRow <=>
              hBorder <=>
              L.renderList (listDrawElement) True l
        ui = C.vCenter $ vBox [ C.hCenter box ]

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt
        ev -> T.zoom tabularList $ L.handleListEvent ev
appEvent _ = return ()

makeColumns as ws ts = Table.alignColumns (intersperse Table.AlignLeft as) (intersperse 1 ws) (intersperse (str " ") ts)

listDrawElement :: Bool -> Row -> Widget ()
listDrawElement _ (TextRow a b c d e f) =
    hLimit totalWidth $
    hBox $
    makeColumns (columnHeaderAlignments ++ columnHeaderAlignments) (perSourceColumnWidths ++ perSourceColumnWidths) [str a, str b, str c, str d, str e, str f]
listDrawElement sel (InstrRow (InstrData a) (InstrData b)) =
    let ws = [str (renderLineNr a), str (renderInstr a), str (renderProfileData a), str (renderLineNr b), str (renderInstr b), str (renderProfileData b)]
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
listDrawElement sel (FullWidthText s) =
    hLimit totalWidth $
    hBox $
    maybeSelect $
    makeColumns [Table.AlignCenter] [totalWidth] [str s]
    where
        maybeSelect es = selectCell <$> zip [0..] es
        selectCell (_, w) = case (sel) of
            True -> withDefAttr annotationSelectedCellAttr w
            False -> withDefAttr annotationCellAttr w
listDrawElement _ (HalfWidthText a b) =
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

perSourceColumnWidths :: [Int]
perSourceColumnWidths = [6, 50, 10]

totalWidth :: Int
totalWidth = 2 * (sum perSourceColumnWidths)

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

renderProfileData :: Maybe RowData -> String
renderProfileData (Just (_, (_, m))) = fromJust . Data.Map.lookup "Instructions Executed" $ m
renderProfileData Nothing = " "

renderRow :: (Either (Maybe RowData, Maybe RowData) Integer) -> Row
renderRow (Left (a, b)) = InstrRow (InstrData a) (InstrData b)
renderRow (Right n) = FullWidthText ("Hiding " ++ (show n) ++ " rows")

cellToBeKept :: RowData -> Bool
cellToBeKept (_, (_, m)) = nExec > 0
    where
        nExec = (read . fromJust . Data.Map.lookup "Instructions Executed" $ m) :: Integer

rowToBeKept :: (Maybe RowData, Maybe RowData) -> Bool
rowToBeKept (Just _, Just _) = False
rowToBeKept (Just a, Nothing) = cellToBeKept a
rowToBeKept (Nothing, Just a) = cellToBeKept a
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
    initialState =
      AppState { _tabularList = L.list () (Vec.fromList initialRows) 1
               , _fn1=fn1
               , _fn2=fn2
               }
