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
import Data.Sequence (Seq(..), empty, null, singleton, (|>), splitAt, filter)
import Data.Foldable (toList)
import qualified Data.Vector as Vec
import Data.Set (Set, empty, insert, member)
import Control.Monad.State (State, execState, modify)
import Control.Monad (forM_)
import Numeric (showHex)
import Data.List (intersperse, null)
import Brick.Types
  ( Widget
  )
import Data.Functor (unzip)
import Data.Char (isDigit)
import Brick.Widgets.Core
  ( (<=>)
  , str
  , hLimit
  , vBox
  , hBox
  , withDefAttr
  , withAttr
  )
import Brick.Widgets.Border (hBorder)
import Brick.Util (on)
import Data.Map (Map, lookup)
import Utils (usedMetrics)

import Sass

type RowData = (Integer, (Instruction, Map String String))
type InstrData = Maybe RowData

data Row 
  = TextRow String String String String String String
  | InstrRow InstrData InstrData
  | HalfWidthText String String
  | HiddenLinesRow Int Int Int Int
  | FullWidthText String



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

correlatedOperandAttr :: A.AttrName
correlatedOperandAttr = A.attrName "correlatedOperand"

correlatedSelectedOperandAttr :: A.AttrName
correlatedSelectedOperandAttr = A.attrName "correlatedSelectedOperand"


data AppState =
    AppState { _rawList :: [(InstrData, InstrData)]
             , _tabularList :: L.List () Row
             , _selectedRow :: Row
             , _fn1 :: String
             , _fn2 :: String
             , _showRelativeMetrics :: Bool
             , _highlightOperands :: Bool
             , _hideRows :: Bool
             , _metricIndex :: Int
             , _totalMetricValue1 :: Integer
             , _totalMetricValue2 :: Integer
             , _gotoBuffer :: [Char]
             }


extractOperandsFromPredicateState :: Predicate -> State (Set Operand) ()
extractOperandsFromPredicateState Predicate { inverse=inv, register=reg } = do
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
extractOperandsFromInstructionState Instruction { predicate=pdc, op=o, operands=opnds } = do
    case pdc of
        Just x -> extractOperandsFromPredicateState x 
        Nothing -> return ()
    forM_ opnds extractOperandsFromOperandState

extractOperandsFromInstruction :: Instruction -> Set Operand
extractOperandsFromInstruction i = execState (extractOperandsFromInstructionState i) Data.Set.empty

extractOperandsFromRow :: Row -> (Set Operand, Set Operand)
extractOperandsFromRow (InstrRow a b) = (fromMaybe Data.Set.empty (f a), fromMaybe Data.Set.empty (f b))
    where
        f = fmap (extractOperandsFromInstruction . fst . snd)
extractOperandsFromRow _ = (Data.Set.empty, Data.Set.empty)

renderPredicateAsBrickStr :: Set Operand -> Predicate -> Widget n
renderPredicateAsBrickStr cs Predicate { inverse=inv, register=reg } = hBox
    [ str ("@" ++ (if inv then "!" else ""))
    , applyStyle . str $ ("P" ++ (show reg))
    ]
    where
        correlated = member (PredicateRegister { register=reg }) cs
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id

renderOperandAsBrickStr :: Set Operand -> Operand -> Widget n
renderOperandAsBrickStr c o@Register { register=reg } = applyStyle. str $ ("R" ++ (show reg))
    where
        correlated = member o c
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id
renderOperandAsBrickStr c o@PredicateRegister { register=reg } = applyStyle . str $ ("P" ++ (show reg))
    where
        correlated = member o c
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id
renderOperandAsBrickStr c o@UniformRegister { register=reg } = applyStyle . str $ ("UR" ++ (show reg))
    where
        correlated = member o c
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id
renderOperandAsBrickStr c o@Barrier { identifier=i } = applyStyle . str $ ("B" ++ (show i))
    where
        correlated = member o c
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id
renderOperandAsBrickStr c o@EffectiveAddress {address=addr, offset=offs}
    | correlated = applyStyle . str $ ("[" ++ (show addr) ++ (if isJust offs then ("+" ++ (show . fromJust $ offs)) else "") ++ "]")
    | otherwise = hBox ([str "[", renderOperandAsBrickStr c addr] ++ (if isJust offs then [str "+", renderOperandAsBrickStr c . fromJust $ offs] else []) ++ [str "]"])
    where
        correlated = member o c
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id
renderOperandAsBrickStr c o@Immediate { value=v } = applyStyle . str $ ("0x" ++ (showHex v ""))
    where
        correlated = member o c
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id
renderOperandAsBrickStr c Negative { operand=o } = hBox [str "-", renderOperandAsBrickStr c o]
renderOperandAsBrickStr c Negate { operand=o } = hBox [str "!", renderOperandAsBrickStr c o]
renderOperandAsBrickStr c Tilde { operand=o } = hBox [str "~", renderOperandAsBrickStr c o]
renderOperandAsBrickStr c Absolute { operand=o } = hBox [str "|", renderOperandAsBrickStr c o, str "|"]
renderOperandAsBrickStr c o@ConstantMemory {arg1=i1, arg2=i2}
    | correlated = applyStyle . str $ ("c[" ++ (show i1) ++ "][" ++ (show i2) ++ "]")
    | otherwise = hBox [str "c[", renderOperandAsBrickStr c i1, str "][", renderOperandAsBrickStr c i2, str "]"]
    where
        correlated = member o c
        applyStyle = if correlated then (withAttr correlatedOperandAttr) else id

renderOperandAsBrickStr _ o = str (show o)

renderInstructionAsHBox :: Set Operand -> Instruction -> Widget n
renderInstructionAsHBox cs Instruction { predicate=pdc, op=o, operands=opnds } = hBox (
    [ fromMaybe (str "") (fmap ((\x -> hBox [x, str " "]) . renderPredicateAsBrickStr cs) pdc)
    , str (o)
    ] ++ 
    (if Data.List.null opnds then [] else [str " "]) ++ 
    (intersperse (str ", ") (fmap (renderOperandAsBrickStr cs) opnds)))

makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI s = [ui]
    where
        l = s^.tabularList
        lbl = str $ "Row " <> cur <> " / " <> (show . length $ s^.tabularList)
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i -> show (i + 1)
        corrs = if s^.highlightOperands then extractOperandsFromRow (s^.selectedRow) else (Data.Set.empty, Data.Set.empty)
        box = B.borderWithLabel lbl $
              hLimit totalWidth $
              listDrawElement corrs s False (HalfWidthText (s^.fn1) (s^.fn2)) <=>
              listDrawElement corrs s False headerRow <=>
              hBorder <=>
              L.renderList (listDrawElement corrs s) True l <=>
              hBorder <=>
              str ("[M]etric: " ++ (usedMetrics !! (s^.metricIndex))) <=>
              str ("[R]elative metrics: " ++ (if s^.showRelativeMetrics then "Yes" else "No")) <=>
              str ("[H]ide rows: " ++ (if s^.hideRows then "Yes" else "No")) <=>
              str ("High[l]ight operands: " ++ (if s^.highlightOperands then "Yes" else "No")) <=>
              str ("Goto buffer: " ++ (s^.gotoBuffer))
        ui = C.vCenter $ vBox [ C.hCenter box ]

toggleRelativeMetrics :: T.EventM () AppState ()
toggleRelativeMetrics = showRelativeMetrics %= not

toggleOperandHighlight :: T.EventM () AppState ()
toggleOperandHighlight = highlightOperands %= not

appendGotoBuffer :: Char -> T.EventM () AppState ()
appendGotoBuffer c = gotoBuffer %= (++ [c])

resetGotoBuffer :: T.EventM () AppState ()
resetGotoBuffer = gotoBuffer .= []

matchesRowNumber :: Bool -> Int -> Row -> Bool
matchesRowNumber right num (InstrRow a b) = case app of
    Just (x, _) -> (fromInteger x) >= num
    Nothing -> False
    where app = if right then b else a
matchesRowNumber right num (HiddenLinesRow lb le rb re)
    | right = rb <= num && re >= num
    | otherwise = lb <= num && le >= num
matchesRowNumber _ _ _ = False

launchGotoBuffer :: Bool -> T.EventM () AppState ()
launchGotoBuffer right = do
    list <- use tabularList
    buff <- use gotoBuffer
    let asInt = (read buff) :: Int
    case L.listFindFirst (matchesRowNumber right asInt) list of
        Just (x, _) -> tabularList %= (L.listMoveTo x)
        Nothing -> return ()


toggleHideRows :: T.EventM () AppState ()
toggleHideRows = do
    hideRows %= not
    oldList <- use tabularList
    let (right, num) = case (L.listSelectedElement oldList) of
            Just (_, (InstrRow (Just x) _)) -> (False, fromInteger . fst $ x)
            Just (_, (InstrRow _ (Just x) )) -> (True, fromInteger . fst $ x)
            Just (_, (HiddenLinesRow lb _ _ _)) -> (False, lb)
            Nothing -> (False, 0)
    newHideRows <- use hideRows
    theRawList <- use rawList
    tabularList .= (L.list () (Vec.fromList (map renderRow . (if newHideRows then (compressRows 2) else noCompressRows) $ theRawList)) 1)
    newList <- use tabularList
    case L.listFindFirst (matchesRowNumber right num) newList of
        Just (x, _) -> tabularList %= (L.listMoveTo x)
        Nothing -> return ()

changeMetric :: Bool -> T.EventM () AppState ()
changeMetric b = do
    metricIndex %= (\x -> (x + (length usedMetrics) + (if b then 1 else -1)) `rem` (length usedMetrics))
    newMetricIndex <- use metricIndex
    rl <- use rawList
    totalMetricValue1 .= (sum . map (read . fromJust . Data.Map.lookup (usedMetrics !! newMetricIndex) . snd . snd) . catMaybes . map fst $ rl)
    totalMetricValue2 .= (sum . map (read . fromJust . Data.Map.lookup (usedMetrics !! newMetricIndex) . snd . snd) . catMaybes . map snd $ rl)
    return () 

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey V.KBS [] -> do
            gotoBuffer %= init
        V.EvKey (V.KChar 'q') [] -> M.halt
        V.EvKey (V.KChar 'Q') [] -> M.halt
        V.EvKey (V.KChar 'r') [] -> do
            toggleRelativeMetrics
            resetGotoBuffer
        V.EvKey (V.KChar 'R') [] -> do
            toggleRelativeMetrics
            resetGotoBuffer
        V.EvKey (V.KChar 'h') [] -> do
            toggleHideRows
            resetGotoBuffer
        V.EvKey (V.KChar 'H') [] -> do
            toggleHideRows
            resetGotoBuffer
        V.EvKey (V.KChar 'l') [] -> do
            toggleOperandHighlight
            resetGotoBuffer
        V.EvKey (V.KChar 'L') [] -> do
            toggleOperandHighlight
            resetGotoBuffer
        V.EvKey (V.KChar 'm') [] -> do
            changeMetric True
            resetGotoBuffer
        V.EvKey (V.KChar 'M') [] -> do
            changeMetric False
            resetGotoBuffer
        V.EvKey (V.KChar '[') [] -> do
            launchGotoBuffer False
            resetGotoBuffer
        V.EvKey (V.KChar ']') [] -> do
            launchGotoBuffer True
            resetGotoBuffer
        V.EvKey (V.KChar x) []
            | isDigit x -> appendGotoBuffer x
            | otherwise -> resetGotoBuffer
        ev -> (do
            T.zoom tabularList $ L.handleListEvent ev
            tl <- use tabularList
            case (L.listSelectedElement tl) of
                Just (_, e) -> do
                    selectedRow .= e
                Nothing -> do
                    return ())
appEvent _ = return ()

makeColumns as ws ts = Table.alignColumns (intersperse Table.AlignLeft as) (intersperse 1 ws) (intersperse (str " ") ts)

listDrawElement :: (Set Operand, Set Operand) -> AppState -> Bool -> Row -> Widget ()
listDrawElement _ st _ (TextRow a b c d e f) =
    hLimit totalWidth $
    hBox $
    makeColumns (columnHeaderAlignments ++ columnHeaderAlignments) (perSourceColumnWidths ++ perSourceColumnWidths) [str a, str b, str c, str d, str e, str f]
listDrawElement (cl, cr) st sel (InstrRow a b) =
    let ws = [str (renderLineNr a), renderInstr ucl a, str (renderProfileData (usedMetrics !! (st^.metricIndex)) (st^.totalMetricValue1) (st^.showRelativeMetrics) a), str (renderLineNr b), renderInstr ucr b, str (renderProfileData (usedMetrics !! (st^.metricIndex)) (st^.totalMetricValue1) (st^.showRelativeMetrics) b)]
        (ucl, ucr) = if sel then (Data.Set.empty, Data.Set.empty) else (cl, cr)
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
listDrawElement _ st sel (FullWidthText s) =
    hLimit totalWidth $
    hBox $
    maybeSelect $
    makeColumns [Table.AlignCenter] [totalWidth + 2 * (length perSourceColumnWidths)] [str s]
    where
        maybeSelect es = selectCell <$> zip [0..] es
        selectCell (_, w) = case (sel) of
            True -> withDefAttr annotationSelectedCellAttr w
            False -> withDefAttr annotationCellAttr w
listDrawElement _ st sel (HiddenLinesRow lb le rb re) =
    hLimit totalWidth $
    hBox $
    maybeSelect $
    makeColumns [Table.AlignCenter] [totalWidth + 2 * (length perSourceColumnWidths)] 
        [str ("Hiding " ++ (show (le - lb)) ++ " rows: " ++ (show lb) ++ "-" ++ (show le) ++ " and "  ++ (show rb) ++ "-" ++ (show re))]
    where
        maybeSelect es = selectCell <$> zip [0..] es
        selectCell (_, w) = case (sel) of
            True -> withDefAttr annotationSelectedCellAttr w
            False -> withDefAttr annotationCellAttr w
listDrawElement _ st _ (HalfWidthText a b) =
    hLimit totalWidth $
    hBox $
    makeColumns [Table.AlignLeft, Table.AlignLeft] [l, l] [str a, str b]
    where
        l = (sum perSourceColumnWidths) + (length perSourceColumnWidths) - 1


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
    , (correlatedOperandAttr, (V.yellow `on` V.black) `V.withStyle` V.bold)
    , (correlatedSelectedOperandAttr, (V.yellow `on` V.white) `V.withStyle` V.bold)
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

renderInstr :: Set Operand -> Maybe RowData -> Widget n
renderInstr cs (Just (_, (x, _))) = renderInstructionAsHBox cs x
renderInstr _ Nothing = str (" ")

renderProfileData :: String -> Integer -> Bool -> Maybe RowData -> String
renderProfileData n t r (Just (_, (_, m)))
    | r = printf "%.2f%%" (100.0 * ((fromInteger raw) / (fromInteger t)) :: Float)
    | raw > 0 = show raw
    | otherwise = " "
    where
        raw = read . fromJust . Data.Map.lookup n $ m :: Integer
renderProfileData _ _ _ Nothing = " "

renderRow :: (Either (Maybe RowData, Maybe RowData) (Int, Int, Int, Int)) -> Row
renderRow (Left (a, b)) = InstrRow a b
renderRow (Right (lb, le, rb, re)) = HiddenLinesRow lb le rb re

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
rowToBeKept (Nothing, Nothing) = error "Invalid state"
rowToBeKept _ = True

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
            | otherwise = (map Left . toList $ cpre) ++ (if (length cpost > 0) then [Right (toRange cpost)] else [])
            where
                (cpre, cpost) = Data.Sequence.splitAt (if f then 0 else fromInteger m) c
        go f (x:xs) c
            | Data.Sequence.null c = case rowToBeKept x of
                False -> go f xs (Data.Sequence.singleton x)
                True -> (Left x):(go False xs Data.Sequence.empty)
            | otherwise = case rowToBeKept x of
                False -> go f xs (c |> x)
                True -> (map Left . toList $ cpre) ++ (if midLen > 0 then [Right . toRange $ cmid] else [])++(map Left . toList $ cpost)++[Left x]++(go False xs Data.Sequence.empty)
            where
                (cbuff, cpost) = Data.Sequence.splitAt ((\x -> x - (fromInteger m)) . length $ c) c
                (cpre, cmid) = Data.Sequence.splitAt (if f then 0 else fromInteger m) cbuff
                midLen = toInteger . length $ cmid


noCompressRows :: [(Maybe RowData, Maybe RowData)] -> [Either (Maybe RowData, Maybe RowData) (Int, Int, Int, Int)]
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
               , _selectedRow = head initialRows
               , _fn1=fn1
               , _fn2=fn2
               , _showRelativeMetrics=False
               , _highlightOperands=True
               , _hideRows=True
               , _metricIndex=0
               , _totalMetricValue1=(sum . map (read . fromJust . Data.Map.lookup (usedMetrics !! 0) . snd . snd) . catMaybes . map fst $ is)
               , _totalMetricValue2=(sum . map (read . fromJust . Data.Map.lookup (usedMetrics !! 0) . snd . snd) . catMaybes . map snd $ is)
               , _gotoBuffer=[]
               }
    initialState = rawInitialState
