module Tui.Attributes where

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L (listAttr)
import Brick.Util (on)
import qualified Graphics.Vty as V

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

attributeMap :: A.AttrMap
attributeMap = A.attrMap V.defAttr
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
