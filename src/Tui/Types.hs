module Tui.Types (RowData, InstrData, Row(..)) where

import Data.Map (Map)
import Sass (Instruction)

type RowData = (Integer, (Instruction, Map String String))
type InstrData = Maybe RowData

data Row 
  = TextRow String String String String String String
  | InstrRow InstrData InstrData
  | HalfWidthText String String
  | HiddenLinesRow Int Int Int Int
  | FullWidthText String
