{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE MagicHash #-}

module Sassaparilla.Sass (Operand(..), Instruction(..), Predicate(..)) where

import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import Numeric (showHex)

data Predicate = Predicate
    { inverse :: Bool
    , register :: Int
    } deriving (Eq)

data Operand
    = Register
    { register :: Int
    } 
    | PredicateRegister
    { register :: Int
    }
    | TruePredicate
    | FalsePredicate
    | UniformTruePredicate
    | UniformFalsePredicate
    | UniformRegister
    { register :: Int
    }
    | UniformZeroRegister
    | Barrier
    { identifier :: Int
    }
    | ZeroRegister
    | SpecialRegister
    { name :: String
    }
    | EffectiveAddress
    { address :: Operand
    , offset :: Maybe Operand
    }
    | Immediate
    { value :: Int
    }
    | FloatImmediate
    { fvalue :: Float
    }
    | Negative
    { operand :: Operand
    }
    | Negate
    { operand :: Operand
    }
    | Tilde -- TODO: What does this do??
    { operand :: Operand
    }
    | Absolute
    { operand :: Operand
    }
    | PositiveInfinity
    | NegativeInfinity
    | TextAddress
    { label :: String
    }
    | ConstantMemory
    { arg1 :: Operand
    , arg2 :: Operand
    }
    deriving (Eq, Ord)

data Instruction = Instruction 
    { predicate :: Maybe Predicate
    , op :: String
    , operands :: [Operand]
    } deriving (Eq)

instance Show Predicate where
    show Predicate { inverse=inv, register=reg } =
        "@" ++ (if inv then "!" else "") ++ "P" ++ (show reg)

instance Show Instruction where
    show Instruction { predicate=pdc, op=o, operands=opnds } =
        predStr ++ o ++ argStr
        where
            predStr = case pdc of
                Just x -> (show x) ++ " "
                Nothing -> ""
            argStr = case opnds of
                [] -> ""
                x -> " " ++ (intercalate ", " . map show $ x)


instance Show Operand where
    show Register { register=reg } = "R" ++ (show reg)
    show ZeroRegister = "RZ"
    show TruePredicate = "PT"
    show FalsePredicate = "PF"
    show UniformTruePredicate = "UPT"
    show UniformFalsePredicate = "UPF"
    show SpecialRegister { name=n } = "SR" ++ n
    show PredicateRegister { register=reg } = "P" ++ (show reg)
    show UniformRegister { register=reg } = "UR" ++ (show reg)
    show UniformZeroRegister = "URZ"
    show Barrier { identifier=i } = "B" ++ (show i)
    show EffectiveAddress {address=addr, offset=offs} =
        "[" ++ 
        (show addr) ++ 
        (if isJust offs then ("+" ++ (show . fromJust $ offs)) else "") ++
        "]"
    show Immediate { value=v } = "0x" ++ (showHex v "")
    show FloatImmediate { fvalue=v } = (show v)
    show Negative { operand=o } = "-" ++ (show o)
    show Negate { operand=o } = "!" ++ (show o)
    show Tilde { operand=o } = "~" ++ (show o)
    show Absolute { operand=o } = "|" ++ (show o) ++ "|"
    show TextAddress { label=l } = "`(" ++ l ++ ")"
    show ConstantMemory {arg1=i1, arg2=i2} =
        "c[" ++ (show i1) ++ "][" ++ (show i2) ++ "]"
    show PositiveInfinity = "+INF"
    show NegativeInfinity = "-INF"
