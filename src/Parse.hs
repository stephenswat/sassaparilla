{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}

module Parse (parseInstruction) where

import Data.Text (pack, null)
import Data.Text.Read (hexadecimal)
import Data.Maybe (isJust, fromJust)
import Text.Parsec
    ( Parsec, ParseError, choice, try, parse, letter, many, many1, eof, char
    , string, sepBy1, digit, optionMaybe, (<|>)
    )
import Sass (Operand(..), Instruction(..), Predicate(..))

parseInstruction :: String -> Either ParseError Instruction
parseInstruction x = parse instructionParser x x

registerParser :: Parsec String () Operand
registerParser = do
    _ <- char 'R'
    num <- many1 (digit)
    return Register { register=(read num) }

predicateRegisterParser :: Parsec String () Operand
predicateRegisterParser = do
    _ <- char 'P'
    num <- many1 (digit)
    return PredicateRegister { register=(read num) }

uniformRegisterParser :: Parsec String () Operand
uniformRegisterParser = do
    _ <- string "UR"
    num <- many1 (digit)
    return UniformRegister { register=(read num) }

uniformZeroRegisterParser :: Parsec String () Operand
uniformZeroRegisterParser = do
    _ <- string "URZ"
    return UniformZeroRegister

barrierParser :: Parsec String () Operand
barrierParser = do
    _ <- char 'B'
    num <- many1 (digit)
    return Barrier { identifier=(read num) }

zeroRegisterParser :: Parsec String () Operand
zeroRegisterParser = do
    _ <- string "RZ"
    return ZeroRegister

truePredicateParser :: Parsec String () Operand
truePredicateParser = do
    _ <- string "PT"
    return TruePredicate

falsePredicateParser :: Parsec String () Operand
falsePredicateParser = do
    _ <- string "PF"
    return FalsePredicate

uniformTruePredicateParser :: Parsec String () Operand
uniformTruePredicateParser = do
    _ <- string "UPT"
    return UniformTruePredicate

uniformFalsePredicateParser :: Parsec String () Operand
uniformFalsePredicateParser = do
    _ <- string "UPF"
    return UniformFalsePredicate

specialRegisterParser :: Parsec String () Operand
specialRegisterParser = do
    _ <- string "SR"
    n <- many (digit <|> letter <|> (char '.') <|> (char '_'))
    return SpecialRegister {name=n}

hexDigit :: Parsec String () Char
hexDigit = choice 
    [ digit
    , char 'a'
    , char 'b'
    , char 'c'
    , char 'd'
    , char 'e'
    , char 'f'
    ]

hexStringParser :: Parsec String () Integer
hexStringParser = do
    _ <- string "0x"
    val <- many1 hexDigit
    case hexadecimal (pack ("0x" ++ val)) of
        Right (x, r) -> 
            if (not . Data.Text.null $ r) 
            then (fail "Not a hex number") 
            else return x
        _ -> fail "Not a hex number"

immediateParser :: Parsec String () Operand
immediateParser = do
    val <- hexStringParser
    return Immediate {value=fromInteger val}

floatImmediateParser :: Parsec String () Operand
floatImmediateParser = do
    prePeriod <- many1 digit
    postPeriod <- optionMaybe (do
        _ <- char '.'
        many1 digit)
    let pStr = 
            if isJust postPeriod 
            then ("." ++ fromJust postPeriod) 
            else ""
    let fStr = prePeriod ++ pStr
    return FloatImmediate {fvalue=read fStr}

constantMemoryParser :: Parsec String () Operand
constantMemoryParser = do
    _ <- string "c["
    addr1 <- operandParser
    _ <- string "]["
    addr2 <- operandParser
    _ <- string "]"
    return ConstantMemory {arg1 = addr1, arg2 = addr2}

effectiveAddressParser :: Parsec String () Operand
effectiveAddressParser = do
    _ <- string "["
    opp <- try operandParser
    _ <- optionMaybe (string ".64")
    off <- optionMaybe offsetParser
    _ <- string "]"
    return EffectiveAddress {address=opp, offset=off}
    where
        offsetParser = do
            _ <- char '+'
            q <- operandParser
            return q

negativeParser :: Parsec String () Operand
negativeParser = do
    _ <- char '-'
    opp <- operandParser
    return Negative { operand = opp }

negateParser :: Parsec String () Operand
negateParser = do
    _ <- char '!'
    opp <- operandParser
    return Negate { operand = opp }

tildeParser :: Parsec String () Operand
tildeParser = do
    _ <- char '~'
    opp <- operandParser
    return Tilde { operand = opp }

absoluteParser :: Parsec String () Operand
absoluteParser = do
    _ <- char '|'
    opp <- operandParser
    _ <- char '|'
    return Absolute { operand = opp }

textAddressParser :: Parsec String () Operand
textAddressParser = do
    _ <- string "`("
    a <- many1 (letter <|> digit <|> (char '_') <|> (char '.'))
    _ <- string ")"
    return TextAddress {label = a}

positiveInfinityParser :: Parsec String () Operand
positiveInfinityParser = do
    _ <- string "+INF"
    return PositiveInfinity

operandParser :: Parsec String () Operand
operandParser = choice 
    [ try registerParser
    , try predicateRegisterParser
    , try truePredicateParser
    , falsePredicateParser
    , zeroRegisterParser
    , specialRegisterParser
    , try uniformRegisterParser
    , try uniformZeroRegisterParser
    , try uniformTruePredicateParser
    , uniformFalsePredicateParser
    , barrierParser
    , try immediateParser
    , floatImmediateParser
    , constantMemoryParser
    , effectiveAddressParser
    , negativeParser
    , negateParser
    , absoluteParser
    , tildeParser
    , textAddressParser
    , positiveInfinityParser
    ]

instPredicateParser :: Parsec String () Predicate
instPredicateParser = do
    _ <- char '@'
    inv <- optionMaybe (char '!')
    _ <- char 'P'
    num <- many (digit)
    return Predicate { inverse = isJust inv, register = read num }

operandListParser :: Parsec String () [Operand]
operandListParser = sepBy1 operandParser (string ", ")

instructionParser :: Parsec String () Instruction
instructionParser = do
    pdc <- optionMaybe instPredicateParser
    _ <- many (char ' ')
    opp <- many1 (letter <|> digit <|> (char '.'))
    args <- (eof >> return []) <|> (char ' ' >> operandListParser)
    return (Instruction{predicate=pdc,op=opp, operands=args})
