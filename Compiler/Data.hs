-- |
-- Module      : Compiler.Data
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Data types used in the lexing and compiling process of 16candles source code.

module Compiler.Data
    ( Word8(..)
    , Word16(..)
    , Flag(..)
    , Token(..)           -- Instances: Show,Eq
    , Expression(..)      -- Instances: Show,Eq
    , ExpressionError(..) -- Instances: Show,Eq
    , JumpMiss(..)        -- Instances: Show
    , showJumpMiss        -- :: JumpMiss -> String
    , showExprError       -- ExpressionError -> String
    , Register(..)        -- Instances: Show,Eq
    , resolveReg          -- :: Register -> Word8
    , registerStrings     -- :: [String]
    , parseRegister       -- String -> Maybe Register
    , Instruction(..)     -- Instances: Show,Eq
    , resolveOpcode       -- :: Instruction -> Word8
    , resolveParam        -- :: Token -> [Word8]
    , instructionStrings  -- :: [(String,String)]
    , parseInstruction    -- :: String -> Maybe Instruction
    , instrOpPairs        -- :: [(Instruction,Word8)]
    , Suffix(..)          -- Instances: Eq
    , suffixToWord        -- :: Suffix -> Word8
    , suffixLength        -- :: Suffix -> Int
    , shortToCharList     -- :: Word16 -> [Word8]
    ) where

import Control.Monad (liftM)
import Data.Bits     ((.&.),shift)
import Data.List     (find,intercalate)
import Data.Maybe    (fromMaybe)
import Data.Word     (Word8,Word16)

-- | The command line flags
data Flag = Version             -- ^ Flag to print version info
          | Help                -- ^ Flag to print help info
          | OutputFile FilePath -- ^ Flag to change output file path

-- | The types of 'Token's that can be read.
data Token = InstrToken Instruction  -- ^ The type of an instruction token.
           | RegToken Register       -- ^ A register or subregister.
           | Literal Word16          -- ^ A 'Word16' literal.
           | MemoryAddress Word16    -- ^ A 'Memory Address' as a parameter.
           | MemoryRegister Register -- ^ A dereferenced 'Register'
           | Label String            -- ^ A new label and its name.
           | NewLine                 -- ^ A token showing a '\n'
           | OpenBrace  String       -- ^ A token showing '{'.
           | CloseBrace String       -- ^ A token showing '}'.
           | WhenToken               -- ^ A token for the 'when' command.
           | UnlessToken             -- ^ A token for the 'unless' command.
           | InvalidToken String     -- ^ Any invalid 'Token', with the string.
             deriving (Eq)

-- | Custom 'Show' instance for 'Token'.
instance Show Token where
    show (InstrToken i)     = show i
    show (RegToken r)       = show r
    show (Literal l)        = show l
    show (MemoryAddress a)  = '*' : show a
    show (MemoryRegister r) = '*' : show r
    show (Label l)          = '@' : l
    show NewLine            = "\\n"
    show (OpenBrace n)      = "{ " ++ n
    show (CloseBrace n)     = "} " ++ n
    show WhenToken          = "when"
    show UnlessToken        = "unless"
    show (InvalidToken t)   = "invalid: " ++ t

-- | An expression.
type Expression = [Token]

-- | An error that plagues an invalid expression.
data ExpressionError = MissingParameters
                     | TooManyParameters [Token]
                     | ExpectedRegister Token
                     | ExpectedRegOrLit Token
                     | ExpectedMemory Token
                     | MismatchedParameters
                     | UnknownInstruction String
                     | MissingInstruction
                     | ExpectedOpeningBrace
                       deriving (Show,Eq)

-- | A jump command that does not have a label associated with it.
data JumpMiss = JumpMiss String Int deriving (Show)

-- | Pretty outputs the 'JumpMiss' error.
showJumpMiss :: JumpMiss -> String
showJumpMiss (JumpMiss label line) = "Could not resolve label '" ++ label ++ "'"

-- | Shows an 'ExpressionError' in a way suitable to be printed during
-- compilation.
showExprError :: ExpressionError -> String
showExprError MissingParameters      =
    "Missing paramaters"
showExprError (TooManyParameters ts) =
    "Too many paramaters (" ++ (intercalate "," . map show) ts ++ ")"
showExprError (ExpectedRegister t) =
    "Expected register paramater instead of '" ++ show t ++ "'"
showExprError (ExpectedRegOrLit t) =
    "Expected register or literal paramater instead of '" ++ show t ++ "'"
showExprError (ExpectedMemory t) =
    "Expected memory address instead of '" ++ show t ++ "'"
showExprError MismatchedParameters =
    "Invalid paramaters for the given operation"
showExprError (UnknownInstruction cs) =
    "Unknown instruction '" ++ cs ++ "'"
showExprError MissingInstruction =
    "No instruction given"

-- | The different names for the 'Register's.
data Register = Ipt   -- ^ The instruction pointer.
              | Spt   -- ^ The stack pointer.
              | Ac1   -- ^ Accumulator 1.
              | Ac2   -- ^ Accumulator 2.
              | Tst   -- ^ The testing register.
              | Inp   -- ^ Standard in buffer pointers.
              | Inp_r -- ^ Standard in buffer read.
              | Inp_w -- ^ Standard in buffer write.
              | R0    -- ^ Free register 0.
              | R0_f
              | R0_b
              | R1   -- ^ Free register 1.
              | R1_f
              | R1_b
              | R2   -- ^ Free register 2.
              | R2_f
              | R2_b
              | R3   -- ^ Free register 3.
              | R3_f
              | R3_b
              | R4   -- ^ Free register 4.
              | R4_f
              | R4_b
              | R5   -- ^ Free register 5.
              | R5_f
              | R5_b
              | R6   -- ^ Free register 6.
              | R6_f
              | R6_b
              | R7   -- ^ Free register 7.
              | R7_f
              | R7_b
              | R8   -- ^ Free register 8.
              | R8_f
              | R8_b
              | R9   -- ^ Free register 9.
              | R9_f
              | R9_b
                deriving (Eq)

-- | Custom 'Show' instance for 'Register'.
instance Show Register where
    show r = fromMaybe (error "show :: Register -> String: "
                        ++ "could not resolve the register")
             $ liftM snd $ find ((==) r . fst) $ zip regs registerStrings
      where
          regs = [ Ipt,Spt,Ac1,Tst,Inp,Ac2,R0,R1,R2,R3,R4,R5,R6,R7,R8,R9
                 , Inp_r,Inp_w,R0_f,R0_b,R1_f,R1_b,R2_f,R2_b,R3_f,R3_b
                 , R4_f,R4_b,R5_f,R5_b,R6_f,R6_b,R7_f,R7_b,R8_f,R8_b,R9_f,R9_b ]

-- | Resolves a 'Register' into the bytecode representation.
resolveReg :: Register -> Word8
resolveReg Ipt   = 0x00
resolveReg Spt   = 0x01
resolveReg Ac1   = 0x02
resolveReg Ac2   = 0x03
resolveReg Tst   = 0x04
resolveReg Inp   = 0x05
resolveReg R0    = 0x06
resolveReg R1    = 0x07
resolveReg R2    = 0x08
resolveReg R3    = 0x09
resolveReg R4    = 0x0a
resolveReg R5    = 0x0b
resolveReg R6    = 0x0c
resolveReg R7    = 0x0d
resolveReg R8    = 0x0e
resolveReg R9    = 0x0f
resolveReg Inp_r = 0x10
resolveReg Inp_w = 0x11
resolveReg R0_f  = 0x12
resolveReg R0_b  = 0x13
resolveReg R1_f  = 0x14
resolveReg R1_b  = 0x15
resolveReg R2_f  = 0x16
resolveReg R2_b  = 0x17
resolveReg R3_f  = 0x18
resolveReg R3_b  = 0x19
resolveReg R4_f  = 0x1a
resolveReg R4_b  = 0x1b
resolveReg R5_f  = 0x1c
resolveReg R5_b  = 0x1d
resolveReg R6_f  = 0x1e
resolveReg R6_b  = 0x1f
resolveReg R7_f  = 0x20
resolveReg R7_b  = 0x21
resolveReg R8_f  = 0x22
resolveReg R8_b  = 0x23
resolveReg R9_f  = 0x24
resolveReg R9_b  = 0x25

-- | A list that contains all possible ways a register may appear in the source
-- code.
registerStrings :: [String]
registerStrings = [ "ipt"
                  , "spt"
                  , "ac1"
                  , "tst"
                  , "inp"
                  , "ac2"
                  , "r0"
                  , "r1"
                  , "r2"
                  , "r3"
                  , "r4"
                  , "r5"
                  , "r6"
                  , "r7"
                  , "r8"
                  , "r9"
                  , "inp_r"
                  , "inp_w"
                  , "r0_f"
                  , "r0_b"
                  , "r1_f"
                  , "r1_b"
                  , "r2_f"
                  , "r2_b"
                  , "r3_f"
                  , "r3_b"
                  , "r4_f"
                  , "r4_b"
                  , "r5_f"
                  , "r5_b"
                  , "r6_f"
                  , "r6_b"
                  , "r7_f"
                  , "r7_b"
                  , "r8_f"
                  , "r8_b"
                  , "r9_f"
                  , "r9_b" ]

-- | Parses the 'Register' type out of a 'String'.
parseRegister :: String -> Maybe Register
parseRegister cs = liftM fst $ find ((==) cs . snd) $ zip regs registerStrings
  where
      regs = [ Ipt,Spt,Ac1,Tst,Inp,Ac2,R0,R1,R2,R3,R4,R5,R6,R7,R8,R9
             , Inp_r,Inp_w,R0_f,R0_b,R1_f,R1_b,R2_f,R2_b,R3_f,R3_b
             , R4_f,R4_b,R5_f,R5_b,R6_f,R6_b,R7_f,R7_b,R8_f,R8_b,R9_f,R9_b ]

-- | An instruction.
data Instruction = OpAnd
                 | OpOr
                 | OpXand
                 | OpXor
                 | OpLshift
                 | OpRshift
                 | OpAdd
                 | OpSub
                 | OpMul
                 | OpDiv
                 | OpMod
                 | OpMin
                 | OpMax
                 | OpGte
                 | OpLte
                 | OpEq
                 | OpNeq
                 | OpGt
                 | OpLt
                 | OpInv
                 | OpInc
                 | OpDec
                 | OpSet
                 | OpPush
                 | OpJmp
                 | OpJmpt
                 | OpJmpf
                 | OpWrite
                 | OpMset
                 | OpSwap
                 | OpPop
                 | OpPeek
                 | OpFlush
                 | OpHalt
                 | OpRead
                 | OpNop
                 | OpTerm
                   deriving (Eq)

-- | Custom 'Show' instance for 'Instruction'.
instance Show Instruction where
    show i        = fromMaybe (error "show :: Instruction -> String: "
                               ++ "could not resolve the instruction")
                    $ liftM ((\(a,b) -> if null b
                                          then a
                                          else '(':a ++ ',':b ++ ")") . snd)
                    $ find ((==) i . fst)
                    $ zip instrs instructionStrings
      where
          instrs = [ OpAnd,OpOr,OpXand,OpXor,OpInv,OpLshift,OpRshift,OpAdd,OpSub
                   , OpMul,OpDiv,OpMod,OpInc,OpDec,OpGt,OpLt,OpGte,OpLte,OpEq
                   , OpNeq,OpMin,OpMax,OpJmp,OpJmpt,OpJmpf,OpPush,OpPop,OpPeek
                   , OpFlush,OpSet,OpMset,OpSwap,OpHalt,OpNop,OpRead,OpWrite
                   , OpTerm ]

-- | Resolves and 'Instruction' into it's bytecode representation.
resolveOpcode :: Instruction -> Word8
resolveOpcode i = snd $ head $ dropWhile ((/=) i . fst) instrOpPairs

-- | Resolves a paramater into it's bytecode representation.
resolveParam :: Token -> [Word8]
resolveParam (Literal n)        = shortToCharList n
resolveParam (MemoryAddress n)  = shortToCharList n
resolveParam (RegToken n)       = [resolveReg n]
resolveParam (MemoryRegister n) = [resolveReg n]
resolveParam _                  = []

-- | The list of 'Instruction's as a pair of the name and operator.
instructionStrings :: [(String,String)]
instructionStrings = [ ("and",   "&&"  )
                     , ("or",    "||"  )
                     , ("xand",  "!&"  )
                     , ("xor",   "!|"  )
                     , ("inv",   "~"   )
                     , ("lshift","<<"  )
                     , ("rshift",">>"  )
                     , ("add",   "+"   )
                     , ("sub",   "-"   )
                     , ("mul",   "*"   )
                     , ("div",   "/"   )
                     , ("mod",   "%"   )
                     , ("inc",   "++"  )
                     , ("dec",   "--"  )
                     , ("gt",    ">"   )
                     , ("lt",    "<"   )
                     , ("gte",   ">="  )
                     , ("lte",   "<="  )
                     , ("eq",    "=="  )
                     , ("neq",   "!="  )
                     , ("min",   ""    )
                     , ("max",   ""    )
                     , ("jmp",   "=>"  )
                     , ("jmpt",  "->"  )
                     , ("jmpf",  "<-"  )
                     , ("push",  ":"   )
                     , ("pop",   "$"   )
                     , ("peek",  "@"   )
                     , ("flush", "#"   )
                     , ("set",   "="   )
                     , ("mset",  ":="  )
                     , ("swap" , "\\\\")
                     , ("halt",  ""    )
                     , ("nop",   ""    )
                     , ("read",  ""    )
                     , ("write", ""    )
                     , ("term",  ""    )  ]

-- | Parses an 'Instruction' out of a 'String'
parseInstruction :: String -> Maybe Instruction
parseInstruction "" = Nothing
parseInstruction cs = liftM fst $ find (\(_,(a,b)) -> cs `elem` [a,b])
                       $ zip instrs instructionStrings
  where
      instrs = [ OpAnd,OpOr,OpXand,OpXor,OpInv,OpLshift,OpRshift,OpAdd,OpSub
               , OpMul,OpDiv,OpMod,OpInc,OpDec,OpGt,OpLt,OpGte,OpLte,OpEq,OpNeq
               , OpMin,OpMax,OpJmp,OpJmpt,OpJmpf,OpPush,OpPop,OpPeek,OpFlush
               , OpSet,OpMset,OpSwap,OpHalt,OpNop,OpRead,OpWrite,OpTerm
               ]

-- | 'Instruction's paired with their pre-suffix opcode.
instrOpPairs :: [(Instruction,Word8)]
instrOpPairs = [ (OpAnd,    0x00)
               , (OpOr,     0x04)
               , (OpXand,   0x08)
               , (OpXor,    0x0c)
               , (OpLshift, 0x10)
               , (OpRshift, 0x14)
               , (OpAdd,    0x18)
               , (OpSub,    0x1c)
               , (OpMul,    0x20)
               , (OpDiv,    0x24)
               , (OpMod,    0x28)
               , (OpMin,    0x2c)
               , (OpMax,    0x30)
               , (OpGte,    0x34)
               , (OpLte,    0x38)
               , (OpEq,     0x3c)
               , (OpNeq,    0x40)
               , (OpGt,     0x44)
               , (OpLt,     0x48)
               , (OpInv,    0x4c)
               , (OpInc,    0x4e)
               , (OpDec,    0x50)
               , (OpSet,    0x52)
               , (OpPush,   0x54)
               , (OpJmp,    0x56)
               , (OpJmpt,   0x58)
               , (OpJmpf,   0x5a)
               , (OpWrite,  0x5c)
               , (OpMset,   0x5e)
               , (OpSwap,   0x64)
               , (OpPop,    0x65)
               , (OpPeek,   0x66)
               , (OpFlush,  0x67)
               , (OpHalt,   0x68)
               , (OpRead,   0x69)
               , (OpNop,    0x6a)
               , (OpTerm,   0xff) ]

-- | A bytecode operator suffix.
data Suffix = SuffixLitLit
            | SuffixLitReg
            | SuffixRegLit
            | SuffixRegReg
            | SuffixLit
            | SuffixReg
            | SuffixLitMemaddr
            | SuffixRegMemaddr
            | SuffixLitMemreg
            | SuffixRegMemreg
            | SuffixMemaddr
            | SuffixMemreg
            | NoSuffix
              deriving (Eq)

-- | Converts a suffix to the actual word representation.
suffixToWord :: Suffix -> Word8
suffixToWord SuffixLitLit     = 0x00
suffixToWord SuffixLitReg     = 0x01
suffixToWord SuffixRegLit     = 0x02
suffixToWord SuffixRegReg     = 0x03
suffixToWord SuffixLit        = 0x00
suffixToWord SuffixReg        = 0x01
suffixToWord SuffixLitMemaddr = 0x00
suffixToWord SuffixRegMemaddr = 0x01
suffixToWord SuffixLitMemreg  = 0x02
suffixToWord SuffixRegMemreg  = 0x03
suffixToWord SuffixMemaddr    = 0x04
suffixToWord SuffixMemreg     = 0x05
suffixToWord NoSuffix         = 0x00

-- | The length a given suffix adds to a command.
suffixLength :: Suffix -> Int
suffixLength SuffixLitLit     = 4
suffixLength SuffixLitReg     = 3
suffixLength SuffixRegLit     = 3
suffixLength SuffixRegReg     = 2
suffixLength SuffixLit        = 2
suffixLength SuffixReg        = 1
suffixLength SuffixLitMemaddr = 4
suffixLength SuffixRegMemaddr = 3
suffixLength SuffixLitMemreg  = 3
suffixLength SuffixRegMemreg  = 2
suffixLength SuffixMemaddr    = 2
suffixLength SuffixMemreg     = 1
suffixLength NoSuffix         = 0

-- | Cast a 'Word16' to a list of 'Word8's
shortToCharList :: Word16 -> [Word8]
shortToCharList n  = [ fromIntegral $ (n .&. 0xff00) `shift` (-8)
                     , fromIntegral $ n .&. 0x00ff ]
