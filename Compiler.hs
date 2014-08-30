-- |
-- Module      : Compiler
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Compiler that targers 16 candles asm.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (liftM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B ( pack,cons,append,unpack
                                            , intercalate,readFile,writeFile)
import Data.List (find,intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import System.Environment (getArgs)

import Brainfuck.Lexer (getInstructions,Instruction(..))
import Brainfuck.Optimization (o2)

import qualified Compiler.Data as C


-- | Parameters to instructions.
data C16Param = RegParam C.Register
              | MemRegParam C.Register
              | LiteralParam Word16
              | Label ByteString


instance Show C16Param where
    show (RegParam r)     = show r
    show (MemRegParam r)  = '*' : show r
    show (LiteralParam w) = show w
    show (Label b)        = B.unpack b


-- | A complete statement.
data C16Statement = C16NoParams C.Instruction
                  | C16WithParams C.Instruction [C16Param]
                  | NewLabel ByteString


-- | A pre-processed 'Instruction' to resolve which braces match.
data PPInstruction = Stmt Instruction
                   | Loop Instruction Int


-- | The register where we are storing the tape pointer.
tapePtr :: C.Register
tapePtr = C.R0


-- | The register wher we are storing values to do operations on them.
valReg :: C.Register
valReg = C.R1


-- | Loads the value stored at the tapePtr into the valReg
loadValAtPtr :: C16Statement
loadValAtPtr = C16WithParams C.OpMset  [MemRegParam tapePtr,RegParam valReg]


-- | Loads the value stored at the tapePtr into the tst register.
loadValAtPtrIntoTst :: C16Statement
loadValAtPtrIntoTst = C16WithParams C.OpMset
                      [MemRegParam tapePtr,RegParam C.Tst]


-- | Writes the value stored in the valReg to the tape at tapePtr.
writeValToPtr :: C16Statement
writeValToPtr = C16WithParams C.OpMset [RegParam valReg,MemRegParam tapePtr]


-- | Creates a label to begin a loop.
begLabel :: Int -> ByteString
begLabel = B.append "C16_INTERNAL_BEG_LOOP_" . B.pack . show


-- | Creates a label to end a loop.
endLabel :: Int -> ByteString
endLabel = B.append "C16_INTERNAL_END_LOOP_" . B.pack . show


-- | Resolve which braces are matched.
numberLoopLevels :: [Instruction] -> [PPInstruction]
numberLoopLevels is = numberLoopLevels' is 0 []
  where
      numberLoopLevels' [] _ _ = []
      numberLoopLevels' (BegLoop:is) n cs =
          Loop BegLoop n : numberLoopLevels' is (n + 1) (n:cs)
      numberLoopLevels' (EndLoop:is) n (c:cs) =
          Loop EndLoop c : numberLoopLevels' is n cs
      numberLoopLevels' (i:is) n cs = (Stmt i) : numberLoopLevels' is n cs


-- | Converts a Brainfuck 'Instruction' into a list of 'C16Statement's
instrToStatements :: PPInstruction -> [C16Statement]
instrToStatements (Stmt PtrRight)      = [C16WithParams
                                          C.OpInc [ RegParam tapePtr
                                                  , RegParam tapePtr
                                                  ]]
instrToStatements (Stmt PtrLeft)       = [C16WithParams
                                          C.OpDec [ RegParam tapePtr
                                                  , RegParam tapePtr
                                                  ]]
instrToStatements (Stmt (PtrJump n))   = [C16WithParams
                                          C.OpAdd [ RegParam tapePtr
                                                  , LiteralParam
                                                  $ fromIntegral n
                                                  , RegParam tapePtr
                                                  ]]
instrToStatements (Stmt ValIncr)       = [ loadValAtPtr
                                         , C16WithParams
                                           C.OpInc [ RegParam valReg
                                                   , RegParam valReg
                                                   ]
                                         , writeValToPtr
                                         ]
instrToStatements (Stmt ValDecr)       = [ loadValAtPtr
                                         , C16WithParams
                                           C.OpDec [ RegParam valReg
                                                   , RegParam valReg
                                                   ]
                                         , writeValToPtr
                                         ]
instrToStatements (Stmt (ValChange n)) = [ loadValAtPtr
                                         , C16WithParams
                                           (if n > 0
                                              then C.OpAdd
                                              else C.OpSub)
                                           [ RegParam valReg
                                           , LiteralParam
                                           $ fromIntegral . abs $ n
                                           , RegParam valReg
                                           ]
                                         , writeValToPtr
            ]
instrToStatements (Stmt (ValSet n))    = [C16WithParams
                                          C.OpMset [ LiteralParam
                                                     (fromIntegral n)
                                                   , MemRegParam tapePtr
                                                   ]]
instrToStatements (Stmt ValPrnt)       = [ loadValAtPtr
                                         , C16WithParams
                                           C.OpWrite [RegParam valReg]
                                         ]
instrToStatements (Stmt ValInpt)       = [ C16WithParams
                                           C.OpRead [RegParam valReg]
                                         , writeValToPtr
                                         ]
instrToStatements (Loop BegLoop n)     = [ loadValAtPtrIntoTst
                                         , C16WithParams
                                           C.OpJmpf [Label $ endLabel n]
                                         , NewLabel (begLabel n)
                                         ]
instrToStatements (Loop EndLoop n)     = [ loadValAtPtrIntoTst
                                         , C16WithParams
                                           C.OpJmpt [Label $ begLabel n]
                                         , NewLabel (endLabel n)
                                         ]
instrToStatements (Stmt NulFunc)       = []


showCInstruction :: C.Instruction -> String
showCInstruction i = fst $ fromMaybe (error "show :: Instruction -> String: \
                                            \could not resolve the instruction")
                     $ liftM snd $ find ((==) i . fst)
                     $ zip instrs C.instructionStrings
  where
      instrs = [ C.OpAnd,C.OpOr,C.OpXand,C.OpXor,C.OpInv,C.OpLshift,C.OpRshift
               , C.OpAdd,C.OpSub,C.OpMul,C.OpDiv,C.OpMod,C.OpInc,C.OpDec,C.OpGt
               , C.OpLt,C.OpGte,C.OpLte,C.OpEq,C.OpNeq,C.OpMin,C.OpMax,C.OpJmp
               , C.OpJmpt,C.OpJmpf,C.OpPush,C.OpPop,C.OpPeek,C.OpFlush,C.OpSet
               , C.OpMset,C.OpSwap,C.OpHalt,C.OpNop,C.OpRead,C.OpWrite
               , C.OpTerm ]


-- | Converts a C16Statement to the corrosponding c16 ASM.
stmtToASM :: C16Statement -> ByteString
stmtToASM (NewLabel l)         = B.cons '@' l
stmtToASM (C16NoParams i)      = B.pack $ show i
stmtToASM (C16WithParams i ps) = B.pack $ intercalate " "
                                 $ showCInstruction i : map show ps


-- | Compiles Brainfuck source.
compile :: ByteString -> ByteString
compile = B.append header
          . B.intercalate "\n"
          . concat
          . map (map stmtToASM . instrToStatements)
          . numberLoopLevels
          . o2
          . getInstructions
  where
      header = B.pack
               $ "set spt " ++ show tapePtr ++ "\n"


main :: IO ()
main = getArgs >>= parseArgs


-- | Parses the args and starts interpreting.
parseArgs :: [String] -> IO ()
parseArgs []    = putStrLn "Usage: b16cc FILE"
parseArgs (a:_) = B.readFile a
                  >>= B.writeFile "a.c16" . compile
