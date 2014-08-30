-- |
-- Module      : Brainfuck
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Interpreter for Brainfuck.
{-# LANGUAGE TupleSections, LambdaCase #-}
module Brainfuck where

import Control.Exception (try,SomeException)
import Control.Monad (void)
import Control.Monad.ST (ST,stToIO,RealWorld)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.IO.Class (liftIO)
import Data.Array.Base (unsafeAt,unsafeWrite,unsafeRead)
import Data.Array.Unboxed (Array,UArray,listArray,bounds)
import Data.Array.ST (STUArray)
import Data.Array.MArray (MArray,newArray)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (readFile,head,tail,foldr)
import Data.STRef (STRef,newSTRef,readSTRef,writeSTRef,modifySTRef',modifySTRef)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (isEOF)


import Brainfuck.Lexer (Instruction(..),getInstructions)
import Brainfuck.Optimization (o2)


-- -----------------------------------------------------------------------------
-- Data types.


-- | Data type to hold the state of the program.
data ProgState s i e = ProgState { ptr        :: STRef s Int
                                 , tape       :: STUArray s i e
                                 , currCmd    :: STRef s Int
                                 , cmds       :: Array Int Command
                                 , numCmds    :: Int
                                 , loopStates :: STRef s [Int]
                                 }

type Brainfuck s = ProgState s Int Word8


type Operation = Brainfuck RealWorld -> ST RealWorld ()


data Command = Command { apply :: Operation
                       , instr :: Instruction
                       }


-- -----------------------------------------------------------------------------
-- File Parsing and reading.

main :: IO ()
main = getArgs >>= parseArgs


-- | Parses the args and starts interpreting.
parseArgs :: [String] -> IO ()
parseArgs []    = putStrLn "Usage: runbrainfuck FILE"
parseArgs (a:_) = B.readFile a
                  >>= \cs -> let cs' = map parseCmd . o2 . getInstructions $ cs
                                 nc  = length cs'
                             in runBrainfuck nc $ listArray (0,nc) cs'




-- | Converts a 'Instruction' into the proper function
parseCmd :: Instruction -> Command
parseCmd PtrRight      = Command (ptrJump     1)   PtrRight
parseCmd PtrLeft       = Command (ptrJump   (-1))  PtrLeft
parseCmd (PtrJump n)   = Command (ptrJump     n)  (PtrJump n)
parseCmd ValIncr       = Command (valChange   1)   ValIncr
parseCmd ValDecr       = Command (valChange (-1))  ValDecr
parseCmd (ValChange n) = Command (valChange   n)  (ValChange n)
parseCmd (ValSet n)    = Command (valSet      n)  (ValSet n)
parseCmd ValPrnt       = Command valPrnt           ValPrnt
parseCmd ValInpt       = Command valInpt           ValInpt
parseCmd BegLoop       = Command begLoop           BegLoop
parseCmd EndLoop       = Command endLoop           EndLoop
parseCmd NulFunc       = Command incrCmd           NulFunc


-- | Loops through, calling apply to each command in the array.
runBrainfuck :: Int -> Array Int Command -> IO ()
runBrainfuck nc cs = void
                     (try (stToIO
                           $ newArray ((0,29999) :: (Int,Int)) (0 :: Word8)
                                 >>= \tp -> newSTRef 0
                                 >>= \cc -> newSTRef 0
                                 >>= \ptr -> newSTRef []
                                 >>= \ls -> processInstructions
                                            (ProgState ptr tp cc cs nc ls))
                      :: IO (Either SomeException ()))


processInstructions :: Operation
processInstructions st =
    readSTRef (currCmd st) >>= \cc -> apply (cmds st `unsafeAt` cc) st
                                      >> processInstructions st


-- -----------------------------------------------------------------------------
-- Operations.


-- | Moves the current array pointer to the right by n.
ptrJump :: Int -> Operation
ptrJump n st = let sptr = ptr st
               in readSTRef sptr >>= \p -> incrCmd st >> writeSTRef sptr (p + n)



-- | Increments the value at the current pointer by n.
valChange :: Int -> Operation
valChange n st = incrCmd st
                 >> readSTRef (ptr st)
                 >>= \sptr -> unsafeRead (tape st) sptr
                 >>= \v -> unsafeWrite (tape st) sptr (v + fromIntegral n)


-- | Assings the current ptr index to n
valSet :: Word8 -> Operation
valSet n st = incrCmd st
              >> readSTRef (ptr st)
              >>= \sptr -> unsafeWrite (tape st) sptr n



-- | Prints the Word8 at the current ptr index.
valPrnt :: Operation
valPrnt st = incrCmd st
             >> readSTRef (ptr st)
             >>= \sptr -> unsafeRead (tape st) sptr
             >>= \c -> unsafeIOToST $ putChar $ (toEnum . fromEnum) c


-- | Reads a Word8 from the user to put at tape!ptr.
valInpt :: Operation
valInpt st = incrCmd st
             >> readSTRef (ptr st)
             >>= \sptr -> unsafeIOToST isEOF >>= \case
                          True -> unsafeIOToST getChar
                                  >>= \c -> unsafeWrite (tape st) sptr
                                            (toEnum . fromEnum $ c)
                          _    -> return ()


-- | Begins a loop.
begLoop :: Operation
begLoop st = readSTRef (ptr st)
             >>= \sptr -> unsafeRead (tape st) sptr
             >>= \case
                 0 -> jmpLoop st
                 _ -> readSTRef (currCmd st)
                      >>= \cmd -> modifySTRef' (loopStates st) ((cmd  + 1) :)
                      >> incrCmd st


-- | Brings the program back to the top of a loop if the value at the
-- ptr is /= 0.
endLoop :: Operation
endLoop st = readSTRef (ptr st)
             >>= \sptr -> unsafeRead (tape st) sptr
             >>= \case
                 0 -> modifySTRef' (loopStates st) tail
                      >> incrCmd st
                 _ -> readSTRef (loopStates st)
                      >>= \(s:_) -> writeSTRef (currCmd st) s


incrCmd :: Operation
incrCmd st = modifySTRef (currCmd st) (+ 1)

-- -----------------------------------------------------------------------------
-- Helper functions


-- | Jumps to the end of a loop without processing the contents.
-- Goes to the matching ']' for the given ']', does so by using n to count
-- nesting levels, where when it encounters a '[', it adds to the nesting level,
-- and when it encounters ']', it takes one away, it only terminates when it is
-- not nesting and it hits the ']'.
jmpLoop :: Operation
jmpLoop st = readSTRef (currCmd st)
             >>= \cmd -> jmpLoop' (cmds st) cmd (numCmds st) (currCmd st) 0
  where
      jmpLoop' cs cc nc cmd n
          | cc + 1 == nc
              = error "Unmatched '['"
          | n == 0 && instr (cs `unsafeAt` (cc + 1)) == EndLoop
              = writeSTRef cmd (cc + 2)
          | instr (cs `unsafeAt` (cc + 1)) == EndLoop
              = jmpLoop' cs (cc + 1) nc cmd (n - 1)
          | instr (cs `unsafeAt` (cc + 1)) == BegLoop
              = jmpLoop' cs (cc + 1) nc cmd (n + 1)
          | otherwise
            = jmpLoop' cs (cc + 1) nc cmd n
