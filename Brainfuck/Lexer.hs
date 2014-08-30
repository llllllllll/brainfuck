-- |
-- Module      : Brainfuck.Lexer
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Lexical analysis of Brainfuck source code.
module Brainfuck.Lexer
    ( getInstructions  -- :: ByteString -> [Instruction]
    , Instruction(..)  -- instances: Eq,Show
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (foldr)
import Data.Word (Word8)


-- | Represents the types of instructions that can be read or performed.
data Instruction = PtrRight       -- ^ '>'
                 | PtrLeft       -- ^ '<'
                 | PtrJump Int   -- ^ Repeated calls to ptr movements.
                 | ValIncr       -- ^ '+'
                 | ValDecr       -- ^ '-'
                 | ValChange Int -- ^ Repeated calls to val changes
                 | ValSet Word8  -- ^ Directly sets the pointer to a value.
                 | ValPrnt       -- ^ '.'
                 | ValInpt       -- ^ ','
                 | BegLoop       -- ^ '['
                 | EndLoop       -- ^ ']'
                 | NulFunc       -- ^ Do nothing.
                   deriving (Eq,Show,Ord)


-- | Reads a ByteString into a list of 'Instruction's
getInstructions :: ByteString -> [Instruction]
getInstructions bs = B.foldr (\b cs -> case getInstruction b of
                                    Just c  -> c : cs
                                    Nothing -> cs) [] bs
  where
      getInstruction '>' = Just PtrRight
      getInstruction '<' = Just PtrLeft
      getInstruction '+' = Just ValIncr
      getInstruction '-' = Just ValDecr
      getInstruction '.' = Just ValPrnt
      getInstruction ',' = Just ValInpt
      getInstruction '[' = Just BegLoop
      getInstruction ']' = Just EndLoop
      getInstruction _   = Nothing
