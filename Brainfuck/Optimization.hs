-- |
-- Module      : Brainfuck.Optimization
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Optimization of Brainfuck 'Instruction's.
module Brainfuck.Optimization
    ( collapseValChanges -- :: [Instruction] -> [Instruction]
    , collapsePtrJmps    -- :: [Instruction] -> [Instruction]
    , o2                 -- :: [Instruction] -> [Instruction]
    ) where

import Data.List (groupBy)

import Brainfuck.Lexer (Instruction(..))


-- | Collapses all chains of '+' and '-' into a single ValChange command.
collapseValChanges :: [Instruction] -> [Instruction]
collapseValChanges cs = let cs' = groupBy g cs
                        in cs' >>= \c -> case count c of
                                             (0,0)   -> c
                                             (ic,dc) -> [ValChange (ic - dc)]
  where
      g ValIncr ValIncr  = True
      g ValIncr ValDecr  = True
      g ValDecr ValIncr  = True
      g ValDecr ValDecr  = True
      g _ _              = False
      count []           = (0,0)
      count (ValIncr:cs) = (\(a,b) -> (a + 1,b)) $ count cs
      count (ValDecr:cs) = (\(a,b) -> (a,b + 1)) $ count cs
      count (c:cs)       = count cs


-- | Collapses all chains of '>' and '<' into one PtrJump command.
collapsePtrJmps :: [Instruction] -> [Instruction]
collapsePtrJmps cs = let cs' = groupBy g cs
                     in cs' >>= \c -> case count c of
                                          (0,0)   -> c
                                          (rc,lc) -> [PtrJump (rc - lc)]
  where
      g PtrRight PtrRight = True
      g PtrRight PtrLeft  = True
      g PtrLeft  PtrRight = True
      g PtrLeft  PtrLeft  = True
      g _ _               = False
      count []            = (0,0)
      count (PtrRight:cs) = (\(a,b) -> (a + 1,b)) $ count cs
      count (PtrLeft:cs)  = (\(a,b) -> (a,b + 1)) $ count cs
      count (c:cs)        = count cs


-- | Applies the full optimizations.
o2 :: [Instruction] -> [Instruction]
o2 = collapsePtrJmps . collapseValChanges
