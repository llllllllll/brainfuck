-- Joe Jevnik
-- 9.11.2013
-- Edited: 20.12.2013

module UnsafeBrainfuck ( main ) where

import Control.Applicative ((<$>))
import Data.Array.Base     (unsafeAt,unsafeWrite)
import Data.Array.Unboxed  (Array,UArray,(!),(//),listArray,bounds)
import Data.Array.IO       (IOUArray)
import Data.Array.MArray   (MArray)
import Data.Array.Unsafe   (unsafeThaw)
import Data.List           (groupBy)
import Data.Word           (Word8)
import System.Environment  (getArgs)
import System.IO           (isEOF)

-- -----------------------------------------------------------------------------
-- Data types.

-- | Represents the types of commands that can be read or performed.
data Command = PtrRight       -- ^ '>'
              | PtrLeft       -- ^ '<'
              | PtrJump Int   -- ^ repeated calls to ptr movements.
              | ValIncr       -- ^ '+'
              | ValDecr       -- ^ '-'
              | ValChange Int -- ^ repeted cals to val changes
              | ValSet Word8
              | ValPrnt       -- ^ '.'
              | ValInpt       -- ^ ','
              | BegLoop       -- ^ '['
              | EndLoop       -- ^ ']'
              | NulFunc       -- ^ Do nothing.
                deriving (Eq,Show)

-- | Data type to hold the state of the program.
data ProgState = ProgState { ptr        :: Int
                           , tape       :: UArray Int Word8
                           , currCmd    :: Int
                           , cmds       :: Array Int Command
                           , numCmds    :: Int
                           , loopStates :: [Int]
                           }

-- -----------------------------------------------------------------------------
-- File Parsing and reading.

main :: IO [String] -> IO ()
main as = as >>= parseArgs

-- | Parses the args and starts interpreting.
parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn "Usage: runbrainfuck [FILE]"
parseArgs as = do
    let b = "-O" `elem` as
    cs <- (if b
             then o2
             else id) . getCommands <$> readFile (if b
                                                    then (head . tail) as
                                                    else head as)
    let csl = length cs
    process ProgState { ptr        = 0
                      , tape       = listArray (0,29999) $ repeat 0
                      , currCmd    = 0
                      , cmds       = listArray (0,csl - 1) cs
                      , numCmds    = csl
                      , loopStates = []
                      }

-- | Reads a string into a list of 'Command's
getCommands :: String -> [Command]
getCommands [] = []
getCommands (c:cs)
          | c == '>'  = PtrRight : getCommands cs
          | c == '<'  = PtrLeft  : getCommands cs
          | c == '+'  = ValIncr  : getCommands cs
          | c == '-'  = ValDecr  : getCommands cs
          | c == '.'  = ValPrnt  : getCommands cs
          | c == ','  = ValInpt  : getCommands cs
          | c == '['  = BegLoop  : getCommands cs
          | c == ']'  = EndLoop  : getCommands cs
          | otherwise =            getCommands cs

-- | Converts a 'Command' into the proper function
parseCmd :: Command -> ProgState -> IO ProgState
parseCmd PtrRight      = return . ptrJump   1
parseCmd PtrLeft       = return . ptrJump (-1)
parseCmd (PtrJump n)   = return . ptrJump   n
parseCmd ValIncr       = valChange          1
parseCmd ValDecr       = valChange        (-1)
parseCmd (ValChange n) = valChange          n
parseCmd (ValSet n)    = valSet             n
parseCmd ValPrnt       = valPrnt
parseCmd ValInpt       = valInpt
parseCmd BegLoop       = return . begLoop
parseCmd EndLoop       = return . endLoop
parseCmd NulFunc       = return . nulFunc

-- | Applies the current command to the state.
applyCommand :: ProgState -> IO ProgState
applyCommand st = parseCmd (cmds st `unsafeAt` currCmd st) st

-- | Loops through, calling apply to each command in the array.
process :: ProgState -> IO ()
process st
    | currCmd st == numCmds st = return ()
    | otherwise                = applyCommand st >>= process

-- -----------------------------------------------------------------------------
-- Optimization.

-- | Collapses all chains of '+' and '-' into a single ValChange command.
collapseValChanges :: [Command] -> [Command]
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
collapsePtrJmps :: [Command] -> [Command]
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
o2 :: [Command] -> [Command]
o2 = collapsePtrJmps . collapseValChanges

-- -----------------------------------------------------------------------------
-- Commands.

-- | Moves the current array pointer to the right by n.
ptrJump :: Int -> ProgState -> ProgState
ptrJump n st = case ptr st + n of
                   m | m > 29999 -> error "Moved off the right side of the tape"
                     | m < 0     -> error "Moved off the left side of the tape"
                     | otherwise -> nulFunc st { ptr = m }

-- | Increments the value at the current pointer by n.
valChange :: Int -> ProgState -> IO ProgState
valChange n st = (unsafeThaw (tape st) :: IO (IOUArray Int Word8))
                 >>= \t -> unsafeWrite t (ptr st) (change n st)
                 >> return (nulFunc st)
  where
      change n st = fromIntegral $ n + fromIntegral (tape st `unsafeAt` ptr st)

-- | Assings the current value to n
valSet :: Word8 -> ProgState -> IO ProgState
valSet n st = (unsafeThaw (tape st) :: IO (IOUArray Int Word8))
              >>= \t -> unsafeWrite t (ptr st) n
              >> return (nulFunc st)

-- | Prints the Word8 at tape st ! ptr st.
valPrnt :: ProgState -> IO ProgState
valPrnt st = putChar ((toEnum . fromEnum) (tape st `unsafeAt` ptr st))
             >> return (nulFunc st)

-- | Reads a Word8 from the user to put at tape!ptr.
valInpt :: ProgState -> IO ProgState
valInpt st = isEOF >>= \b ->
             if not b
               then do
                   c <- getChar
                   t <- unsafeThaw (tape st) :: IO (IOUArray Int Word8)
                   unsafeWrite t (ptr st) ((toEnum . fromEnum) c)
                   return $ nulFunc st
               else return $ nulFunc st

-- | Begins a loop.
begLoop :: ProgState -> ProgState
begLoop st
    | tape st `unsafeAt` ptr st == 0
        = jmpLoop st
    | otherwise
        = nulFunc st { loopStates = currCmd st + 1 : loopStates st }

-- | Brings the program back to the top of a loop if the value at the
-- ptr is /= 0.
endLoop :: ProgState -> ProgState
endLoop st
    | tape st `unsafeAt` ptr st == 0
        = nulFunc st { loopStates = tail $ loopStates st }
    | otherwise
        = st { currCmd = head $ loopStates st }

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Converts a Char to a Word8
charToWord :: Char -> Word8
charToWord = toEnum . fromEnum

-- | Consumes one character but otherwise has no affect on the ProgState.
nulFunc :: ProgState -> ProgState
nulFunc st = st { currCmd = currCmd st + 1 }

-- | Jumps to the end of a loop without processing the contents.
-- Goes to the matching ']' for the given ']', does so by using n to count
-- nesting levels, where when it encounters a '[', it adds to the nesting level,
-- and when it encounters ']', it takes one away, it only terminates when it is
-- not nesting and it hits the ']'.
jmpLoop :: ProgState -> ProgState
jmpLoop st = jmpLoop' (cmds st) (currCmd st) st 0
  where
      jmpLoop' cs cc st n
          | cc + 1                           == numCmds st
              = error "Unmatched '['"
          | n == 0 && cs `unsafeAt` (cc + 1) == EndLoop
              = st { currCmd = cc + 2 }
          | cs `unsafeAt` (cc + 1)           == EndLoop
              = jmpLoop' cs (cc + 1) st (n - 1)
          | cs `unsafeAt` (cc + 1)           == BegLoop
              = jmpLoop' cs (cc + 1) st (n + 1)
          | otherwise
              = jmpLoop' cs (cc + 1) st n
