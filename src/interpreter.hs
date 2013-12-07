-- Joe Jevnik
-- 9.11.2013
-- Edited: 13.10.2013

import Control.Applicative ((<$>))
import Control.Monad      (void)
import Data.Array.Unboxed (UArray,(!),(//),listArray)
import Data.Word          (Word32)
import System.Environment (getArgs)
import Text.Read          (readMaybe)

-- Data type to hold the state of the program.
data ProgState = ProgState { ptr        :: !Int              -- Value Pointer
                           , vals       :: UArray Int Word32  -- Values
                           , state      :: (String,String)   -- (consumed,left)
                           , loopStates :: [(String,String)] -- (consumed,left)
                           }

-- |Starts consuming characters from the ProgState file.
main :: IO ()
main = void $ getArgs >>= parseArgs >>= consumeChars

-- Parses the command line arguments to interperet the flags.
parseArgs :: [String]-> IO ProgState
parseArgs as =
    if head as == "-m"
      then do
          fl <- readFile $ as!!2
          let ln=  case readMaybe $ as!!1 of
                       Nothing -> error "'-m' expects and Int argument"
                       Just n  -> n - 1
          return ProgState { ptr        = 0
                           , vals       = listArray (0,ln) (repeat 0)
                           , state      = ("",fl)
                           , loopStates = []
                           }
      else do
          fl <- readFile $ head as
          return ProgState { ptr        = 0
                           , vals       = listArray (0,29999) (repeat 0)
                           , state      = ("",fl)
                           , loopStates = []
                           }

-- |Parses the next character from the ProgState.
consumeChars :: ProgState -> IO ProgState
consumeChars st =
    if (not $ null ((snd . state) st))
      then parseChar ((toEnum . fromEnum) $ (head . snd . state) st) st
               >>= consumeChars
      else return (ProgState 0 (listArray (0,29999) (repeat 0)) ("","") [])

-- |Parses a character into the proper function.
parseChar :: Word32 -> ProgState -> IO ProgState
parseChar c st
    | c == charToWord '>' = return $ ptrRight st
    | c == charToWord '<' = return $ ptrLeft st
    | c == charToWord '+' = return $ valIncr st
    | c == charToWord '-' = return $ valDecr st
    | c == charToWord '.' = valPrnt st
    | c == charToWord ',' = valInpt st
    | c == charToWord '[' = if (vals st)!(ptr st) /= 0
                              then return $ begLoop st
                              else return $ jmpLoop st
    | c == charToWord ']' = return $ endLoop st
    | otherwise = return $ nulFunc st


-- | Moves the current array pointer right by 1 index.
ptrRight :: ProgState -> ProgState
ptrRight st = st { ptr = ptr st + 1
                 , state = consumeChar st
                 }

-- | Moves the current array pointer left by 1 index.
ptrLeft :: ProgState -> ProgState
ptrLeft st = st { ptr = ptr st - 1
                , state = consumeChar st
                }

-- | Increments the value at the current pointer by one.
valIncr :: ProgState -> ProgState
valIncr st = st { vals = vals st//[(ptr st,succ $ (vals st)!(ptr st))]
                , state = consumeChar st
                }

-- | Decrements the value at the current pointer by one.
valDecr :: ProgState -> ProgState
valDecr st = st { vals = vals st//[(ptr st,pred $ (vals st)!(ptr st))]
                , state = consumeChar st
                }

-- | Prints the Word32 at vals!ptr.
valPrnt :: ProgState -> IO ProgState
valPrnt st = putChar ((toEnum . fromEnum) ((vals st)!(ptr st))) >> return
              st { state = consumeChar st }

-- | Reads a Word32 from the user to put at vals!ptr.
valInpt :: ProgState -> IO ProgState
valInpt st = getChar >>= \c
            -> return st { vals = vals st//[(ptr st,(toEnum . fromEnum) c)]
                         , state = consumeChar st
                         }

-- | Consumes one character but otherwise has no affect on the ProgState.
nulFunc :: ProgState -> ProgState
nulFunc st = st { state = consumeChar st }

-- | Begins a loop.
begLoop :: ProgState -> ProgState
begLoop st = if (vals st)!(ptr st) == 0
                then jmpLoop st
                else st { state    = consumeChar st
                        , loopStates = consumeChar st:loopStates st
                        }

-- | Brings the program back to the top of a loop if the value at the
-- ptr is /= 0.
endLoop :: ProgState -> ProgState
endLoop st = if (vals st)!(ptr st) == 0
                then st { state    = consumeChar st
                        , loopStates = tail $ loopStates st
                        }
                else st { state    = head $ loopStates st }

-- | Jumps to the end of a loop without processing the contents.
jmpLoop :: ProgState -> ProgState
jmpLoop st = if (head . snd . state) st == (toEnum . fromEnum) ']'
                then endLoop st
                else jmpLoop $ nulFunc st

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Moves one char from the left to the consumed.
consumeChar :: ProgState -> (String,String)
consumeChar st = let f = state st
                    in (fst f ++ [head $ snd f],tail $ snd f)

-- | Converts a Char to a Word32
charToWord :: Char -> Word32
charToWord = toEnum . fromEnum
