-- Joe Jevnik
-- 9.11.2013
-- Edited: 13.10.2013

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Array.Unboxed (Array,(!),(//),listArray)
import Data.Word (Word32)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Data type to hold the state of the program.
data ProgState = ProgState { ptr     :: Int               -- Value Pointer
                           , vals    :: Array Int Word32  -- Values
                           , file    :: (String,String)   -- (consumed,left)
                           , loop_fl :: [(String,String)] -- (consumed,left)
                            }

-- |Starts consuming characters from the ProgState file.
main :: IO ()
main = void $ getArgs >>= parse_args >>=  consume_chars

-- Parses the command line arguments to interperet the flags.
parse_args :: [String]-> IO ProgState
parse_args as =
    if head as == "-m"
      then do
          fl <- readFile $ as!!2
          let ln=  case readMaybe $ as!!1 of
                       Nothing -> error "'-m' expects and Int argument"
                       Just n  -> n - 1
          return ProgState { ptr     = 0
                           , vals    = listArray (0,ln) (repeat 0)
                           , file    = ("",fl)
                           , loop_fl = []
                           }
      else do
          fl <- readFile $ head as
          return ProgState { ptr     = 0
                           , vals    = listArray (0,29999) (repeat 0)
                           , file    = ("",fl)
                           , loop_fl = []
                           }

-- |Parses the next character from the ProgState.
consume_chars :: ProgState -> IO ProgState
consume_chars st = do
    if (not $ null ((snd . file) st))
      then do
          let c = (toEnum . fromEnum) $ (head . snd . file) st
          parse_char c st >>= consume_chars
      else return (ProgState 0 (listArray (0,29999) (repeat 0)) ("","") [])

-- |Parses a character into the proper function.
parse_char :: Word32 -> ProgState -> IO ProgState
parse_char c st
    | c == (toEnum . fromEnum) '>' = return $ ptr_right st
    | c == (toEnum . fromEnum) '<' = return $ ptr_left st
    | c == (toEnum . fromEnum) '+' = return $ val_incr st
    | c == (toEnum . fromEnum) '-' = return $ val_decr st
    | c == (toEnum . fromEnum) '.' = val_prnt st
    | c == (toEnum . fromEnum) ',' = val_inpt st
    | c == (toEnum . fromEnum) '[' = if (vals st)!(ptr st) /= 0
                                       then return $ beg_loop st
                                       else return $ jmp_loop st
    | c == (toEnum . fromEnum) ']' = return $ end_loop st
    | otherwise = return $ nul_func st


-- |Moves the current array pointer right by 1 index.
ptr_right :: ProgState -> ProgState
ptr_right st = st { ptr = ptr st + 1
                  , file = cnsm_file_char st
                  }

-- |Moves the current array pointer left by 1 index.
ptr_left :: ProgState -> ProgState
ptr_left st = st { ptr = ptr st - 1
                 , file = cnsm_file_char st
                 }

-- |Increments the value at the current pointer by one.
val_incr :: ProgState -> ProgState
val_incr st = st { vals = vals st//[(ptr st,succ $ (vals st)!(ptr st))]
                 , file = cnsm_file_char st
                 }

-- |Decrements the value at the current pointer by one.
val_decr :: ProgState -> ProgState
val_decr st = st { vals = vals st//[(ptr st,pred $ (vals st)!(ptr st))]
                 , file = cnsm_file_char st
                 }

-- |Prints the Word32 at vals!ptr.
val_prnt :: ProgState -> IO ProgState
val_prnt st = putChar ((toEnum . fromEnum) ((vals st)!(ptr st))) >> return
              st { file = cnsm_file_char st }

-- |Reads a Word32 from the user to put at vals!ptr.
val_inpt :: ProgState -> IO ProgState
val_inpt st = getChar >>= \c
            -> return st { vals = vals st//[(ptr st,(toEnum . fromEnum) c)]
                         , file = cnsm_file_char st
                         }


-- |Consumes one character but otherwise has no affect on the ProgState.
nul_func :: ProgState -> ProgState
nul_func st = st { file = cnsm_file_char st }

-- |Begins a loop.
beg_loop :: ProgState -> ProgState
beg_loop st = if (vals st)!(ptr st) == 0
                then jmp_loop st
                else st { file    = cnsm_file_char st
                        , loop_fl = cnsm_file_char st:loop_fl st
                        }

-- |Brings the program back to the top of a loop if the value at the ptr is /= 0.
end_loop :: ProgState -> ProgState
end_loop st = if (vals st)!(ptr st) == 0
                then st { file    = cnsm_file_char st
                        , loop_fl = tail $ loop_fl st
                        }
                else st { file    = head $ loop_fl st }

-- Jumps to the end of a loop without processing the contents.
jmp_loop :: ProgState -> ProgState
jmp_loop st = if (head . snd . file) st == (toEnum . fromEnum) ']'
                then end_loop st
                else jmp_loop $ nul_func st

-- |Moves one char from the left to the consumed.
cnsm_file_char :: ProgState -> (String,String)
cnsm_file_char st = let f = file st
                    in (fst f ++ [head $ snd f],tail $ snd f)
