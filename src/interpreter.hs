-- Joe Jevnik
-- 9.11.2013

import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Control.Applicative
import Data.Array

-- Data type to hold the state of the program.
data ProgState = ProgState { ptr     :: Int             -- Value Pointer
                           , vals    :: Array Int Char  -- Values
                           , file    :: (String,String) -- (consumed,left)
                           , loop_fl :: (String,String) -- (consumed,left)
                           }

main :: IO ()
main = do
    fl <- (head <$> getArgs) >>= readFile
    let ps = return (ProgState 0 (listArray (0,29999) (repeat '0')) 
                                   ("",fl) ("",fl))
    consume_chars ps
    return ()

-- Parses the next character from the ProgState.
consume_chars iost = do
    st <- iost
    if (not $ null ((snd . file) st))
      then do
          let c = (head . snd . file) st
          consume_chars $ parse_char c st
      else return (ProgState 0 (listArray (0,29999) (repeat '0')) ("","") 
                                 ("","")) 

-- Parses a character into the proper function.
parse_char :: Char -> ProgState -> IO ProgState
parse_char c st
    | c == '>' = ptr_right st
    | c == '<' = ptr_left st
    | c == '+' = val_incr st
    | c == '-' = val_decr st
    | c == '.' = val_prnt st
    | c == ',' = val_inpt st
    | otherwise = nul_func st


-- Moves the current array pointer right by 1 index.
ptr_right :: ProgState -> IO ProgState
ptr_right st = return (ProgState (ptr st + 1) (vals st) (cnsm_file_char st) 
               (loop_fl st))

-- Moves the current array pointer left by 1 index.
ptr_left :: ProgState -> IO ProgState
ptr_left st = return (ProgState (ptr st - 1) (vals st) (cnsm_file_char st) 
              (loop_fl st))

-- Increments the value at the current pointer by one.
val_incr :: ProgState -> IO ProgState
val_incr st = return (ProgState (ptr st) 
              ((vals st)//[(ptr st,succ $ (vals st)!(ptr st))]) 
              (cnsm_file_char st) (loop_fl st))

-- Decrements the value at the current pointer by one.
val_decr :: ProgState -> IO ProgState
val_decr st = return (ProgState (ptr st) 
              ((vals st)//[(ptr st,pred $ (vals st)!(ptr st))]) 
              (cnsm_file_char st) (loop_fl st))

-- Prints the Char at vals!ptr.
val_prnt :: ProgState -> IO ProgState
val_prnt st = putChar ((vals st)!(ptr st)) >> return 
              (ProgState (ptr st) (vals st) (cnsm_file_char st) (loop_fl st))

-- Reads a Char from the user to put at vals!ptr.
val_inpt :: ProgState -> IO ProgState
val_inpt st = do
    c <- getChar
    return (ProgState (ptr st) ((vals st)//[(ptr st,c)]) (cnsm_file_char st) 
                          (loop_fl st))

nul_func :: ProgState -> IO ProgState
nul_func st = return 
              (ProgState (ptr st) (vals st) (cnsm_file_char st) (loop_fl st))

-- Begins a loop -- TODO
beg_loop :: ProgState -> IO ProgState
beg_loop st = return st

cnsm_file_char :: ProgState -> (String,String)
cnsm_file_char st = let f = file st in (fst f ++ [head (snd f)],tail (snd f))
