-- Joe Jevnik
-- 9.11.2013

import System.Environment
import System.IO
import Control.Monad
import Control.Applicative
import Data.Array
import Data.Word

-- Data type to hold the state of the program.
data ProgState = ProgState { ptr     :: Int             -- Value Pointer
                           , vals    :: Array Int Word32  -- Values
                           , file    :: (String,String) -- (consumed,left)
                           , loop_fl :: (String,String) -- (consumed,left)
                           }

-- Starts consuming characters from the ProgState file.
main :: IO ()
main = do
    args <- getArgs
    consume_chars (parse_args args)
    return ()

-- Parses the command line arguments to interperet the flags.
parse_args :: [String]-> IO ProgState
parse_args args = do
    if head args == "-m"
      then do
          fl <-  readFile (args!!2) 
          return (ProgState 0 (listArray (0,read (args!!1) - 1) (repeat 0)) 
                                    ("",fl) ("",fl))
      else do
          fl <- readFile (head args)
          return (ProgState 0 (listArray (0,29999) (repeat 0))
                                    ("",fl) ("",fl))

-- Parses the next character from the ProgState.
consume_chars iost = do
    st <- iost
    if (not $ null ((snd . file) st))
      then do
          let c = (toEnum . fromEnum) $ (head . snd . file) st
          consume_chars $ parse_char c st
      else return (ProgState 0 (listArray (0,29999) (repeat 0)) ("","") 
                                 ("","")) 

-- Parses a character into the proper function.
parse_char :: Word32 -> ProgState -> IO ProgState
parse_char c st
    | c == (toEnum . fromEnum) '>' = ptr_right st
    | c == (toEnum . fromEnum) '<' = ptr_left st
    | c == (toEnum . fromEnum) '+' = val_incr st
    | c == (toEnum . fromEnum) '-' = val_decr st
    | c == (toEnum . fromEnum) '.' = val_prnt st
    | c == (toEnum . fromEnum) ',' = val_inpt st
    | c == (toEnum . fromEnum) '[' = if (vals st)!(ptr st) /= 0
                                       then beg_loop st
                                       else jmp_loop st
    | c == (toEnum . fromEnum) ']' = end_loop st
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

-- Prints the Word32 at vals!ptr.
val_prnt :: ProgState -> IO ProgState
val_prnt st = putChar ((toEnum . fromEnum) ((vals st)!(ptr st))) >> return 
              (ProgState (ptr st) (vals st) (cnsm_file_char st) (loop_fl st))

-- Reads a Word32 from the user to put at vals!ptr.
val_inpt :: ProgState -> IO ProgState
val_inpt st = do
    c <- getChar
    return (ProgState (ptr st) ((vals st)//[(ptr st,(toEnum . fromEnum) c)]) 
                          (cnsm_file_char st) (loop_fl st))

nul_func :: ProgState -> IO ProgState
nul_func st = return 
              (ProgState (ptr st) (vals st) (cnsm_file_char st) (loop_fl st))

-- Begins a loop -- TODO
beg_loop :: ProgState -> IO ProgState
beg_loop st = if (vals st)!(ptr st) == 0
                then jmp_loop st
                else return (ProgState (ptr st) (vals st) (cnsm_file_char st)
                                       (cnsm_file_char st))

-- Brings the program back to the top of a loop if the value at the ptr is /= 0.
end_loop :: ProgState -> IO ProgState
end_loop st = if (vals st)!(ptr st) == 0
                then nul_func st
                else return (ProgState (ptr st) (vals st) (loop_fl st)
                                       (loop_fl st))
                
-- Jumps to the end of a loop without processing the contents.
jmp_loop :: ProgState -> IO ProgState
jmp_loop st = do
    let c = (head . snd . file) st
    if c == (toEnum . fromEnum) ']'
      then end_loop st
      else nul_func st >>= jmp_loop

-- Moves one char from the left to the consumed.
cnsm_file_char :: ProgState -> (String,String)
cnsm_file_char st = let f = file st in (fst f ++ [head (snd f)],tail (snd f))
