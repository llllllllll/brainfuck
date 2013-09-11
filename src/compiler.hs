-- Joe Jevnik
-- 9.11.2013

import System.Environment
import System.Process
import System.IO
import System.Directory
import Control.Monad
import Control.Applicative
import Data.Array
import Data.Word
import Data.List

main :: IO ()
main = do
    args <- getArgs
    fl <- readFile $ head args
    build (fl) (args!!1 ++ ".hs")
    return ()

build :: String -> FilePath -> IO ()
build inp out = do
    let main_body = foldl (|>>=|) "iost" [parse_char c | c <- inp]
    removeFile out
    appendFile out $ import_list ++ prog_state ++ funcs ++ iost inp 
                   ++ main_decl ++ main_body 
    
(|>>=|) :: String -> String -> String
(|>>=|) a b = a ++ "\n        >>= " ++ b

-- Parses a character into the proper function.
parse_char :: Char -> String
parse_char c
    | c == '>' = "ptr_right"
    | c == '<' = "ptr_left"
    | c == '+' = "val_incr"
    | c == '-' = "val_decr"
    | c == '.' = "val_prnt"
    | c == ',' = "val_inpt"
    | c == '[' = "(\\st -> if (vals st)!(ptr st) /= 0\n"
                 ++ "                  then beg_loop st\n"
                 ++ "                  else jmp_loop st)\n"
    | c == ']' = "end_loop"
    | otherwise = "nul_func"


-- The list of imports for the intermediate code.
import_list :: String
import_list = (concatMap (\cs -> "import " ++ cs ++ "\n" ) 
              [ "System.IO"
              , "Control.Monad"
              , "Control.Applicative"
              , "Data.Array"
              , "Data.Word" ]) ++ "\n"

main_decl :: String
main_decl = "main :: IO ()\nmain = do\n    "

iost :: String -> String
iost inp = "iost :: IO ProgState\niost = "
           ++ "return (ProgState 0 (listArray (0,30000) (repeat 0))"
           ++ "(\"\"," ++ show inp ++ ") (\"\"," ++ show inp ++ "))\n\n"

prog_state :: String
prog_state = "data ProgState = ProgState { ptr     :: Int\n"
             ++ "    , vals    :: Array Int Word32\n"
             ++ "    , file    :: (String,String)"
             ++ "    , loop_fl :: (String,String) }\n\n"

funcs :: String
funcs = (concat $ intersperse "\n\n" 
        [ "ptr_right :: ProgState -> IO ProgState\n"
          ++ "ptr_right st = return (ProgState (ptr st + 1) (vals st)"
          ++ " (cnsm_file_char st) (loop_fl st))"
        , "ptr_left :: ProgState -> IO ProgState\n"
          ++ "ptr_left st = return (ProgState (ptr st - 1) (vals st)" 
          ++ "(cnsm_file_char st) (loop_fl st))"
        , "val_incr :: ProgState -> IO ProgState\n"
          ++ "val_incr st = return (ProgState (ptr st)"
          ++ " ((vals st)//[(ptr st,succ $ (vals st)!(ptr st))])"
          ++ " (cnsm_file_char st) (loop_fl st))"
        , "val_decr :: ProgState -> IO ProgState\n"
          ++ "val_decr st = return (ProgState (ptr st)" 
          ++ " ((vals st)//[(ptr st,pred $ (vals st)!(ptr st))])" 
          ++ " (cnsm_file_char st) (loop_fl st))"
        , "val_prnt :: ProgState -> IO ProgState\n"
          ++ "val_prnt st = putChar ((toEnum . fromEnum)"
          ++ " ((vals st)!(ptr st))) >> return (ProgState (ptr st)"
          ++ " (vals st) (cnsm_file_char st) (loop_fl st))"
        , "val_inpt :: ProgState -> IO ProgState\n"
          ++ "val_inpt st = do { c <- getChar; return (ProgState (ptr st)"
          ++ " ((vals st)//[(ptr st,(toEnum . fromEnum) c)])"
          ++ " (cnsm_file_char st) (loop_fl st)); }"
        , "nul_func :: ProgState -> IO ProgState\n"
          ++ "nul_func st = return (ProgState (ptr st) (vals st)"
          ++ " (cnsm_file_char st) (loop_fl st))"
        , "end_loop :: ProgState -> IO ProgState\n"
          ++ "end_loop st = if (vals st)!(ptr st) == 0"
          ++ " then nul_func st else return (ProgState (ptr st)"
          ++  " (vals st) (loop_fl st) (loop_fl st))"
        , "jmp_loop :: ProgState -> IO ProgState\n"
          ++ "jmp_loop st = do\n    let c = (head . snd . file) st\n    "
          ++ "if c == (toEnum . fromEnum) ']'\n        then end_loop st\n    "
          ++ "    else nul_func st >>= jmp_loop"
        , "cnsm_file_char :: ProgState -> (String,String)\n"
          ++ "cnsm_file_char st = let f = file st in (fst f ++"
          ++ " [head (snd f)],tail (snd f))"
        , "beg_loop :: ProgState -> IO ProgState\n"
          ++ "beg_loop st = if (vals st)!(ptr st) == 0\n"
          ++ "        then jmp_loop st\n        else return (ProgState (ptr st)"
          ++ " (vals st)  (cnsm_file_char st) (cnsm_file_char st))" ]) ++ "\n"
