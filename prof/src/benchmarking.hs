-- Joe Jevnik
-- 20.12.2013
-- The benchmarking tests for my brainfuck interpreter implementation
-- interations.

import Criterion.Main
import Criterion.Config

import qualified UnsafeBrainfuck as UBF (main)
import qualified NewArrayTest    as NAT (main)
import qualified SafeBrainfuck   as SBF (main)
import qualified ByteStringTest  as BST (main)

args :: IO [String]
args = return ["../txt/squares"]

cfg :: Config
cfg = defaultConfig { cfgPerformGC = ljust True }

main = defaultMainWith cfg (return ()) [
        bgroup "brainfuck" [ bench "unsafe"  $ nfIO (UBF.main args)
                           , bench "newArr"  $ nfIO (NAT.main args)
                           , bench "safe"    $ nfIO (SBF.main args)
                           , bench "bytestr" $ nfIO (BST.main args)
                           ]
       ]
