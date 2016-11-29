module Main where

import Lib
import Data.Word

petra   = 0xe7292721 :: Word32
perfect = 0x524a524a :: Word32

main :: IO ()
main = pretty petra

