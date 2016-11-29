module Main where

import Lib
import Data.Word
import qualified Data.ByteString.Char8 as B
import Data.Char

petra   = 0xe7292721 :: Word32
perfect = 0x524a524a :: Word32

pretty :: [Int] -> IO ()
pretty sts = mapM_ (putStrLn . showStat sts) [Strength .. Luck]
  where showStat st s = show s ++ ": " ++ show (st !! fromEnum s)

main :: IO ()
main = do
  b <- B.readFile "data/WIZ1.DSK"
  putStr "NAME > "
  name   <- (fmap . fmap) toUpper getLine
  let offset = seekName name b
  putStrLn $ "-=-=-=-=-=-=-= STATS FOR " ++ name ++ " =-=-=-=-=-=-=-"
  putStrLn $ "[+] READING CHARACTER DATA FROM OFFSET " 
             ++ showHex offset
  putStrLn $ "[+] PASSWORD: " ++ getPassword offset b
  putStrLn $ "[+] PARSING DWORD " ++ showHex (statsDwordFor offset b)
  pretty $ statsFor offset b
  putStrLn $ "[+] STATUS: " ++ show (isAlive offset b)

