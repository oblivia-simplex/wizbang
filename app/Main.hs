module Main where

import Lib
import Data.Word
import qualified Data.ByteString.Char8 as B

petra   = 0xe7292721 :: Word32
perfect = 0x524a524a :: Word32

pretty :: [Int] -> IO ()
pretty sts = mapM_ (putStrLn . showStat sts) [Strength .. Luck]
  where showStat st s = show s ++ ": " ++ show (st !! fromEnum s)

main :: IO ()
main = do
  b <- B.readFile "data/WIZ1.DSK"
  putStr "NAME > "
  name <- getLine
  --putStrLn "-=-=-=-=-= STATS FOR " ++ name ++ " =-=-=-=-=-"
  putStrLn $ "[+] READING CHARACTER DATA FROM OFFSET " ++ showHex (seekName name b)
  putStrLn $ "[+] PARSING DWORD " ++ showHex (statsDwordFor name b)
  pretty $ statsFor name b

