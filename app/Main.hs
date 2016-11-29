module Main where

import Lib

petra = 0xe7292721

main :: IO ()
main = mapM_ putStrLn $ map (showStat petra) [Strength .. Luck]
  where showStat w s = (show s) ++ ": " ++ (show $ stats w !! fromEnum s)

