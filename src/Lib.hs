module Lib where
import Data.Word
import Data.Bits
import qualified Numeric (showHex)


data StatEnum = Strength
              | Intelligence
              | Piety
              | Vitality
              | Agility
              | Luck 
              deriving (Show, Enum, Eq)

showHex = (`Numeric.showHex` "") . toInteger . fromIntegral

masksFor :: StatEnum -> [(Int, Int)]
masksFor s = case s of
  Strength     -> [(24,29)]
  Intelligence -> [(29,32), (16,18)]
  Piety        -> [(18,23)]
  Vitality     -> [(8,13)]
  Agility      -> [(13,16), (0,2)]
  Luck         -> [(2,7)]

stats :: Word32 -> [Int]
stats w = map (fromIntegral . maskS w . masksFor) [Strength .. Luck]

en :: (Enum a, Enum b) => a -> b
en = toEnum . fromEnum

mask :: (Integral a, Bits a) => a -> Int -> Int -> a
mask w low high
  | low > high = error "Lower bound higher than upper bound"
  | low < 0 || high > 32 = error "Bit range out of bounds"
  | otherwise  = (highmask .&. w) `shiftR` low
  where highmask :: (Integral a) => a
        highmask = (2^high)-1

stamp :: (Integral a, Bits a) => a -> [(Int, Int)] -> a
stamp val idxs  = foldr (.|.) 0 (st val idxs)
  where st :: (Integral a, Bits a) => a -> [(Int, Int)] -> [a]
        st _ [] = []
        st val ((i,j):is) = mask val 0 (j-i) `shiftL` i
                            : st (val `shiftR` i+1) is

stampW :: (Integral a, Bits a) => a -> [(Int, Int)] -> a -> a
stampW val idxs orig = stamp 0xFF idxs `xor` orig .|. stamp val idxs

maskS :: (Integral a, Bits a) => a -> [(Int, Int)] -> a
maskS _ [] = 0
maskS w ((i,j):ms) = mask w i j .|. (maskS w ms `shiftL` (j-i))

-- v 
-- gold is just a little-endian integer starting at name_len + 0x34
