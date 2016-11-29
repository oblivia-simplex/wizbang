module Lib where
import Data.Word
import Data.Bits
import qualified Numeric (showHex)
import qualified Data.ByteString.Char8 as B 
import Text.Regex.PCRE

data Status = ALIVE | DEAD | LOST | CORRUPTED Int deriving (Show, Eq)

data StatEnum = Strength
              | Intelligence
              | Piety
              | Vitality
              | Agility
              | Luck 
              deriving (Show, Enum, Eq)

showHex :: (Integral a) => a -> String
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
stats w = map (fromIntegral . maskS w . masksFor) 
          [Strength .. Luck]

en :: (Enum a, Enum b) => a -> b
en = toEnum . fromEnum

-- | For reading:
mask :: (Integral a, Bits a) => a -> Int -> Int -> a
mask w low high
  | low > high = error "Lower bound higher than upper bound"
  | low < 0 || high > 32 = error "Bit range out of bounds"
  | otherwise  = (highmask .&. w) `shiftR` low
  where highmask :: (Integral a) => a
        highmask = (2^high)-1

maskS :: (Integral a, Bits a) => a -> [(Int, Int)] -> a
maskS _ [] = 0
maskS w ((i,j):ms) = mask w i j .|. (maskS w ms `shiftL` (j-i))

-- | For writing:
stamp :: (Integral a, Bits a) => a -> [(Int, Int)] -> a
stamp val idxs  = foldr (.|.) 0 (st val idxs)
  where st :: (Integral a, Bits a) => a -> [(Int, Int)] -> [a]
        st _ [] = []
        st val ((i,j):is) = mask val 0 (j-i) `shiftL` i
                            : st (val `shiftR` i+1) is

stampW :: (Integral a, Bits a) => a -> [(Int, Int)] -> a -> a
stampW val idxs orig = stamp 0xFF idxs `xor` orig .|. stamp val idxs

-- | Seeking
-- | Finds the offset of the end of the character's name
seekName :: String -> B.ByteString -> Int
seekName name dsk = fst ((dsk =~ B.pack name) :: (Int, Int))

-- | The DWORD containing the stats appears 0x25 bytes after the end of
-- | the character's name
statsDwordFor :: Int -> B.ByteString -> Word32
statsDwordFor offset dsk = 
  readWord (B.drop (offset + statOffset) dsk)
  where statOffset = 
          0x2B - (nameLen offset dsk `mod` 2)

-- reads a big endian word from the first four bytes of a bytestring
readWord :: B.ByteString -> Word32
readWord bs = (head bsl   `shiftL` 24) .|.
              ((bsl !! 1) `shiftL` 16) .|.
              ((bsl !! 2) `shiftL` 8) .|.
              (bsl !! 3)
              where bsl = map ((.&. 0xFF) . en) $ B.unpack bs

statsFor :: Int -> B.ByteString -> [Int]
statsFor offset bs = stats $ statsDwordFor offset bs

getPassword :: Int -> B.ByteString -> String
getPassword offset bs =
  B.unpack $ B.take len $ B.drop (0x10 + offset) bs
  where len = en (bs `B.index` (0x0F + offset)) 

nameLen :: Int -> B.ByteString -> Int
nameLen offset bs = en $ bs `B.index` (offset - 1)

isAlive :: Int -> B.ByteString -> Status
isAlive offset bs = 
  let byte = en (bs `B.index` (offset + 0x27 - 
                                (nameLen offset bs `mod` 2)))
  in case byte of
    0x00 -> ALIVE
    0x05 -> DEAD
    0x07 -> LOST
    _    -> CORRUPTED byte
