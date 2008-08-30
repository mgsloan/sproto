module Text.Sproto where
import Data.Word
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as B

type WireId = Word32
data WireValue = WireVar WireId Word64
               | Wire64 WireId Word64
			   | WireString WireId Word32 String
			   | Wire32 WireId Word32
    deriving Show
	
data Field a = Field Word32 a

putId :: (Integral a, Bits a) => a -> a -> Put
putId i wt = putVarSInt $ i `shiftR` 3 + wt

instance Binary (Field WireValue) where
  put (Field i (Wire32 val)) = putId i 5 >> putLInt 4 val
  put (Field i (Wire64 val)) = putId i 1 >> putLInt 8 val
  put (Field i (WireVar val)) = putId i 0 >> putVarSInt val
  put (Field i (WireString val)) = putId i 2 >> putVarSInt (length val) >> put (B.pack val)
  get = do
    key <- getVarInt
    let id  = key `shiftR` 3
    let typ = key .&. 0x07
    case typ of
      0 -> getVarInt >>= return . Field id . WireVar
      1 -> getLInt 8 >>= return . Field id . Wire64
      5 -> getLInt 4 >>= return . Field id . Wire32
      2 -> do
        l <- getVarInt
        get >>= return . Field id . WireString . take l . B.unpack 

getLInt :: (Integral a, Bits a) => Int -> Get a
getLInt 0 = return 0
getLInt i = do
  b <- getWord8
  next <- getLInt (i-1)
  return $ fromIntegral b + (next `shiftL` 8)

putLInt :: (Integral a, Bits a) => Int -> a -> Put
putLInt 0 x = return () -- x should be 0, otherwise number too large to encode
putLInt i x = putWord8 (fromIntegral $ x .&. 0xFF) >> putLInt (i-1) (x `shiftR` 8)

--All following lines jacked straight from Christopher Kuklewicz's protocol buffers lib
getVarInt :: (Integral a, Bits a) => Get a
getVarInt = do -- optimize first read instead of calling (go 0 0)
  b <- getWord8
  if testBit b 7 then go 7 (fromIntegral (b .&. 0x7F))
    else return (fromIntegral b)
 where
  go n val = do
    b <- getWord8
    if testBit b 7 then go (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else return (val .|. ((fromIntegral b) `shiftL` n))

divBy :: (Ord a, Integral a) => a -> a -> a
divBy a b = let (q,r) = quotRem (abs a) b
            in if r==0 then q else succ q

putVarSInt :: (Integral a, Bits a) => a -> Put
putVarSInt b =
  case compare b 0 of
    LT -> let len = divBy (bitSize b) 7               -- (pred len)*7 < bitSize b <= len*7
              last'Size = (bitSize b)-((pred len)*7)  -- at least 1 and at most 7
              last'Mask = pred (1 `shiftL` last'Size) -- at least 1 and at most 255
              go i 1 = putWord8 (fromIntegral i .&. last'Mask)
              go i n = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7) (pred n)
          in go b len
    EQ -> putWord8 0
    GT -> putVarUInt b

-- This should be used on unsigned Integral types only (not checked)
putVarUInt :: (Integral a, Bits a) => a -> Put
putVarUInt b = let go i | i < 0x80 = putWord8 (fromIntegral i)
                        | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7)
               in go b


{-
-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
-}