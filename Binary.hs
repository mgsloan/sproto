{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Sproto.Binary (
    WireValue(..), WireValues(..),
	getBytes,
	getVarInt, putVarSInt, putVarUInt,
    castWord64ToDouble, castWord32ToFloat, castDoubleToWord64, castFloatToWord32,
    zzEncode32, zzEncode64, zzDecode32, zzDecode64
  ) where

import Data.Binary
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy.Char8 as B

-- GHC internals for getting at Double and Float representation as Word64 and Word32
import GHC.Exts (Double(D#),Float(F#),unsafeCoerce#)
import GHC.Word (Word64(W64#),Word32(W32#))

import Data.Sproto.Types

newtype WireValues = WireValues [WireValue] deriving Show

instance Binary WireValues where
  put (WireValues xs) = mapM_ put xs
  get = do
    e <- G.isEmpty
    if e then return (WireValues []) else do
      x <- get
      (WireValues xs) <- get
      return . WireValues $ x : xs

putId :: (Integral a, Bits a) => a -> a -> Put
putId i wt = putVarSInt $ i `shiftL` 3 .|. wt

data WireValue = WireVar WireId Word64
               | Wire64 WireId Word64
               | WireString WireId B.ByteString               | Wire32 WireId Word32
               | WireGroupOpen WireId
               | WireGroupClose WireId
    deriving Show

instance Binary WireValue where
  put (Wire32 i val) = putId i 5 >> P.putWord32be val
  put (Wire64 i val) = putId i 1 >> P.putWord64be val
  put (WireVar i val) = putId i 0 >> putVarSInt val
  put (WireString i val) = putId i 2 >> putVarSInt (B.length val) >> put val
  get = do
    key <- getVarInt
    let i  = key `shiftR` 3
    let typ = key .&. 0x07
    case typ of
      0 -> getVarInt >>= return . WireVar i
      1 -> G.getWord64be >>= return . Wire64 i
      5 -> G.getWord32be >>= return . Wire32 i
      2 -> getVarInt >>= G.getLazyByteString >>= return . WireString i
      3 -> return $ WireGroupOpen i
      4 -> return $ WireGroupClose i

getBytes = getVarInt >>= G.getLazyByteString

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

castWord64ToDouble (W64# w) = D# (unsafeCoerce# w)
castWord32ToFloat (W32# w) = F# (unsafeCoerce# w)
castDoubleToWord64 (D# d) = W64# (unsafeCoerce# d)
castFloatToWord32 (F# d) = W32# (unsafeCoerce# d)

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
