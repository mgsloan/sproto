{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Sproto.Binary (
    WireField(..), WireValue(..), WireFields(..),
	getBytes,
	getVarInt, putVarSInt, putVarUInt,
    castFloat, castDouble, zigzag
  ) where

import Data.Binary
import Data.Bits
import Data.Int
import Data.Word
import Data.Char (ord)
import Numeric (showHex)
import Text.Show(showListWith)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy.Char8 as B

-- GHC internals for getting at Double and Float representation as Word64 and Word32
import GHC.Exts (Double(D#),Float(F#),unsafeCoerce#)
import GHC.Word (Word64(W64#),Word32(W32#))

import Data.Sproto.Types

newtype WireFields = WireFields [WireField]

instance Binary WireFields where
  put (WireFields xs) = mapM_ put xs
  get = do
    e <- G.isEmpty
    if e then return (WireFields []) else do
      x <- get
      (WireFields xs) <- get
      return . WireFields $ x : xs

data WireField = WireField WireId WireValue

data WireValue = WireVar Word64
               | Wire64 Word64
               | WireString B.ByteString               | Wire32 Word32
               | WireGroup
instance Show WireValue where
  show (WireVar x) = showHex x ""
  show (Wire64 x) = showHex x ""
  show (WireString x) = (showListWith showHex . map (ord) . B.unpack $ x) $ ""
  show (Wire32 x) = showHex x ""

putId :: (Integral a, Bits a) => a -> a -> Put
putId i wt = putVarSInt $ i `shiftL` 3 .|. wt

instance Binary WireField where
  put (WireField i (Wire32 val)) = putId i 5 >> P.putWord32be val
  put (WireField i (Wire64 val)) = putId i 1 >> P.putWord64be val
  put (WireField i (WireVar val)) = putId i 0 >> putVarSInt val
  put (WireField i (WireString val)) = putId i 2 >> putVarSInt (B.length val) >> put val
  get = do
    key <- getVarInt
    let i  = key `shiftR` 3
    let typ = key .&. 0x07
    (\f ->
     case typ of
      0 -> getVarInt >>= f . WireVar
      1 -> G.getWord64be >>= f . Wire64
      5 -> G.getWord32be >>= f . Wire32
      2 -> getVarInt >>= G.getLazyByteString >>= f . WireString
      3 -> f $ WireGroup
      4 -> f $ WireGroup
     ) $ return . WireField i

getBytes = getVarInt >>= G.getLazyByteString

--All following lines derived from Christopher Kuklewicz's protocol buffers lib
castDouble :: Iso Double Word64
castDouble = Iso (\(D# d) -> W64# (unsafeCoerce# d))
                 (\(W64# w) -> D# (unsafeCoerce# w))
castFloat :: Iso Float Word32
castFloat = Iso (\(F# d) -> W32# (unsafeCoerce# d))
                (\(W32# w) -> F# (unsafeCoerce# w))

zigzag :: (Integral a, Bits a, Integral b, Bits b) => Int -> Iso a b
zigzag n = Iso (\x -> fromIntegral $ (x `shiftL` 1) `xor` (x `shiftR` (n-1)))
               (\x -> (fromIntegral $ x `shiftR` 1) `xor` (negate . fromIntegral $ x .&. 1))

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
