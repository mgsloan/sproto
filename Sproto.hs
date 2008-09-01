{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Sproto where
import Data.Word
import Data.Int
import Data.Binary
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sproto.Types
import Data.Sproto.Descriptor
import Data.Maybe
import Data.Char (chr)
import qualified Data.Map as M
-- GHC internals for getting at Double and Float representation as Word64 and Word32
import GHC.Exts (Double(D#),Float(F#),unsafeCoerce#)
import GHC.Word (Word64(W64#),Word32(W32#))

data WireValue = WireVar WireId Word64
               | Wire64 WireId Word64
               | WireString WireId B.ByteString               | Wire32 WireId Word32
    deriving Show

wireId (WireVar x _) = x
wireId (Wire64 x _) = x
wireId (WireString x _) = x
wireId (Wire32 x _) = x

data FieldValue = MessageVal  [Field]
                | EnumVal     Integer String
                | DoubleVal   Double
                | FloatVal    Float
                | IntegralVal Integer
                | BoolVal     Bool
                | StringVal   String
                | BytesVal    B.ByteString
  deriving Show

data Field = Field String WireId FieldValue
           | UnknownField WireValue
  deriving Show

test x n d = G.runGet (readMsg (toDicts x) n) $ B.pack $ map chr d

readMsg :: DefDicts -> String -> Get FieldValue
readMsg env v = return . MessageVal =<< (readFields env . fromJust . M.lookup v . fst $ env)

readFields :: DefDicts -> M.Map WireId MessageField -> Get [Field]
readFields env fs = do
  e <- G.isEmpty
  if e then return [] else do
    f <- readField env fs
    others <- readFields env fs
    return $ f : others

readField :: DefDicts -> M.Map WireId MessageField -> Get Field
readField env fs =
  do wireVal <- get
     let ix = wireId wireVal
     case M.lookup ix fs of
       Just (MessageField _ n _ t) -> return . Field n ix $ readVal env t wireVal
       Nothing -> return $ UnknownField wireVal

{-
data CustomTypReader = CustomTypReader DefDicts String B.ByteString

instance Binary CustomTypReader where
  get (CustomTypReader env n str) = 
  put = error "Not implemented"
-}

withVar :: (Word64 -> a) -> WireValue -> a
withVar f val = case val of
  (WireVar _ x) -> f x
  _ -> error "Expected VarInt field"

withString :: (B.ByteString -> a) -> WireValue -> a
withString f val = case val of
  (WireString _ x) -> f x
  _ -> error "Expected bytes field"

with32 :: (Word32 -> a) -> WireValue -> a
with32 f val = case val of
  (Wire32 _ x) -> f x
  _ -> error "Expected 32-bit field"

with64 :: (Word64 -> a) -> WireValue -> a
with64 f val = case val of
  (Wire64 _ x) -> f x
  _ -> error "Expected 64-bit field"

integ :: (Integral a) => a -> FieldValue
integ = IntegralVal . fromIntegral

readVal :: DefDicts -> FieldType -> WireValue -> FieldValue 
readVal env typ val =
  (case typ of
    CustomTyp str -> withString $ G.runGet $ readMsg env str
    EnumTyp str def -> withVar (\x -> EnumVal (fromIntegral x) "")
    DoubleTyp  def -> with64 $ DoubleVal . castWord64ToDouble
    FloatTyp   def -> with32 $ FloatVal . castWord32ToFloat
    Int32Typ   def -> withVar integ
    Int64Typ   def -> withVar integ
    UInt32Typ  def -> withVar integ
    UInt64Typ  def -> withVar integ
    SInt32Typ  def -> withVar $ integ . zzDecode32
    SInt64Typ  def -> withVar $ integ . zzDecode64
    Fixed32Typ def -> with32 integ
    Fixed64Typ def -> with64 integ
    BoolTyp    def -> withVar $ BoolVal . (>0)
    StringTyp  def -> withString $ StringVal . B.unpack
    BytesTyp -> withString BytesVal
  ) $ val

putId :: (Integral a, Bits a) => a -> a -> Put
putId i wt = putVarSInt $ i `shiftL` 3 .|. wt

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
      2 -> do
        l <- getVarInt
        G.getLazyByteString l >>= return . WireString i

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

castWord64ToDouble :: Word64 -> Double
castWord64ToDouble (W64# w) = D# (unsafeCoerce# w)
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat (W32# w) = F# (unsafeCoerce# w)
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 (D# d) = W64# (unsafeCoerce# d)
castFloatToWord32 :: Float -> Word32
castFloatToWord32 (F# d) = W32# (unsafeCoerce# d)

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word64
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word64 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
