{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Sproto where
import Data.Binary (decode)
import Data.Char (chr)
import Data.Function (on)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M

import Data.Sproto.Types
import Data.Sproto.Descriptor
import Data.Sproto.Binary

data FieldValue = MessageVal  [Field]
                | EnumVal     Integer String
                | DoubleVal   Double
                | FloatVal    Float
                | IntegralVal Integer
                | BoolVal     Bool
                | StringVal   String
                | BytesVal    B.ByteString
  deriving Show

data Field = Field WireId FieldValue
           | UnknownField WireValue
  deriving Show

test x n = readMsg (toDicts x) n . decode . B.pack . map chr

readMsg :: DefDicts -> String -> WireValues -> FieldValue
readMsg env v (WireValues xs) = MessageVal . processFields fs . map (readField env fs) $ xs
  where fs = fromJust $ M.lookup v $ fst env

wireId (WireVar x _) = x
wireId (Wire64 x _) = x
wireId (WireString x _) = x
wireId (Wire32 x _) = x

fid ix x = case x of
            Field i _ | i == ix -> True
--            UnknownField  | i == ix -> True
            _ -> False

--protoMerge :: FieldValue -> FieldValue -> FieldValue
--protoMerge (MessageVal xs) (MessageVal ys) 

processFields decls fs = fs {- map process $ elems decls
  where process (MessageField rule name ix typ) =
            case rule of
              Required -> merge fields
              Optional -> merge fields
              Repeated -> fields
          where fields = filter (fid ix)
-}

readField :: DefDicts -> M.Map WireId MessageField -> WireValue -> Field
readField env fs wireVal =
  let ix = wireId wireVal in
     case M.lookup ix fs of
       Just (MessageField _ _ _ t) -> Field ix $ readVal env t wireVal
       Nothing -> UnknownField wireVal

withVar f val = case val of
  (WireVar _ x) -> f x
  _ -> error "Expected VarInt field"
withString f val = case val of
  (WireString _ x) -> f x
  _ -> error "Expected bytes field"
with32 f val = case val of
  (Wire32 _ x) -> f x
  _ -> error "Expected 32-bit field"
with64 f val = case val of
  (Wire64 _ x) -> f x
  _ -> error "Expected 64-bit field"

integ :: (Integral a) => a -> FieldValue
integ = IntegralVal . fromIntegral

readVal :: DefDicts -> FieldType -> WireValue -> FieldValue 
readVal env typ val =
  (case typ of
    CustomTyp str -> withString $ readMsg env str . decode
    EnumTyp str def -> withVar (\x -> EnumVal (fromIntegral x) "")
    DoubleTyp  def -> with64 $ DoubleVal . castWord64ToDouble
    FloatTyp   def -> with32 $ FloatVal . castWord32ToFloat
    Int32Typ   def -> withVar integ
    Int64Typ   def -> withVar integ
    UInt32Typ  def -> withVar integ
    UInt64Typ  def -> withVar integ
    SInt32Typ  def -> withVar $ IntegralVal . zzDecode32
    SInt64Typ  def -> withVar $ IntegralVal . zzDecode64
    Fixed32Typ def -> with32 integ
    Fixed64Typ def -> with64 integ
    BoolTyp    def -> withVar $ BoolVal . (>0)
    StringTyp  def -> withString $ StringVal . B.unpack
    BytesTyp -> withString BytesVal
  ) $ val
