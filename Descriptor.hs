module Data.Sproto.Descriptor where

import Data.Int
import Data.Word
import Data.Ranged
import Data.List (partition, find)
import Data.Binary (decode)
import Control.Arrow (first, second)
import Data.Maybe (fromJust)
import Data.Sproto.Types
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.Sproto.Binary

data FieldType = TYPE_DOUBLE | TYPE_FLOAT | TYPE_INT64 | TYPE_UINT64 | TYPE_INT32 | TYPE_FIXED64 | TYPE_FIXED32 | TYPE_BOOL | TYPE_STRING
               | TYPE_GROUP | TYPE_MESSAGE | TYPE_BYTES | TYPE_UINT32 | TYPE_ENUM | TYPE_SFIXED32 | TYPE_SFIXED64 | TYPE_SINT32 | TYPE_SINT64
                   deriving (Enum, Eq, Read, Show)
data FieldLabel = Optional | Required | Repeated
                    deriving (Enum, Eq, Read, Show)

data FieldProto = FieldProto { protoName :: String, protoNumber :: WireId, protoLabel :: FieldLabel, protoType :: FieldType, protoTypeName, protoDefault :: String }

instance Show FieldProto where
  show (FieldProto name (WireId number) label typ tname def) = show label ++ " " ++
     (if typ /= TYPE_ENUM && typ /= TYPE_MESSAGE then show typ else tname) ++ " = " ++ show number

type MessageProto = [FieldProto]
type EnumProto = IsoMap String Int
type Protos = M.Map String (Either MessageProto EnumProto)

data FieldValue = MessageValue Message
                | EnumValue   String Integer
                | DoubleValue Double
                | FloatValue  Float
                | IntValue    Integer
                | BoolValue   Bool
                | StringValue String
                | BytesValue  B.ByteString

data Message = Message (M.Map String Member) [WireField]

data Member = RepeatedMember [FieldValue]
            | Member FieldValue

mapMember :: (FieldValue -> FieldValue) -> Member -> Member
mapMember f (Member x) = Member $ f x
mapMember f (RepeatedMember xs) = RepeatedMember $ map f xs

member s (Message x _) = fromJust $ M.lookup s x

merge :: FieldType -> [FieldValue] -> FieldValue
merge TYPE_MESSAGE = BytesValue . B.concat . map (\(BytesValue x) -> x)
merge _ = last

readMsg :: Protos -> String -> WireFields -> Message
readMsg env name = resolveMessage env proto . readMessage proto
  where (Left proto) = fromJust $ M.lookup name env

readMessage :: MessageProto -> WireFields -> Message
readMessage desc (WireFields fields) = Message (M.fromList known) unknown
  where (known, unknown) = foldr (\fp (k, u) -> first (:k) $ process fp u) ([], fields) desc --TODO: make sure it's ordered right
        process :: FieldProto -> [WireField] -> ((String, Member), [WireField])
        process (FieldProto nam ix lab typ _ def)  = first (processMember . map (\(WireField _ val) -> wireToField typ val)) . partition (\(WireField i _) -> i == ix)
          where processMember :: [FieldValue] -> (String, Member)
                processMember xs = (nam,
                  case (lab, xs) of
                    (Required, []) -> error "blargh!"
                    (Optional, []) -> Member $ readField typ def
                    (Repeated, xs) -> RepeatedMember xs
                    _ -> Member (merge typ xs)
                 )

resolveMessage :: Protos -> MessageProto -> Message -> Message
resolveMessage env desc = foo TYPE_MESSAGE processMessage . foo TYPE_ENUM processEnum
  where foo :: FieldType -> (FieldProto -> FieldValue -> FieldValue) -> Message -> Message
        foo typ f (Message m wfs) = Message (M.mapWithKey (\name -> mapMember (maybe id f $ M.lookup name protos)) m) wfs
          where protos = M.fromList . map (\x -> (protoName x, x)) $ filter ((==typ).protoType) desc
        processMessage pr (BytesValue bs) =
          case M.lookup (protoTypeName pr) env of
            (Just (Left mProto)) -> MessageValue . resolveMessage env mProto . readMessage mProto $ decode bs
            _ -> BytesValue bs
        processEnum pr (IntValue ix) =
          case M.lookup (protoTypeName pr) env of
            (Just (Right mEnum)) -> EnumValue (maybe "" id (lookupFrom (fromIntegral ix) mEnum)) ix
            _ -> IntValue ix

toListWith :: (k -> a -> b) -> M.Map k a -> [b]
toListWith f = map (\(x,y) -> f x y) . M.assocs

writeMessage :: MessageProto -> Message -> WireFields
writeMessage desc (Message members wfs) = WireFields $ concat (toListWith process members) ++ wfs
  where process name m = case m of
                           (Member x) -> [conv x]
                           (RepeatedMember xs) -> map conv xs
          where proto = fromJust $ find ((==name).protoName) desc
                conv x = WireField (protoNumber proto) (fieldToWire (protoType proto) x)

-- This function returns an Iso (Isomorphism) for converting fieldvalues to and
-- from wirevalues, given a type. We use Isos throughout this function.  The form
-- fieldConstructorIso >>> manipulation >>> wireConstructorIso, is used.
convertType :: FieldType -> Iso FieldValue WireValue
convertType t = case t of
  TYPE_DOUBLE -> Iso  (\(DoubleValue x) -> x) DoubleValue >>> castDouble >>> w64
  TYPE_FLOAT  -> Iso   (\(FloatValue x) -> x) FloatValue  >>> castFloat  >>> w32
  TYPE_BOOL   -> Iso    (\(BoolValue x) -> x) BoolValue   >>> castBool   >>> wVar
  TYPE_STRING -> Iso  (\(StringValue x) -> x) StringValue >>> Iso B.pack B.unpack >>> wStr
  TYPE_MESSAGE -> Iso  (\(BytesValue x) -> x) BytesValue  >>> wStr
  TYPE_BYTES   -> Iso  (\(BytesValue x) -> x) BytesValue  >>> wStr
  TYPE_SINT32   -> fieldInt >>> zigzag 32 >>> wVar
  TYPE_SINT64   -> fieldInt >>> zigzag 64 >>> wVar
  TYPE_FIXED32  -> fieldInt >>> castInt   >>> w32
  TYPE_SFIXED32 -> fieldInt >>> castInt   >>> w32
  TYPE_FIXED64  -> fieldInt >>> castInt   >>> w64
  TYPE_SFIXED64 -> fieldInt >>> castInt   >>> w64
  --TYPE_ENUM --initially stored as an IntegralField
  --TYPE_GROUP
  _ -> fieldInt >>> castInt >>> wVar
 where w32 = Iso Wire32     (\(Wire32 x) -> x)
       w64 = Iso Wire64     (\(Wire64 x) -> x)
       wVar= Iso WireVar    (\(WireVar x) -> x)
       wStr= Iso WireString (\(WireString x) -> x)
       fieldInt = Iso (\(IntValue x) -> x) IntValue
       castInt :: (Integral a, Integral b) => Iso a b
       castInt = Iso fromIntegral fromIntegral
       castBool = Iso (\x->if x then 1 else 0) (>0)

fieldToWire = to . convertType
wireToField = from . convertType

readField :: FieldType -> (String -> FieldValue)
readField t = case t of
  TYPE_DOUBLE -> DoubleValue . read
  TYPE_FLOAT  -> FloatValue  . read
  TYPE_BOOL   -> BoolValue   . (\x -> if x == "true" then True else False)
  TYPE_STRING -> StringValue
  TYPE_BYTES  -> BytesValue  . read
  _ -> IntValue . read
