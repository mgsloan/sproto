module Data.Sproto.Descriptor (
    FieldType(..), FieldRule(..), ProtoField(..), Proto(..), DefDicts, 
    toDicts
  ) where
import Data.Int
import Data.Word
import Data.Ranged
import qualified Data.List as L
import qualified Data.Map as M
import Control.Arrow ((&&&),(***))
import Data.Sproto.Types
import qualified Data.ByteString.Lazy.Char8 as B

data FieldType = TYPE_DOUBLE | TYPE_FLOAT | TYPE_INT64 | TYPE_UINT64 | TYPE_INT32 | TYPE_FIXED64 | TYPE_FIXED32 | TYPE_BOOL | TYPE_STRING
               | TYPE_GROUP | TYPE_MESSAGE | TYPE_BYTES | TYPE_UINT32 | TYPE_ENUM | TYPE_SFIXED32 | TYPE_SFIXED64 | TYPE_SINT32 | TYPE_SINT64
                            deriving (Enum)

-- This function returns an Iso (Isomorphism) for converting fieldvalues to and
-- from wirevalues. We use Isos throughout this function.  The form
-- fieldConstructorIso >>> manipulation >>> wireConstructorIso, is used.
convertType :: FieldType -> Iso FieldValue WireValue
convertType t = case t of
  TYPE_DOUBLE -> Iso  (\(DoubleField x) -> x) DoubleField >>> convertDouble >>> w64
  TYPE_FLOAT  -> Iso   (\(FloatField x) -> x) FloatField  >>> convertFloat  >>> w32
  TYPE_BOOL   -> Iso    (\(BoolField x) -> x) BoolField   >>> convertBool   >>> wVar
  TYPE_STRING -> Iso  (\(StringField x) -> x) StringField >>> Iso B.pack B.unpack >>> wStr
  TYPE_MESSAGE -> Iso  (\(BytesField x) -> x) BytesField  >>> wStr
  TYPE_BYTES   -> Iso  (\(BytesField x) -> x) BytesField  >>> wStr
  TYPE_SINT32   -> fieldInt >>> zigzag     >>> wVar
  TYPE_SINT64   -> fieldInt >>> zigzag     >>> wVar
  TYPE_FIXED32  -> fieldInt >>> convertInt >>> w32
  TYPE_SFIXED32 -> fieldInt >>> convertInt >>> w32
  TYPE_FIXED64  -> fieldInt >>> convertInt >>> w64
  TYPE_SFIXED64 -> fieldInt >>> convertInt >>> w64
  --TYPE_ENUM --initially stored as an IntegralField
  --TYPE_GROUP
  _ -> fieldInt >>> convertInt >>> wVar
 where w32 = Iso Wire32     $ \(Wire32 x) -> x
       w64 = Iso Wire64     $ \(Wire64 x) -> x
       wVar= Iso WireVar    $ \(WireVar x) -> x
       wStr= Iso WireString $ \(WireString x) -> x
       fieldInt = Iso (\(IntegralField x) -> x) IntegralField
       convertInt = Iso fromIntegral fromIntegral
       convertBool = Iso (\x->if x then 1 else 0) (>0)

readField :: FieldType -> (String -> FieldValue)
readField = case t of
  TYPE_DOUBLE -> DoubleField . read
  TYPE_FLOAT  -> FloatField  . read
  TYPE_BOOL   -> BoolField   . (\x -> if x == "true" then True else False)
  TYPE_STRING -> StringField
  TYPE_BYTES  -> BytesField  . read
  _ -> IntegralField . read

data FieldType = CustomTyp  String --indicates which ProtoMessage type, not the default value
               | EnumTyp    String String
               | DoubleTyp  Double
               | FloatTyp   Float
               | Int32Typ   Int32
               | Int64Typ   Int64
               | SInt32Typ  Int32
               | SInt64Typ  Int64
               | Fixed32Typ Int32
               | Fixed64Typ Int64
               | BoolTyp    Bool
               | StringTyp  String
               | BytesTyp   B.ByteString
  deriving (Eq, Show)

data FieldRule = Required | Optional | Repeated
  deriving (Eq, Enum, Show)

data ProtoField = ProtoField { fieldRule :: FieldRule, fieldName :: String, fieldId :: WireId, fieldType :: FieldType }
  deriving (Eq, Show)
data Proto = ProtoMessage String [ProtoField] [Proto] (RSet WireId)
           | ProtoExtension String [ProtoField] [Proto]
           | ProtoEnum String [(Int, String)]
 deriving (Eq, Show)

name (ProtoMessage s _ _ _) = s
name (ProtoExtension s _ _) = s
name (ProtoEnum s _) = s

-- This seems pretty messy...
absoluteNames :: String -> [String] -> [Proto] -> [Proto]
absoluteNames pre env ps = map process ps
  where names = map ((pre++).name) ps ++ env
        process (ProtoMessage s f p e) = ProtoMessage (pre++s) (map processF f) (absoluteNames (pre++s++".") names p) e
          where processF (ProtoField a b c (CustomTyp n)) = ProtoField a b c (CustomTyp newName)
                  where newName = maybe n (id) $ L.find (\x -> n == x || ('.':n) `L.isSuffixOf` x) names
                processF x = x
        process (ProtoEnum s f) = ProtoEnum (pre++s) f

zipT f g (x1, y1) (x2, y2) = (f x1 x2, g y1 y2)

type DefDicts = (M.Map String (M.Map WireId ProtoField), M.Map String (M.Map Int String))

toDicts :: [Proto] -> DefDicts
toDicts = (M.fromList *** M.fromList) . concatT . map denest . absoluteNames "" []
  where denest :: Proto -> ([(String, M.Map WireId ProtoField)], [(String, M.Map Int String)])
        denest (ProtoMessage s f c _) =  concatT $ ([(s, fieldMap)], []) : map denest c
          where fieldMap = M.fromList $ map (fieldId &&& id) f
        denest (ProtoEnum s f) = ([], [(s, M.fromList f)])
        concatT = foldl1 (\(x1, y1) (x2, y2) -> (x1++x2, y1++y2))
