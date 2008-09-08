{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Sproto where
import Data.Binary (encode, decode)
import Data.Char (chr)
import Data.Function (on)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import qualified Data.Ranged as R
import qualified Data.List as L

import Data.Sproto.Types
import Data.Sproto.Descriptor
import Data.Sproto.Binary

data Message = Message String (M.Map String [MemberValue]) [WireValue] deriving Show

data MemberValue = MessageMember  Message
                 | EnumMember     String Integer String
                 | DoubleMember   Double
                 | FloatMember    Float
                 | IntegralMember Integer
                 | BoolMember     Bool
                 | StringMember   String
                 | BytesMember    B.ByteString
  deriving Show

data FieldValue = MessageField  String B.ByteString
                | EnumField     String Integer
                | DoubleField   Double
                | FloatField    Float
                | IntegralField Integer
                | BoolField     Bool
                | StringField   String
                | BytesField    B.ByteString

msg n xs = ProtoMessage n xs [] R.rSetEmpty
test x n = readMsg (toDicts x) n . decode . B.pack . map chr

writeMsg :: DefDicts -> Message -> WireValues
writeMsg env (Message name ms wv) = WireValues $ catMaybes (concatMap (\(str, fs) -> map (writeMember env $ getmsg str) fs) $ M.toList ms) ++ wv
  where protofields = M.toList $ fromJust $ M.lookup name $ fst env
        getmsg x = snd $ fromJust $ L.find ((==x) . fieldName . snd) protofields

writeMember :: DefDicts -> ProtoField -> MemberValue -> Maybe WireValue
writeMember env (ProtoField _ _ ix (CustomTyp _))  (MessageMember m)  = Just . WireString ix . encode $ writeMsg env m
writeMember env (ProtoField r _ ix (EnumTyp _ d))  (EnumMember _ x _) = if r == Optional && fromIntegral x == d then Nothing else Just . WireVar ix . fromIntegral $ x
writeMember env (ProtoField r _ ix (DoubleTyp d))  (DoubleMember x)   = if r == Optional && x == d              then Nothing else Just . Wire64 ix . castDoubleToWord64 $ x
writeMember env (ProtoField r _ ix (FloatTyp d))   (FloatMember x)    = if r == Optional && x == d              then Nothing else Just . Wire32 ix . castFloatToWord32 $ x
writeMember env (ProtoField r _ ix (Int32Typ d))   (IntegralMember x) = if r == Optional && fromIntegral x == d then Nothing else Just . WireVar ix . fromIntegral $ x
writeMember env (ProtoField r _ ix (Int64Typ d))   (IntegralMember x) = if r == Optional && fromIntegral x == d then Nothing else Just . WireVar ix . fromIntegral $ x
writeMember env (ProtoField r _ ix (SInt32Typ d))  (IntegralMember x) = if r == Optional && fromIntegral x == d then Nothing else Just . WireVar ix . zzEncode32 $ x
writeMember env (ProtoField r _ ix (SInt64Typ d))  (IntegralMember x) = if r == Optional && fromIntegral x == d then Nothing else Just . WireVar ix . zzEncode64 $ x
writeMember env (ProtoField r _ ix (Fixed32Typ d)) (IntegralMember x) = if r == Optional && fromIntegral x == d then Nothing else Just . Wire32 ix . fromIntegral $ x
writeMember env (ProtoField r _ ix (Fixed64Typ d)) (IntegralMember x) = if r == Optional && fromIntegral x == d then Nothing else Just . Wire64 ix . fromIntegral $ x
writeMember env (ProtoField r _ ix (BoolTyp d))    (BoolMember x)     = if r == Optional && x == d              then Nothing else Just . WireVar ix $ (if x then 1 else 0)
writeMember env (ProtoField r _ ix (StringTyp d))  (StringMember x)   = if r == Optional && x == d              then Nothing else Just . WireString ix . B.pack $ x
writeMember env (ProtoField _ _ ix (BytesTyp))     (BytesMember x)    = Just $ WireString ix x

merge :: [FieldValue] -> FieldValue
merge xs@((MessageField n _):_) = MessageField n . B.concat . map (\(MessageField _ x) -> x) $ xs
merge xs = last xs

lefts = catMaybes . map (\x -> case x of (Left x) -> Just x; _ -> Nothing)
rights = catMaybes . map (\x -> case x of (Right x) -> Just x; _ -> Nothing)

readMsg :: DefDicts -> String -> WireValues -> Message
readMsg env str (WireValues xs) = Message str (M.map (map $ fieldToMember env) . processFields fsd . lefts $ fields) (rights fields)
  where fsd = fromJust $ M.lookup str $ fst env
        fields = map (readField fsd) xs

--processFields :: M.Map WireId ProtoField -> [(WireId, FieldValue)] -> M.Map String [FieldValue]
processFields fsd xs = M.fromList . map (\x -> (fieldName x, processMember x . map snd . filter (\y -> fst y == fieldId x) $ xs)) . M.elems $ fsd
  where processMember (ProtoField Required _ _ _) [] = error "blargh!"
        processMember (ProtoField Required _ _ _) xs = [merge xs]
        processMember (ProtoField Optional _ _ t) [] = maybeToList $ defaultValue t
        processMember (ProtoField Optional _ _ _) xs = [merge xs]
        processMember (ProtoField Repeated _ _ _) xs = xs

toEither :: Maybe a -> b -> Either a b
toEither (Just a) _ = Left a
toEither Nothing x = Right x

wireId (WireVar ix _) = ix
wireId (Wire64 ix _) = ix
wireId (WireString ix _) = ix
wireId (Wire32 ix _) = ix

readField :: M.Map WireId ProtoField -> WireValue -> Either (WireId, FieldValue) WireValue
readField fs wireVal =
  let ix = wireId wireVal in
    toEither (do mf <- M.lookup ix fs
                 return (ix, readVal (fieldType mf) wireVal))
              wireVal

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
integ = IntegralField . fromIntegral

readVal :: FieldType -> (WireValue -> FieldValue)
readVal typ =
  case typ of
    CustomTyp str -> withString $ MessageField str
    EnumTyp str _ -> withVar $ EnumField str . fromIntegral
    DoubleTyp   _ -> with64 $ DoubleField . castWord64ToDouble
    FloatTyp    _ -> with32 $ FloatField . castWord32ToFloat
    SInt32Typ   _ -> withVar $ IntegralField . zzDecode32
    SInt64Typ   _ -> withVar $ IntegralField . zzDecode64
    Fixed32Typ  _ -> with32 integ
    Fixed64Typ  _ -> with64 integ
    BoolTyp     _ -> withVar $ BoolField . (>0)
    StringTyp   _ -> withString $ StringField . B.unpack
    BytesTyp      -> withString BytesField
    _             -> withVar integ

defaultValue :: FieldType -> Maybe FieldValue
defaultValue (EnumTyp n x)  = Just $ EnumField n $ fromIntegral x
defaultValue (DoubleTyp x)  = Just $ DoubleField x
defaultValue (FloatTyp x)   = Just $ FloatField x
defaultValue (Int32Typ x)   = Just $ integ x
defaultValue (Int64Typ x)   = Just $ integ x
defaultValue (SInt32Typ x)  = Just $ integ x
defaultValue (SInt64Typ x)  = Just $ integ x
defaultValue (Fixed32Typ x) = Just $ integ x
defaultValue (Fixed64Typ x) = Just $ integ x
defaultValue (BoolTyp x)    = Just $ BoolField x
defaultValue (StringTyp x)  = Just $ StringField x
defaultValue _              = Nothing

fieldToMember :: DefDicts -> FieldValue -> MemberValue
fieldToMember env (MessageField n v) = MessageMember . readMsg env n $ decode v
fieldToMember env (EnumField n v) = EnumMember n v $ fromJust (M.lookup n (snd env) >>= M.lookup (fromIntegral v))
fieldToMember env (DoubleField v) = DoubleMember v
fieldToMember env (FloatField v) = FloatMember v
fieldToMember env (IntegralField v) = IntegralMember v
fieldToMember env (BoolField v) = BoolMember v
fieldToMember env (StringField v) = StringMember v
fieldToMember env (BytesField v) = BytesMember v

