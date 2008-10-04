import Test.QuickCheck
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Sproto.Types
import Control.Arrow (second)
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B

data FieldProto = FieldProto { protoName :: String, protoNumber :: WireId, protoLabel :: FieldLabel, protoType :: FieldType, protoTypeName, protoDefault :: String }

instance Arbitrary EnumProto where
    arbitrary = do nums <- arbitrary
                   mapM (\x -> genIdent >>= return . (\y -> (y, x))) (nub.sort$nums)
instance Arbitrary FieldLabel where
    arbitrary = oneof $ map return $ [Optional, Required, Repeated]
instance Arbitrary FieldType where
    arbitrary = oneof $ map return $ [TYPE_DOUBLE, TYPE_FLOAT, TYPE_INT64, TYPE_UINT64, TYPE_INT32, TYPE_FIXED64, TYPE_FIXED32, TYPE_BOOL, TYPE_STRING
                                      TYPE_GROUP, TYPE_MESSAGE, TYPE_BYTES, TYPE_UINT32, TYPE_ENUM, TYPE_SFIXED32, TYPE_SFIXED64, TYPE_SINT32, TYPE_SINT64]

genIdent = arbitrary >>= return . filter (not isAlphaNum)
genFieldProto ens msgs ix = do name <- genIdent
                               num <- arbitrary
                               label <- arbitrary
                               typ <- arbitrary
                               let f = FieldProto name ix label typ
                               let fn = return . f "" . show
                               case typ of
                                 TYPE_DOUBLE -> arbitrary :: Gen Double >>= fn
                                 TYPE_FLOAT -> arbitrary :: Gen Float >>= fn
                                 TYPE_BOOL -> arbitrary :: Gen Bool >>= fn
                                 TYPE_STRING -> arbitrary :: Gen String >>= return . f ""
                                 TYPE_MESSAGE -> oneof $ map return $ msgs
                                 TYPE_ENUMS -> oneof $ map return $ ens
                                 TYPE_BYTES -> arbitrary :: Gen B.ByteString >>= return . f "" . show
                                 _ -> arbitrary :: Gen Int >>= fn --Hrmm, this might upset unsigned stuff

instance Arbitrary Protos where
    arbitrary = 

pickFromList :: [a] -> Gen a
pickFromList xs = do i <- choose (0, length xs - 1)
                     return (xs !! i)

randomSubset :: [a] -> Gen [a]
randomSubset inp = choose (0, length inp - 1) >> helper inp
    where helper xs n = do x <- pickFromList xs
                           rs <- helper (delete x xs) n-1
                           return x:rs

zipT :: (a -> b -> c) -> (a,b) -> c
zipT f (x, y) = f x y

pickRandomProto :: Protos -> Gen String
pickRandomProto = pickFromList . M.elems

genMessage :: Protos -> [FieldProto] -> Message
genMessage env = msg $ M.fromList $ map genMember $ zipT (++) $ second (randomSubset) $ partition ((==Required).protoLabel)
    where msg x = Message x []
    
genMember :: Protos -> FieldProto -> Gen (String, Member)
genMember env = sized . helper
  where helper pr n = (protoName,
            case protoLabel pr of
              Repeated -> mapM_ (const genField) [0..] >> return . RepeatedMember
              _        -> genField >> return . Member
           )
          where genField =
            case protoType pr of
              TYPE_DOUBLE  -> liftM DoubleValue arbitrary
              TYPE_FLOAT   -> liftM FloatValue arbitrary
              TYPE_BOOL    -> liftM BoolValue arbitrary
              TYPE_STRING  -> liftM StringValue arbitrary
              TYPE_BYTES   -> liftM BytesValue arbitrary
              TYPE_MESSAGE -> genMessage env $ lookupMessage env $ protoTypeName pr
              TYPE_ENUM    -> genEnumVal $ lookupEnum env $ protoTypeName pr
              _            -> liftM IntValue arbitrary

genEnumVal = uncurry EnumValue . pickFromList . isoToList

--Checks if there are any cycles among Required fields
mandatoryRecursion :: Protos -> Bool
mandatoryRecursion m = any checkLoop [] . M.fromList $ m
  checkLoop past (str, _) | str `elem` past = True
  checkLook past (str, Left xs) = any fieldLoop [x | (ProtoField _ _ Required TYPE_MESSAGE x _) <- xs]
    where fieldLoop x = let name = protoTypeName x in maybe False (\v -> checkLoop (str:past) (name, v)) (M.lookup name m)
  checkLoop _ = False