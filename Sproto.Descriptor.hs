module Data.Sproto.Descriptor where
import Data.Int
import Data.Word
import Data.Ranged
import qualified Data.List as L
import qualified Data.Map as M
import Control.Arrow ((&&&),(***))
import Data.Sproto.Types

data FieldType = CustomTyp  String --indicates which message type, not the default value
               | EnumTyp    String Int32
               | DoubleTyp  Double
               | FloatTyp   Float
               | Int32Typ   Int32
               | Int64Typ   Int64
               | UInt32Typ  Word32
               | UInt64Typ  Word64
               | SInt32Typ  Int32
               | SInt64Typ  Int64
               | Fixed32Typ Int32
               | Fixed64Typ Int64
               | BoolTyp    Bool
               | StringTyp  String
               | BytesTyp
  deriving (Eq, Show)

data FieldRule = Required | Optional | Repeated
  deriving (Eq, Enum, Show)

data MessageField = MessageField { fieldRule :: FieldRule, fieldName :: String, fieldId :: WireId, fieldType :: FieldType }
  deriving (Eq, Show)
data Proto = Message String [MessageField] [Proto] (RSet WireId)
           | Extension String [MessageField] [Proto]
           | Enum String [(Int, String)]
 deriving (Eq, Show)

instance DiscreteOrdered WireId where
    adjacent x y = adjacent (toInteger x) (toInteger y)
    adjacentBelow 1 = Nothing
    adjacentBelow x = Just (x - 1)

name (Message s _ _ _) = s
name (Extension s _ _) = s
name (Enum s _) = s

-- This seems pretty messy...
absoluteNames :: String -> [String] -> [Proto] -> [Proto]
absoluteNames pre env ps = map process ps
  where names = map ((pre++).name) ps ++ env
        process (Message s f p e) = Message (pre++s) (map processF f) (absoluteNames (pre++s++".") names p) e
          where processF (MessageField a b c (CustomTyp n)) = MessageField a b c (CustomTyp newName)
                  where newName = maybe n (id) $ L.find (\x -> n == x || ('.':n) `L.isSuffixOf` x) names
                processF x = x
        process (Enum s f) = Enum (pre++s) f

zipT f g (x1, y1) (x2, y2) = (f x1 x2, g y1 y2)

type DefDicts = (M.Map String (M.Map WireId MessageField), M.Map String (M.Map Int String))

toDicts :: [Proto] -> DefDicts
toDicts = (M.fromList *** M.fromList) . concatT . map denest . absoluteNames "" []
  where denest :: Proto -> ([(String, M.Map WireId MessageField)], [(String, M.Map Int String)])
        denest (Message s f c _) =  concatT $ ([(s, fieldMap)], []) : map denest c
          where fieldMap = M.fromList $ map (fieldId &&& id) f
        denest (Enum s f) = ([], [(s, M.fromList f)])
        concatT = foldl1 (\(x1, y1) (x2, y2) -> (x1++x2, y1++y2))

