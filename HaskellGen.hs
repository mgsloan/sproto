module Data.Sproto.HaskellGen () where
import Data.Sproto.Descriptor
import Data.Sproto.Types
import qualified Data.Map as M
import Data.List (intersperse)
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper, ord)
import Data.Bits ((.|.), shiftL)

seperateWith :: String -> [String] -> String
seperateWith x = concat . intersperse x

defDictsToHaskell :: DefDicts -> String
defDictsToHaskell (ms, es) = prefix ++ (seperateWith "\n\n" $ map enumToHaskell (M.toList es) ++ map messageToHaskell (M.toList ms))
  where prefix = "import qualified Data.ByteString.Lazy.Char8 as B\n\
                 \import qualified Data.Binary as Bin\n\
                 \import qualified Data.Binary.Get as BinG\n\
                 \import qualified Data.Sproto.Binary as SprotoB\n\n\
                 \class Default a where\n\
                 \    def :: a\n\n"

munge c = capitalize . concatMap numChar
  where capitalize (x:xs) | isAlpha x = (if c then toUpper else toLower) x : xs
                          | otherwise = capitalize $ numChar x ++ xs
        numChar x | not $ isAlphaNum x = "x" ++ show (ord x)
        numChar x = [x]

enumToHaskell :: (String, M.Map Int String) -> String
enumToHaskell (str, fields) = unwords $ ["data", name, "=", name] ++ intersperse "|" (map (munge True) $ M.elems fields) ++ ["deriving (Show, Enum)"]
  where name = munge True str

messageToHaskell :: (String, M.Map WireId MessageField) -> String
messageToHaskell (str, m) = unlines . map unwords $
    [["data", name, "=", name, "{"] ++ concat (intersperse [","] $ map fieldRecord fields) ++ ["}"],
     ["instance Default", name, "where"],
     ["    def = ", name] ++ map fieldDefault fields,
     ["instance Bin.Binary", name, "where"],
    -- ["    put = fast"],
     ["    get =", fastName . head $ fields, "def"],
     ["      where slow v = BinG.getWord8 >>= \\x -> case x of"]
    ] ++ map slowCase fields ++ fastCases fields
  where fields = M.elems m
        name = munge True str
        fieldRecord x = [munge False $ fieldName x, if fieldRule x == Repeated then ":: [" else "::",
          case fieldType x of
            CustomTyp n -> munge True n
            EnumTyp n _ -> munge True n
            DoubleTyp _ -> "Double"
            FloatTyp _ -> "Float"
            BoolTyp _ -> "Bool"
            StringTyp _ -> "String"
            BytesTyp -> "B.ByteString"
            _ -> "Integer"
         , if fieldRule x == Repeated then "]" else ""]
        slowCase x = ["               ", show $ fbyte x, "->"] ++
          (\(g,p)->[g,">>= \\y -> return $ ",p]) (readType x)
        fastName x = let (WireId id) = fieldId x in "fast" ++ show id
        fastCases [] = []
        fastCases (x:xs) = (["           ", fastName x, "v = BinG.isEmpty >>= \\e -> if e then return v else BinG.getWord8 >>= \\x -> if x ==", show $ fbyte x, "then ("] ++
            (\(g,p)->[g,">>= \\y ->"] ++ if null xs then ["return $", p]
                                                    else [fastName . head $ xs, "$", p]
            ) (readType x) ++ [") else (", "slow v >>=", fastName x, ")"]
          ) : fastCases xs
        readType x = (g, "v { " ++ munge False (fieldName x) ++ " = " ++ p ++ (if fieldRule x == Repeated then " : " ++ munge False (fieldName x) ++ " v" else "") ++ " }")
          where (g,p) = case fieldType x of
                  --EnumTyp
                  CustomTyp  _ -> ("SprotoB.getBytes", "decode y")
                  StringTyp  _ -> ("SprotoB.getBytes", "B.unpack y")
                  BytesTyp     -> ("SprotoB.getBytes", "y")
                  DoubleTyp  _ -> ("BinG.getWord64be", "SprotoB.castWord64ToDouble y")
                  Fixed64Typ _ -> ("BinG.getWord64be", "y")
                  FloatTyp   _ -> ("BinG.getWord32be", "SprotoB.castWord32ToFloat y")
                  Fixed32Typ _ -> ("BinG.getWord32be", "y")
                  _            -> ("SprotoB.getVarInt", "y")
        fbyte x = fid `shiftL` 3 .|. 
            case fieldType x of
              CustomTyp   _ -> 2
              StringTyp   _ -> 2
              BytesTyp      -> 2
              DoubleTyp   _ -> 1
              Fixed64Typ  _ -> 1
              FloatTyp    _ -> 5
              Fixed32Typ  _ -> 5
              _             -> 0
          where (WireId fid) = fieldId x
        fieldDefault x = if fieldRule x == Repeated then "[]" else case fieldType x of
            StringTyp  v -> show v
            DoubleTyp  v -> show v
            Fixed64Typ v -> show v
            FloatTyp   v -> show v
            Fixed32Typ v -> show v
            --EnumTyp _ v  -> show v
            Int32Typ   v -> show v
            Int64Typ   v -> show v
            UInt32Typ  v -> show v
            UInt64Typ  v -> show v
            SInt32Typ  v -> show v
            SInt64Typ  v -> show v
