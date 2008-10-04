module Data.Sproto.HaskellGen () where
import Data.Sproto.Descriptor
import Data.Sproto.Types
import qualified Data.Map as M
import Data.List (intersperse)
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper, ord)
import Data.Bits ((.|.), shiftL)

seperateWith :: String -> [String] -> String
seperateWith x = concat . intersperse x

protosToHaskell :: Protos -> String
protosToHaskell es = prefix ++ (seperateWith "\n\n" $ map (\(str, x) -> either (messageToHaskell str) (enumToHaskell str) x) (M.toList es))
  where prefix = "import qualified Data.ByteString.Lazy.Char8 as B\n\
                 \import qualified Data.Binary as Bin\n\
                 \import qualified Data.Binary.Get as BinG\n\
                 \import qualified Data.Sproto.Binary as SprotoB\n\n\
                 \class Default a where\n\
                 \    def :: a\n\n"
--todo: figure out what to do about identical record names.
--todo: handle required fields properly (figure out what this is)

munge c = prefixDisallowed . capitalize . concatMap numChar
  where capitalize (x:xs) | isAlpha x = (if c then toUpper else toLower) x : xs
                          | otherwise = "Proto" ++ (x:xs)
        prefixDisallowed x = if x `elem` disallowed then "Proto" ++ x else x
        disallowed = words "as case of class data defualt deriving do forall foreign hiding if then else import infix infixl infixr instance let in mdo module newtype qualified type where"
        numChar x | not $ isAlphaNum x = "x" ++ show (ord x)
        numChar x = [x]

enumToHaskell :: String -> IsoMap String Int -> String
enumToHaskell str fields = unwords $ ["data", name, "=", name] ++ intersperse "|" (map (munge True) $ isoAs fields) ++ ["deriving (Show, Enum)"]
  where name = munge True str


repeated = (==Repeated) . protoLabel

fieldId x = ix
  where (WireId ix) = protoNumber x

fieldDef x = if null d then
    if repeated x then "[]" else 
     case protoType x of
      TYPE_BOOL -> "False"
      TYPE_STRING -> "\"\""
      TYPE_BYTES -> "B.empty"
      --errors on non-covered / integral types
      _ -> "0"
   else d
  where d = protoDefault x

messageToHaskell :: String -> [FieldProto] -> String
messageToHaskell str fields = unlines . map unwords $
    [["data", name, "=", name, "{"] ++ concat (intersperse [","] $ map fieldRecord fields ++ [["unknown :: [ WireField ] }"]]),
     ["instance Default", name, "where"],
     ["    def = ", name] ++ map fieldDef fields ++ ["[]"],
     ["instance Bin.Binary", name, "where"],
    -- ["    put = fast"],
     ["    get =", omitIfNone . fastName . head $ fields, "def"],
     [omitIfNone "      where slow x v = case x of"]
    ] ++ map slowCase fields ++
    [["               "]] ++ fastCases fields ++
	["    put v =" : if null fields then ["return ()"] else (concat $ intersperse [">>"] (map writeOut fields))]
  where name = munge True str
        omitIfNone x = if null fields then [] else x
        fieldRecord x = [munge False $ protoName x, if repeated x then ":: [" else "::",
          case protoType x of
            TYPE_MESSAGE -> munge True (protoTypeName x)
            TYPE_ENUM -> munge True (protoTypeName x)
            TYPE_DOUBLE -> "Double"
            TYPE_FLOAT -> "Float"
            TYPE_BOOL -> "Bool"
            TYPE_STRING -> "String"
            TYPE_BYTES -> "B.ByteString"
            _ -> "Integer"
         , if protoLabel x == Repeated then "]" else ""]
        slowCase x = ["               ", show $ fbyte x, "->"] ++
          (\(g,p)->[g,">>= \\y -> return $ ",p]) (readType x)
        fastName x = "fast" ++ show (fieldId x)
        fastCases [] = []
        fastCases (x:xs) = (["           ", fastName x, "v = BinG.isEmpty >>= \\e -> if e then return v else BinG.getWord8 >>= \\x -> if x ==", show $ fbyte x, "then ("] ++
            (\(g,p)->[g,">>= \\y ->"] ++ if null xs then ["return $", p]
                                                    else [fastName . head $ xs, "$", p]
            ) (readType x) ++ [") else (", "slow x v >>=", fastName x, ")"]
          ) : fastCases xs
        readType x = (g, "v { " ++ munge False (protoName x) ++ " = " ++ p ++ (if repeated x then " : " ++ munge False (protoName x) ++ " v" else "") ++ " }")
          where (g,p) = case protoType x of
                  --EnumTyp
                  TYPE_MESSAGE -> ("SprotoB.getBytes", "decode y")
                  TYPE_STRING  -> ("SprotoB.getBytes", "B.unpack y")
                  TYPE_BYTES   -> ("SprotoB.getBytes", "y")
                  TYPE_DOUBLE  -> ("BinG.getWord64be", "SprotoB.castWord64ToDouble y")
                  TYPE_FIXED64 -> ("BinG.getWord64be", "y")
                  TYPE_FLOAT   -> ("BinG.getWord32be", "SprotoB.castWord32ToFloat y")
                  TYPE_FIXED32 -> ("BinG.getWord32be", "y")
                  _            -> ("SprotoB.getVarInt", "y")
        writeOut x = ["BinG.putWord8", show (fbyte x), ">>", writeType x, ">> (mapM_ put . unknown $ v)"]
        writeType x = "(" ++
          (case protoType x of
            TYPE_MESSAGE -> "SprotoB.putBytes . encode"
            TYPE_STRING -> "SprotoB.putBytes . B.pack"
            TYPE_BYTES -> "SprotoB.putBytes"
            TYPE_DOUBLE -> "BinG.putWord64be . SprotoB.castDoubleToWord64"
            TYPE_FIXED64 -> "BinG.putWord64be"
            TYPE_FIXED32 -> "BinG.putWord32be"
            TYPE_FLOAT -> "BinG.putWord32be . SprotoB.castFloatToWord32"
            _ -> "SprotoB.putVarInt") ++ " . " ++ munge False (protoName x) ++ " $ v)"
        fbyte x = fieldId x `shiftL` 3 .|. 
            case protoType x of
              TYPE_MESSAGE -> 2
              TYPE_STRING  -> 2
              TYPE_BYTES   -> 2
              TYPE_DOUBLE  -> 1
              TYPE_FIXED64 -> 1
              TYPE_FLOAT   -> 5
              TYPE_FIXED32 -> 5
              _            -> 0