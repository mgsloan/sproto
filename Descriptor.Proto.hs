module Data.Sproto.Descriptor.Proto where
import Data.Ranged
import Data.Binary
import Data.Sproto.Descriptor
import Data.Sproto.Types
import Data.Maybe
import Control.Arrow (first, second)
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M

stri = TYPE_STRING
numb = TYPE_INT32

field x y z = (x, y, "", z, "")
msgfi x y z = (x, TYPE_MESSAGE, y, z, "")
enufi x y z = (x, TYPE_ENUM, y, z, "")

msg n xs = (n, Left $ map (\(ix, (r, t, tn, n, df)) -> FieldProto n (fromIntegral ix) r t tn df) $ zip [1..] xs)
enu n xs = (n, Right . isoFromList $ zip xs [1..])

descriptorProtos = M.fromList
  [msg "FileDescriptorSet"
    [msgfi Repeated "FileDescriptorProto" "file"
    ],
   msg "FileDescriptorProto"
    [field Optional stri "name",
     field Optional stri "package",
     field Repeated stri "dependency",
     msgfi Repeated "DescriptorProto" "message_type",
     msgfi Repeated "EnumDescriptorProto" "enum_type",
     msgfi Repeated "ServiceDescriptorProto" "service",
     msgfi Repeated "FieldDescriptorProto" "extension",
     msgfi Optional "FileOptions" "options"
    ],
   msg "DescriptorProto_ExtensionRange"
    [field Optional numb "start",
     field Optional numb "end"],
   msg "DescriptorProto"
    [field Optional stri "name",
     msgfi Repeated "FieldDescriptorProto" "field",
     msgfi Repeated "DescriptorProto" "nested_type",
     msgfi Repeated "EnumDescriptorProto" "enum_type",
     msgfi Repeated "ExtensionRange" "extension_range",
     msgfi Repeated "FieldDescriptorProto" "extension",
     msgfi Repeated "MessageOptions" "options"
    ],
   enu "FieldDescriptorProto_Type"
    ["TYPE_DOUBLE", "TYPE_FLOAT", "TYPE_INT64", "TYPE_UINT64", "TYPE_INT32", "TYPE_FIXED64", "TYPE_FIXED32", "TYPE_BOOL", "TYPE_STRING",
     "TYPE_GROUP", "TYPE_MESSAGE", "TYPE_BYTES", "TYPE_UINT32", "TYPE_ENUM", "TYPE_SFIXED32", "TYPE_SFIXED64", "TYPE_SINT32", "TYPE_SINT64"],
   enu "FieldDescriptorProto_Label" ["Optional", "Required", "Repeated"],
   msg "FieldDescriptorProto"
    [field Optional stri "name",
     field Optional stri "extendee",
     field Optional numb "number",
     enufi Optional "FieldDescriptorProto_Label" "label",
     enufi Optional "FieldDescriptorProto_Type" "type",
     field Optional stri "type_name",
     field Optional stri "default_value",
     msgfi Optional "FieldOptions" "field_options"
    ],
   msg "EnumDescriptorProto"
    [field Optional stri "name",
     msgfi Repeated "EnumValueDescriptorProto""value",
     msgfi Optional "EnumOptions" "options"
    ],
   msg "EnumValueDescriptorProto"
    [field Optional stri "name",
     field Optional numb "number",
     msgfi Optional "EnumValueOptions" "options"
    ],
   msg "ServiceDescriptorProto"
    [field Optional stri "name",
     msgfi Repeated "MethodDescriptorProto" "method",
     msgfi Optional "ServiceOptions" "options"
    ],
   msg "MethodDescriptorProto"
    [field Optional stri "name",
     field Optional stri "input_type",
     field Optional stri "output_type",
     msgfi Optional "MethodOptions" "options"
    ],
   msg "FileOptions" [],
   msg "MessageOptions" [],
   msg "EnumOptions" [],
   msg "EnumValueOptions" [],
   msg "ServiceOptions" [],
   msg "MethodOptions" []
  ]

children s x =
  case member s x of
    Member x -> [x]
    RepeatedMember xs -> xs

rmsgs f y = (\xs -> [f x | MessageValue x <- xs]) . children y
rmsg y = head . rmsgs id y
rstris y = (\xs -> [x | StringValue x <- xs]) . children y
rstri y = head . rstris y
rnumbs y = (\xs -> [x | IntValue x <- xs]) . children y
rnumb y = head . rnumbs y
renums y = (\xs -> [read x | EnumValue x _ <- xs]) . children y
renum y = head . renums y

readDescriptor :: B.ByteString -> Protos
readDescriptor = M.fromList . concat . rmsgs readFileDescriptor "file" . readMsg descriptorProtos "FileDescriptorSet" . decode

data MessageDesc = MessageDesc String [FieldProto] [MessageDesc] [(String, EnumProto)] (RSet WireId)

readFileDescriptor x = readProtos "." [] (rmsgs readMessageDescriptor "message_type" x) (rmsgs readEnumDescriptor "enum_type" x)

-- flattens the nest hierarchy and resolves references to absolute names
readProtos :: String -> [String] -> [MessageDesc] -> [(String, EnumProto)] -> [(String, (Either MessageProto EnumProto))]
readProtos pre env msgs enus = (map (first prefix) $ map (second Right) enus ++ map (\(MessageDesc str fields _ _ _) -> (str, Left . map processF $ fields)) msgs) ++ recurse
  where names = env ++ [prefix x | (MessageDesc x _ _ _ _) <- msgs] ++ [prefix x | (x, _) <- enus]
        prefix str | "." `L.isPrefixOf` str = str
                   | otherwise = pre++str
        processF pr | protoType pr == TYPE_MESSAGE = pr { protoTypeName = newName . protoTypeName $ pr }
            where newName n = maybe n (id) $ L.find (('.':n) `L.isSuffixOf`) names
        processF pr = pr
        recurse = concatMap (\(MessageDesc name _ ms es _) -> readProtos (pre++name++".") names ms es) msgs

readMessageDescriptor :: Message -> MessageDesc
readMessageDescriptor x = MessageDesc
   (rstri "name" x)
   (rmsgs readFieldDescriptor "field" x)
   (rmsgs readMessageDescriptor "nested_type" x)
   (rmsgs readEnumDescriptor "enum_type" x)
   (makeRangedSet $ rmsgs readExtensionRange "extension_range" x)

readExtensionRange :: Message -> Range WireId
readExtensionRange x = Range (BoundaryBelow . fromIntegral $ rnumb "start" x)
                             (BoundaryAbove . fromIntegral $ rnumb "end" x)

readFieldDescriptor :: Message -> FieldProto
readFieldDescriptor x = FieldProto (rstri "name" x) (fromIntegral $ rnumb "number" x) (renum "label" x) (renum "type" x) (rstri "type_name" x) (rstri "default_value" x)

readEnumDescriptor :: Message -> (String, EnumProto)
readEnumDescriptor x = (rstri "name" x, isoFromList $ rmsgs readVal "value" x)
  where readVal y = (rstri "name" y, fromIntegral $ rnumb "number" y)