module Data.Sproto.Descriptor.Proto (descriptorProto, readDescriptorSet) where
import Data.Ranged
import Data.Sproto.Descriptor
import Data.Sproto
import Data.Sproto.Types
import Data.Maybe
import qualified Data.Map as M

msg n nest xs = ProtoMessage n (map (\(ix, (r, t, n)) -> ProtoField r n (fromIntegral ix) t) $ zip [1..] xs) nest rSetEmpty
cust = CustomTyp
enu = EnumTyp
stri = StringTyp ""
numb = Int32Typ 0
field x y z = (x, y, z)

data FileDescriptorProto = FileDescriptorProto String String [String] [DescriptorProto] [EnumDescriptorProto] [ServiceDescriptorProto] [FieldDescriptorProto] (Maybe FileOptions)
data DescriptorProto = DescriptorProto String [FieldDescriptorProto] [DescriptorProto] [EnumDescriptorProto] [ExtensionRange] [FieldDescriptorProto] (Maybe MessageOptions)
data DescriptorProto_ExtensionRange = DescriptorProto_ExtensionRange Integer Integer
data FieldDescriptorProto = FieldDescriptorProto String String Integer FieldDescriptorProto_Label (Maybe FieldDescriptorProto_Type) (Maybe String) (Maybe String) (Maybe FieldOptions)
data FieldDescriptorProto_Type = TYPE_DOUBLE | TYPE_FLOAT | TYPE_INT64 | TYPE_UINT64 | TYPE_INT32 | TYPE_FIXED64 | TYPE_FIXED32 | TYPE_BOOL | TYPE_STRING |
                               | TYPE_GROUP | TYPE_MESSAGE | TYPE_BYTES | TYPE_UINT32 | TYPE_ENUM | TYPE_SFIXED32 | TYPE_SFIXED64 | TYPE_SINT32 | TYPE_SINT64
                            deriving (Enum)
data FieldDescriptorProto_Label = LABEL_OPTIONAL | LABEL_REQUIRED | LABEL_REPEATED
                            deriving (Enum)
data EnumDescriptorProto = EnumDescriptorProto String [EnumValueDescriptorProto] (Maybe EnumOptions)
data EnumValueDescriptorProto = EnumValueDescriptorProto String Integer
data ServiceDescriptorProto = ServiceDescriptorProto String [MethodDescriptorProto] (Maybe ServiceOptions)
data MethodDescriptorProto = MethodDescriptorProto String String String (Maybe MethodOptions)
data FileOptions = FileOptions
data MessageOptions = MessageOptions
data EnumOptions = EnumOptions
data EnumValueOptions = EnumValueOptions
data ServiceOptions = ServiceOptions
data MethodOptions = MethodOptions

descriptorProto =
  [msg "FileDescriptorSet" []
    [field Repeated (cust "FileDescriptorProto") "file"
    ],
   msg "FileDescriptorProto" []
    [field Optional stri "name",
     field Optional stri "package",
     field Repeated stri "dependency",
     field Repeated (cust "DescriptorProto") "message_type",
     field Repeated (cust "EnumDescriptorProto") "enum_type",
     field Repeated (cust "ServiceDescriptorProto") "service",
     field Repeated (cust "FieldDescriptorProto") "extension",
     field Optional (cust "FileOptions") "options"
    ],
   msg "DescriptorProto"
    [msg "ExtensionRange" []
     [field Optional numb "start",
      field Optional numb "end"]
    ]
    [field Optional stri "name",
     field Repeated (cust "FieldDescriptorProto") "field",
     field Repeated (cust "DescriptorProto") "nested_type",
     field Repeated (cust "EnumDescriptorProto") "enum_type",
     field Repeated (cust "ExtensionRange") "extension_range",
     field Repeated (cust "FieldDescriptorProto") "extension",
     field Repeated (cust "MessageOptions") "options"
    ],
   msg "FieldDescriptorProto"
    [ProtoEnum "Type" [(1, "TYPE_DOUBLE"), (2, "TYPE_FLOAT"), (3, "TYPE_INT64"), (4, "TYPE_UINT64"), (5, "TYPE_INT32"), (6, "TYPE_FIXED64"), (7, "TYPE_FIXED32"), (8, "TYPE_BOOL"), (9, "TYPE_STRING"),
                       (10, "TYPE_GROUP"), (11, "TYPE_MESSAGE"), (12, "TYPE_BYTES"), (13, "TYPE_UINT32"), (14, "TYPE_ENUM"), (15, "TYPE_SFIXED32"), (16, "TYPE_SFIXED64"), (17, "TYPE_SINT32"), (18, "TYPE_SINT64")],
     ProtoEnum "Label" [(1, "LABEL_OPTIONAL"), (2, "LABEL_REQUIRED"), (3, "LABEL_REPEATED")]
    ]
    [field Optional stri "name",
     field Optional stri "extendee",
     field Optional numb "number",
     field Optional (enu "Label" "LABEL_OPTIONAL") "label",
     field Optional (enu "Type" "TYPE_DOUBLE") "type",
     field Optional stri "type_name",
     field Optional stri "default_value",
     field Optional (cust "FieldOptions") "field_options"
    ],
   msg "EnumDescriptorProto" []
    [field Optional stri "name",
     field Repeated (cust "EnumValueDescriptorProto") "value",
     field Optional (cust "EnumOptions") "options"
    ],
   msg "EnumValueDescriptorProto" []
    [field Optional stri "name",
     field Optional numb "number",
     field Optional (cust "EnumValueOptions") "options"
    ],
   msg "ServiceDescriptorProto" []
    [field Optional stri "name",
     field Repeated (cust "MethodDescriptorProto") "method",
     field Optional (cust "ServiceOptions") "options"
    ],
   msg "MethodDescriptorProto" []
    [field Optional stri "name",
     field Optional stri "input_type",
     field Optional stri "output_type",
     field Optional (cust "MethodOptions") "options"
    ],
   msg "FileOptions" [] [],
   msg "MessageOptions" [] [],
   msg "EnumOptions" [] [],
   msg "EnumValueOptions" [] [],
   msg "ServiceOptions" [] [],
   msg "MethodOptions" [] []
  ]

-- (B.readFile "Foo" >>= print . readDescriptorSet . readMsg (toDicts descriptorProto) "FileDescriptorSet" . decode)

children :: String -> Message -> [MemberValue]
children str (Message _ m _) = concat . maybeToList $ M.lookup str m

rmsgs y = (\xs -> [x | MessageMember x <- xs]) . children y
rmsg y = head . rmsgs y
rstris y = (\xs -> [x | StringMember x <- xs]) . children y
rstri y = head . rstris y
rnumbs y = (\xs -> [x | IntegralMember x <- xs]) . children y
rnumb y = head . rnumbs y
renums y = (\xs -> [x | EnumMember _ x <- xs]) . children y
renum y = head . renums y

readDescriptorSet :: Message -> [Proto]
readDescriptorSet = concatMap readFileDescriptor . rmsgs "file"

readFileDescriptor :: Message -> [Proto]
readFileDescriptor = map readDescriptorProto . rmsgs "message_type"

readDescriptorProto :: Message -> Proto
readDescriptorProto x =
  ProtoMessage (rstri "name" x)
               (map readFieldDescriptorProto $ rmsgs "field" x)
               ((map readDescriptorProto $ rmsgs "nested_type" x) ++ (map readEnumDescriptorProto $ rmsgs "enum_type" x))
               (makeRangedSet $ map readExtensionRange $ rmsgs "extension_range" x)

readExtensionRange :: Message -> Range WireId
readExtensionRange x = Range (BoundaryBelow . fromIntegral $ rnumb "start" x)
                             (BoundaryAbove . fromIntegral $ rnumb "end" x)

readFieldDescriptorProto :: Message -> ProtoField
readFieldDescriptorProto x = ProtoField
  (case renum "label" x of 1 -> Optional; 2 -> Required; 3 -> Repeated)
  (rstri "name" x)
  (fromIntegral $ rnumb "number" x)
  (case renum "type" x of
        1 -> DoubleTyp numeric
        2 -> FloatTyp numeric
        3 -> Int64Typ numeric
        4 -> Int64Typ numeric
        5 -> Int32Typ numeric
        6 -> Fixed64Typ numeric
        7 -> Fixed32Typ numeric
        8 -> BoolTyp (if def == "true" then True else False)
        9 -> StringTyp def --TODO: Group?
        11 -> CustomTyp type_name
        12 -> BytesTyp (read def)
        13 -> Int32Typ numeric
        14 -> EnumTyp type_name numeric --TODO: check default handling
        15 -> Fixed32Typ numeric
        16 -> Fixed64Typ numeric
        17 -> SInt32Typ numeric
        18 -> SInt64Typ numeric)
 where def = rstri "default_value" x
       numeric :: (Num a, Read a) => a
       numeric = if def == "" then 0 else read def
       type_name = rstri "type_name" x

readEnumDescriptorProto :: Message -> Proto
readEnumDescriptorProto x = ProtoEnum (rstri "name" x) (map readVal $ rmsgs "value" x)
  where readVal y = (fromIntegral $ rnumb "number" y, rstri "name" y)
