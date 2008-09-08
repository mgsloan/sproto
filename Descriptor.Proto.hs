module Data.Sproto.Descriptor.Proto (descriptorProto) where
import Data.Ranged
import Data.Sproto.Descriptor

msg n nest xs = ProtoMessage n (map (\(ix, (r, t, n)) -> ProtoField r n (fromIntegral ix) t) $ zip [1..] xs) nest rSetEmpty
cust = CustomTyp
enu = EnumTyp
stri = StringTyp ""
numb = Int32Typ 0
field x y z = (x, y, z)

descriptorProto =
  [msg "FileDescriptorProto" []
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
     field Optional (enu "Label" 0) "label",
     field Optional (enu "Type" 0) "type",
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
