(* ::Package:: *)

(*
   CppClassLayoutParser.wl

   Provides:

     \[Bullet] ParseCppToClasses[path_String]    \[Dash] uses clang to dump a JSON AST, then
                                         builds an Association:
       <|
         "ClassName1" -> <|
             "AST"    -> <\[Ellipsis]raw AST node\[Ellipsis]>,
             "Bases"  -> { {"BaseA", False}, {"BaseB", True}, \[Ellipsis] },
             "Fields" -> { {"field1", "int"}, {"field2", "double"}, \[Ellipsis] }
           |>,
         "ClassName2" -> <| \[Ellipsis] |>,
         \[Ellipsis]
       |>

     \[Bullet] ComputeClassLayout[classes_Association, className_String]  
         \[Dash] walks that Association recursively and returns
       <|
         "Subobjects" -> { <|"Name"->\[Ellipsis],"Kind"->\[Ellipsis],"Offset"->\[Ellipsis],"Size"->\[Ellipsis],
                             (* optional keys: "ClassOfVtable", "IsVirtualBase" *) |>, \[Ellipsis] },
         "TotalSize"  -> totalBytes,
         "MaxAlign"   -> maxAlignment
       |>

     \[Bullet] DrawClassLayout[layout_Association]  
         \[Dash] turns the output of ComputeClassLayout[\[Ellipsis]] into a single Graphics
           showing each subobject\[CloseCurlyQuote]s rectangle, little \[OpenCurlyDoubleQuote]vptr\[CloseCurlyDoubleQuote]/\[OpenCurlyDoubleQuote]vbase\[CloseCurlyDoubleQuote] slots
           in red, and a tick\[Hyphen]mark ruler below.

     \[Bullet] ParseAndDrawClassLayout[ ] and ParseAndDrawClassLayout[path_String, className_String]
         \[Dash] interactive helpers for picking a file, a class, then invoking
           ComputeClassLayout<>DrawClassLayout.

   To use: put this file somewhere (e.g. \[OpenCurlyDoubleQuote]CppClassLayoutParser.wl\[CloseCurlyDoubleQuote]), then in
   Mathematica evaluate

     << "path/to/CppClassLayoutParser.wl";

   and call e.g.

     clsAssoc = ParseCppToClasses["MyHeader.hpp"];
     layoutX = ComputeClassLayout[clsAssoc, "X"];
     DrawClassLayout[layoutX];

   etc.
*)

BeginPackage["CppClassLayoutParser`"];

ParseCppToClasses::usage =
  "ParseCppToClasses[path_String] parses the given C++ header via clang's \
AST dump (JSON) and returns an Association of the form \
  <| \
    \"ClassA\" -> <| \
        \"AST\"    -> <\[Ellipsis]raw AST node\[Ellipsis]>, \
        \"Bases\"  -> { {\"Base1\", False}, {\"Base2\", True}, \[Ellipsis] }, \
        \"Fields\" -> { {\"fld1\", \"int\"}, {\"fld2\", \"double\"}, \[Ellipsis] } \
      |>, \
    \"Base1\" -> <| \[Ellipsis] |>, \
    \[Ellipsis] \
  |> \
Each class entry lists its immediate bases (name, isVirtual) and its data \
fields (fieldName, typeString)."

ComputeClassLayout::usage =
  "ComputeClassLayout[classes_Association, className_String] \
recursively computes the in-memory layout of className (including all \
non-virtual and virtual bases, vptr slots, vbase slots, and ordinary fields). \
Returns an Association: \
  <| \
    \"Subobjects\" -> { subobj1, subobj2, \[Ellipsis] }, \
    \"TotalSize\"  -> totalSizeInBytes, \
    \"MaxAlign\"   -> maxAlignment \
  |> \
where each subobj is an Association: \
  <| \
    \"Name\"         -> \"X\",       (* class or field name *) \
    \"Kind\"         -> \"NonVirtualBase\" or \"VirtualBase\" or \"Vptr\" or \
                       \"VbaseSlot\" or \"Field\", \
    \"Offset\"       -> offsetInBytes, \
    \"Size\"         -> sizeInBytes, \
    (* optional: if Kind->\"Vptr\", then \"ClassOfVtable\"->className *) \
    (* optional: if Kind->\"VirtualBase\", then \"IsVirtualBase\"->True *) \
  |>."

DrawClassLayout::usage =
  "DrawClassLayout[layout_Association] takes the output of \
ComputeClassLayout and draws a schematic rectangle for each subobject in \
the order they occupy memory, labeling each with its Name/Kind, small red \
\"vptr\" and \"vbase\" boxes at the left edge of each class subobject, \
and a byte-offset ruler along the bottom. Clicking on a \"vptr\" pops up \
a tooltip showing which class\[CloseCurlyQuote]s vtable lives there."

ParseAndDrawClassLayout::usage =
  "ParseAndDrawClassLayout[] opens a file-picker, lets you choose a C++ \
header, picks a class by name, and draws the layout. \
ParseAndDrawClassLayout[path_String, className_String] does the same \
programmatically."

Begin["`Private`"];


(* 1) typeSize / typeAlign helpers *)
typeSize["char"]   := 1;
typeSize["int"]    := 4;
typeSize["double"] := 8;
typeSize[_]        := 8;   (* default for any unknown type *)
typeAlign[t_]      := typeSize[t];


(* 2) ParseCppToClasses: run clang, get JSON, extract CXXRecordDecls *)
ParseCppToClasses[file_String] := Module[
  {out, json, nodes, classes},
  If[ !StringQ[file] || !FileExistsQ[file],
    Message[ParseCppToClasses::noFile, file]; 
    Return[$Failed]
  ];
  out = RunProcess[
    {
      "clang", "-x", "c++", "-std=c++17", "-fsyntax-only", 
      "-fno-color-diagnostics", "-Xclang", "-ast-dump=json", file
    },
    "StandardOutput"
  ];
  If[ !StringQ[out] || StringTrim[out] === "",
    Message[ParseCppToClasses::badAST, file]; 
    Return[$Failed]
  ];
  json = Quiet@Check[ ImportString[out, "RawJSON"], $Failed ];
  If[ !AssociationQ[json],
    Message[ParseCppToClasses::badAST, file]; 
    Return[$Failed]
  ];
  nodes = Cases[
	  json,
	  assoc_Association /; assoc["kind"] === "CXXRecordDecl",
	  Infinity
  ];
  classes = Association@Reap[
    Scan[
      Function[node,
	        If[
		      MatchQ[node, _Association] &&
		      node["kind"] === "CXXRecordDecl" &&
		      KeyExistsQ[node, "name"] &&
		      (* 1) skip purely implicit records: *)
		      !Lookup[node, "isImplicit", False] &&
		      (* 2) skip anything with no real \[OpenCurlyDoubleQuote]loc\[CloseCurlyDoubleQuote] info: *)
		      KeyExistsQ[node, "loc"] && node["loc"] =!= <||> &&
		      (* 3) only full definitions, not forward\[Hyphen]decls: *)
		      Lookup[node, "completeDefinition", False],
		      
		      Print["Class \"", node["name"], "\" location: ", Lookup[node, "loc", <||>]];
		      Sow[
		        node["name"] -> <|
		          "AST"    -> node,
		          "Bases"  -> Map[{Lookup[#1, "type", <|"qualType"->""|>]["qualType"],
		                           Lookup[#1, "isVirtual", False]} &,
		                         Lookup[node, "bases", {}]],
		          "Fields" -> Map[{#1["name"], #1["type"]["qualType"]} &,
		                          Select[Lookup[node, "inner", {}],
		                                 (#1["kind"] === "FieldDecl") &]]
		     |>
		   ]
	    ];
      ],
      nodes
    ]
  ][[2]];
  Print["\[RightArrow] FINAL classes keys: ",Keys[classes]];
  classes
];


(* 3) ComputeClassLayout: recursively assemble subobjects *)
ComputeClassLayout[classes_Association, className_String] := Module[
  {
    cls, nonV, virtV, subobjs = {}, offs = 0, maxAlign = 1,
    appendSubobj, totalSize, rec, hasPolymorphicBaseQ
  },
  
  Print["\[HorizontalLine]\[HorizontalLine]\[FilledRightTriangle] Enter ComputeClassLayout for: ", className];


  If[ !KeyExistsQ[classes, className],
    Message[ComputeClassLayout::noClass, className];
    Return[$Failed]
  ];
  cls = classes[className];

  (* Partition bases *)
  nonV  = Select[ cls["Bases"], Not[#[[2]]] & ];
  virtV = Select[ cls["Bases"], #[[2]] & ];
  
  Print["    non-virtual bases =", nonV, "; virtual bases =", virtV];

  rec = ComputeClassLayout;  (* recursive reference *)

  appendSubobj[name_, kind_, sz_, al_, opts___] := Module[
    {alignedOffs, entry},
    alignedOffs = Ceiling[offs/al]*al;
    offs = alignedOffs;
    entry = <|
      "Name"   -> name,
      "Kind"   -> kind,
      "Offset" -> offs,
      "Size"   -> sz
    |>;
    (* merge extra options if any *)
    Do[
       entry[k] = v,
       {{k, v}, {opts}}
    ];
    AppendTo[subobjs, entry];
    offs += sz;
    maxAlign = Max[maxAlign, al];
  ];

 (* 1st pass: non-virtual bases *)
  Scan[
    
    Function[{basePair},
    Print["    Processing non-virtual base: ", basePair[[1]]];
      Module[{bname, isVirt, sublayout},
        bname   = basePair[[1]];
        isVirt  = basePair[[2]];  (* currently always False in nonV *)
        sublayout = rec[classes, bname];
        appendSubobj[bname, NonVirtualBase,
                     sublayout["Size"],   
                     sublayout["Align"]
                     ];
      ]
    ],
    nonV
  ];
  (* 2nd pass: virtual bases (place at end of \[OpenCurlyDoubleQuote]base area\[CloseCurlyDoubleQuote]) *)
    Scan[
    Function[{basePair},
      Module[{bname, isVirt, sublayout},
        bname   = basePair[[1]];
        isVirt  = basePair[[2]];  (* currently always False in nonV *)
        sublayout = rec[classes, bname];
        appendSubobj[bname, NonVirtualBase,
                     "VirtualBase",
                     sublayout["Size"],   
                     sublayout["Align"],
                     "IsVirtualBase" -> True
                     ];
      ];
      Print["    \[RightArrow] Layout of base ", basePair[[1]], " was: ", rec[classes, basePair[[1]]]];
    ],
    nonV
  ];
 
  (* Determine \[OpenCurlyDoubleQuote]polymorphic\[CloseCurlyDoubleQuote] status: if there is any virtual base, we place a vptr *)
  hasPolymorphicBaseQ = Lookup[ cls["AST"], "isPolymorphic", False ];

  If[ hasPolymorphicBaseQ,
    appendSubobj["vptr", "Vptr", 8, 8, "ClassOfVtable" -> className];
  ];

  (* For each immediate virtual base, allocate an 8-byte vbase slot *)
  Scan[
    Function[{bname, isVirt},
      appendSubobj[
        "vbase:"<>bname,
        "VbaseSlot",
        8,
        8
      ];
    ],
    virtV
  ];

  (* Lay out this class\[CloseCurlyQuote]s own data fields in declaration order *)
	Scan[
	  Function[{pair},
	    Module[{fname = pair[[1]], ftype = pair[[2]], sz, al},
	      sz = typeSize[ftype];
	      al = typeAlign[ftype];
	      appendSubobj[fname, "Field", sz, al];
	    ]
	  ],
	  cls["Fields"]
	];

  (* Round up final size to maxAlign *)
  totalSize = Ceiling[offs/maxAlign]*maxAlign;
  Print["    FINAL subobjects for ", className, " \[RightArrow] ", subobjs];
  Print["    Computed Size = ", totalSize, ", Align = ", maxAlign];
  <|
    "Subobjects" -> subobjs,
    "TotalSize"  -> totalSize,
    "MaxAlign"   -> maxAlign
  |>
];


(* 4) DrawClassLayout: build a single Graphics *)
DrawClassLayout[layout_Association] := Module[
  {
    subobjs   = layout["Subobjects"],
    totalSize = layout["TotalSize"],
    maxAlign  = layout["MaxAlign"],
    maxCanvasWidth = 800,
    scale,
    rectangles = {},
    labels = {},
    tickMarks = {},
    y0 = 50,       (* y\[Hyphen]offset for the subobject row *)
    boxHeight = 60,
    tickHeight = 15,
    fontSizeField = 10,
    fontSizeClass = 16
  },
  (* 4.a) scale factor so totalSize \[RightArrow] maxCanvasWidth *)
  scale = maxCanvasWidth / totalSize;

  (* 4.b) for each subobject, draw a big rectangle + tiny vptr/vbase if needed + text *)
  Do[
    Module[
      {
        name   = sub["Name"],
        kind   = sub["Kind"],
        offs   = sub["Offset"],
        sz     = sub["Size"],
        isVB   = If[ KeyExistsQ[sub, "IsVirtualBase"], sub["IsVirtualBase"], False ],
        classV = If[ KeyExistsQ[sub, "ClassOfVtable"], sub["ClassOfVtable"], None ],
        xLeft, xRight, rect, inner
      },
      xLeft  = scale * offs;
      xRight = scale * (offs + sz);

      (* big enclosing rectangle *)
      rect = { Black, Rectangle[{xLeft, y0}, {xRight, y0 + boxHeight}] };
      AppendTo[rectangles, rect];

      (* tiny 8\[Times]8 vptr if needed *)
      If[kind === "Vptr",
        inner = {
          Red,
          EventHandler[
            {
              EdgeForm[Directive[Black, Thick]],
              FaceForm[LightRed],
              Rectangle[{xLeft, y0 + boxHeight - 8}, {xLeft + 8*scale, y0 + boxHeight}],
              Text[
                Style["vptr", Italic, 8, Red],
                {xLeft + 4*scale, y0 + boxHeight - 4},
                {0, 0}
              ]
            },
            {
              "MouseClicked" :> Tooltip[
                Style[
                  Column[{
                    Style["vptr \[RightArrow] " <> classV, Bold, 12],
                    Style["Offset: " <> ToString[offs] <> " bytes", 10]
                  }],
                  Background -> LightYellow
                ],
                Appearance -> Automatic,
                TooltipDelay -> 0
              ]
            }
          ]
        };
        AppendTo[labels, inner];
      ];

      (* tiny 8\[Times]8 vbase slot if needed *)
      If[kind === "VbaseSlot",
        inner = {
          Red,
          FaceForm[LightPink],
          EdgeForm[{Black, Thick}],
          Rectangle[{xLeft, y0 + boxHeight - 8}, {xLeft + 8*scale, y0 + boxHeight}],
          Text[
            Style["vbase", Italic, 8, Red],
            {xLeft + 4*scale, y0 + boxHeight - 4},
            {0, 0}
          ]
        };
        AppendTo[labels, inner];
      ];

      (* main label: if it's a Field, use black small; otherwise (class subobject),
         show class name in big red *)
      If[kind === "Field",
        inner = {
          Black,
          Text[
            Style[name, Plain, fontSizeField],
            { (xLeft + xRight)/2, y0 + boxHeight/2 },
            {0, 0}
          ]
        };
      ,
        inner = {
          Red,
          Text[
            Style[name, Bold, fontSizeClass],
            { (xLeft + xRight)/2, y0 + boxHeight/2 },
            {0, 0}
          ]
        };
      ];
      AppendTo[labels, inner];

    ],
    {sub, subobjs}
  ];

  (* 4.c) build tick marks every 8 bytes along the bottom *)
  Do[
    Module[{bx = scale * (8 n)},
      AppendTo[tickMarks, {
        {Black, Thick, Line[{{bx, y0 - 5}, {bx, y0 - tickHeight}}]},
        {Black,
         Text[
           Style[ToString[8 n], Plain, 10],
           {bx, y0 - tickHeight - 5},
           {0, 1}
         ]}
      }];
    ],
    {n, 0, Ceiling[totalSize/8]}
  ];

  (* 4.d) assemble final Graphics *)
  Graphics[
 {
  LightGray, Rectangle[{0, 0}, {4, 1}],     (* one 4-byte box *)
  Black,    Text["x", {2, 0.5}],            (* field name in center *)

  Thick,
  Line[{{0, 0}, {0, -0.2}}],  Text["0", {0, -0.4}],
  Line[{{4, 0}, {4, -0.2}}],  Text["4", {4, -0.4}]
 },
 Axes -> False,
 ImageSize -> 400,
 PlotRange -> {{-0.5, 4.5}, {-1, 1.5}}
]
];


(* 5) Interactive helpers *)
ParseAndDrawClassLayout[] := Module[{file, cls, names, choice, lay},
  file = SystemDialogInput["FileOpen"];
  If[ !StringQ[file] || !FileExistsQ[file], Return[$Canceled] ];
  cls = ParseCppToClasses[file];
  If[ cls === $Failed, Return[$Failed] ];
  names = Keys[cls];
  If[ names === {}, Message[ParseAndDrawClassLayout::noClasses]; Return[$Failed] ];
  choice = ChoiceDialog["Select class:", Thread[names -> names]];
  If[ !StringQ[choice], Return[$Canceled] ];
  lay = ComputeClassLayout[cls, choice]["Subobjects"];
  DrawClassLayout[<|"Subobjects"->lay, "TotalSize"->ComputeClassLayout[cls, choice]["TotalSize"], "MaxAlign"->ComputeClassLayout[cls, choice]["MaxAlign"]|>]
];

ParseAndDrawClassLayout[path_String, className_String] := Module[{cls, res},
  cls = ParseCppToClasses[path];
  If[ cls === $Failed, Return[$Failed] ];
  res = ComputeClassLayout[cls, className];
  DrawClassLayout[<|"Subobjects"->res["Subobjects"], "TotalSize"->res["TotalSize"], "MaxAlign"->res["MaxAlign"]|>]
];


(* 6) A small convenience wrapper *)
ComputeClassLayoutExample[path_String, className_String] := Module[{cls, res},
  cls = ParseCppToClasses[path];
  If[ cls === $Failed, Return[$Failed] ];
  res = ComputeClassLayout[cls, className];
  res
];


End[];  (* `Private` *)

EndPackage[];

