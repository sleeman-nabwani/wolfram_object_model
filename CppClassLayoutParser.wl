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
fields (fieldName, typeString).";


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
  |>.";
  
  
DrawClassDiagram::usage = 
  "DrawClassDiagram[layout_Association] takes the output of \
ComputeClassLayout and draws a two-panel diagram (vtable on the left, \
object layout + ruler on the right).";


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
		                                 (#1["kind"] === "FieldDecl") &]],
		          (* \[HorizontalLine]\[HorizontalLine]\[HorizontalLine] collect all virtual methods in this class \[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
		          "VTableEntries" ->
				    Map[
				      Lookup[#,"name"]&,
				      Select[
				        Lookup[node,"inner",{}],
				        #["kind"] === "CXXMethodDecl" &&
				        Lookup[#,"virtual", False] === True &
				      ]
				    ]                             
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


ClearAll[ComputeClassLayout];
ComputeClassLayout[classes_Association, className_String] := Module[
  {
    cls, nonV, virtV, rec,
    subobjs = {}, offs = 0, maxAlign = 1,
    inherited, own, allEntries,
    appendSubobj,
    totalSize, fieldTypes, totalFieldSize, fieldAlign
  },

  (*\[LongDash]\[LongDash] sanity check \[LongDash]\[LongDash]*)
  If[! KeyExistsQ[classes, className],
    Message[ComputeClassLayout::noClass, className];
    Return[$Failed]
  ];
  cls   = classes[className];
  nonV  = Select[cls["Bases"], ! #[[2]] &];
  virtV = Select[cls["Bases"],    #[[2]] &];
  rec   = ComputeClassLayout;

  (*\[LongDash]\[LongDash] helper to append one subobject \[LongDash]\[LongDash]*)
  appendSubobj[name_, kind_, sz_, al_, opts___] := Module[
    {aOff = Ceiling[offs/al]*al, entry},
    offs = aOff;
    entry = <|
      "Name"   -> name,
      "Kind"   -> kind,
      "Offset" -> offs,
      "Size"   -> sz
    |>;
    Scan[(entry[#1[[1]]] = #1[[2]])&, {opts}];
    AppendTo[subobjs, entry];
    offs += sz;
    maxAlign = Max[maxAlign, al];
  ];

  (*\[LongDash]\[LongDash] 1) pull in inherited vtable entries (already qualified) \[LongDash]\[LongDash]*)
  inherited = Flatten[
    rec[classes, #[[1]]]["VTableEntries"] & /@ virtV
  ];

  (*\[LongDash]\[LongDash] 2) qualify this class\[CloseCurlyQuote]s own vmethods \[LongDash]\[LongDash]*)
  own = Map[
    className <> "::" <> # &,
    Lookup[cls, "VTableEntries", {}]
  ];

  (*\[LongDash]\[LongDash] 3) splice + override by method name \[LongDash]\[LongDash]*)
  allEntries = Module[{joined = Join[inherited, own]},
    Reverse @ DeleteDuplicatesBy[
      Reverse @ joined,
      Last @ StringSplit[#, "::"] &
    ]
  ];

  (*\[LongDash]\[LongDash] 4) vptr slot \[LongDash]\[LongDash]*)
  If[Length[allEntries] > 0,
    appendSubobj[
      "vptr", "Vptr", 8, 8,
      "ClassOfVtable" -> className,
      "VTableEntries" -> allEntries
    ];
  ];

  (*\[LongDash]\[LongDash] 5) virtual-base pointer slots \[LongDash]\[LongDash]*)
  Scan[
    With[{b = #[[1]]},
      appendSubobj[
        "vbase:" <> b, "VbaseSlot",
        8, 8,
        "IsVirtualBase" -> True,
        "VBaseOf"       -> b
      ]
    ] &,
    virtV
  ];

  (*\[LongDash]\[LongDash] 6) non-virtual bases \[LongDash]\[LongDash]*)
  Scan[
    With[{b = #[[1]], sl = rec[classes, #[[1]]]},
      appendSubobj[
        b, "NonVirtualBase",
        sl["TotalSize"], sl["MaxAlign"]
      ]
    ] &,
    nonV
  ];

  (*\[LongDash]\[LongDash] 7) this class\[CloseCurlyQuote]s own field+slot region \[LongDash]\[LongDash]*)
  fieldTypes     = cls["Fields"][[All, 2]];
  totalFieldSize = Total[typeSize /@ fieldTypes] + 8*Length[allEntries];
  fieldAlign     = If[fieldTypes === {}, 1, Max[typeAlign /@ fieldTypes]];
  appendSubobj[
    className, "Class",
    totalFieldSize,
    Max[fieldAlign, 8]
  ];

  (*\[LongDash]\[LongDash] 8) actual virtual-base subobject(s) at end \[LongDash]\[LongDash]*)
  Scan[
    With[{b = #[[1]], sl = rec[classes, #[[1]]]},
      appendSubobj[
        b, "VirtualBase",
        sl["TotalSize"], sl["MaxAlign"],
        "IsVirtualBase" -> True
      ]
    ] &,
    virtV
  ];

  (*\[LongDash]\[LongDash] finalize & return \[LongDash]\[LongDash]*)
  totalSize = Ceiling[offs/maxAlign]*maxAlign;
  <|
    "ClassName"     -> className,
    "Subobjects"    -> subobjs,
    "TotalSize"     -> totalSize,
    "MaxAlign"      -> maxAlign,
    "VTableEntries" -> allEntries
  |>
];



ClearAll[DrawClassDiagram];
DrawClassDiagram[layout_Association] := Module[
  {
    subobjs   = layout["Subobjects"],
    vtbl      = layout["VTableEntries"],
    totalSize = layout["TotalSize"],

    (* geometry parameters *)
    objHeight = 1,                   (* fixed height of object panel *)
    objWidth  = layout["TotalSize"], (* horizontal bytes *)
    slotH     = 1,                   (* height per vtable slot *)
    vtabW     = 0.6,                 (* width of vtable panel *)
    sp        = 0.5,                 (* horizontal spacing *)

    x2, ptrEntry, vtabHeight
  },

  (* compute where vtable panel starts *)
  vtabHeight = Length[vtbl]*slotH;
  x2 = objWidth + sp;

  (* find the vptr subobject, if present *)
  ptrEntry = SelectFirst[subobjs, #["Kind"]==="Vptr"&, None];

  Graphics[
    {
      (* \[HorizontalLine]\[HorizontalLine]\[HorizontalLine] Object panel \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
      FaceForm[None], EdgeForm[Black],
      (* outer box *)
      Rectangle[{0, 0}, {objWidth, objHeight}],
      (* each subobject *)
      Table[
        {
          EdgeForm[GrayLevel[0.7]], FaceForm[None],
          Rectangle[
            {sub["Offset"], 0},
            {sub["Offset"] + sub["Size"], objHeight}
          ],
          Text[
            sub["Name"],
            {sub["Offset"] + sub["Size"]/2, objHeight/2}
          ]
        },
        {sub, subobjs}
      ],

      (* \[HorizontalLine]\[HorizontalLine]\[HorizontalLine] VTable panel \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
      FaceForm[None], EdgeForm[Black],
      (* outer box *)
      Rectangle[{x2, 0}, {x2+vtabW, vtabHeight}],
      (* each vtable entry *)
      Table[
        Text[
          vtbl[[i]],
          {
            x2 + vtabW/2,
            vtabHeight - (i - 0.5)*slotH
          }
        ],
        {i, Length[vtbl]}
      ],

      (* \[HorizontalLine]\[HorizontalLine]\[HorizontalLine] Arrow from vptr \[RightArrow] vtable midpoint \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
      If[ptrEntry =!= None,
        Arrow[{
          (* start at center of vptr rectangle *)
          { ptrEntry["Offset"] + ptrEntry["Size"]/2, objHeight/2 },
          (* end at center of vtable panel *)
          { x2, vtabHeight/2 }
        }],
        {}
      ]
    },

    Axes      -> False,
    Frame     -> False,
    PlotRange -> {
      {0, x2 + vtabW},
      {0, Max[objHeight, vtabHeight]}
    }
  ]
];



(*(**)
(* Interactive helpers *)
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
];*)


End[];  (* `Private` *)

EndPackage[];

