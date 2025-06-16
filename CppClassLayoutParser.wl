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


(* helper to find all virtual bases (direct + via non-virtual bases) *)
ClearAll[getVirtBases];
getVirtBases[classes_Association, className_String] := Module[
  {
    direct, nonv
  },
  direct = Cases[classes[className]["Bases"], {b_, True} :> b];
  nonv   = Cases[classes[className]["Bases"], {b_, False} :> b];
  DeleteDuplicates@Join[
    direct,
    Flatten[getVirtBases[classes, #] & /@ nonv]
  ]
];

(* main layout function *)
ClearAll[ComputeClassLayout];
ComputeClassLayout[classes_Association, className_String] := Module[
  {
    cls,
    dirPairs,            (* direct bases as {name, isVirt}? *)
    directNames,         (* {Y1, Y2, \[Ellipsis]} *)
    nonVirtualNames,
    virtBaseNames,       (* all virtual bases, incl. indirect *)
    rec,                 (* for recursion *)

    (* layout\[Hyphen]building *)
    subobjs = {}, offs = 0, maxAlign = 1,

    (* vtable assembly *)
    inheritedAll, inheritedUniq,
    ownQual, ownNames, inheritedFilt,
    allEntries,

    (* helper to record a subobject *)
    appendSubobj,

    (* field area *)
    fieldTypes, totalFieldSize, fieldAlign,
    totalSize
  },

  (* sanity check *)
  If[! KeyExistsQ[classes, className],
    Message[ComputeClassLayout::noClass, className];
    Return[$Failed]
  ];

  cls = classes[className];
  dirPairs       = cls["Bases"];
  directNames    = dirPairs[[All, 1]];
  nonVirtualNames= Cases[dirPairs, {b_, False} :> b];
  virtBaseNames  = getVirtBases[classes, className];
  rec            = ComputeClassLayout;  (* for recursive calls *)

  (* subobject appender *)
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

  (* 1) Build the VTableEntries by merging direct bases *)
  inheritedAll = Flatten[
    rec[classes, #]["VTableEntries"] & /@ directNames
  ];
  (* later bases override earlier *)
  inheritedUniq = Reverse @ DeleteDuplicatesBy[
    Reverse @ inheritedAll,
    Last @ StringSplit[#, "::"] &
  ];
  (* qualify this class\[CloseCurlyQuote]s own methods *)
  ownQual = Map[
    className <> "::" <> # &,
    Lookup[cls, "VTableEntries", {}]
  ];
  ownNames = Last @ StringSplit[#, "::"] & /@ ownQual;
  inheritedFilt = Select[
    inheritedUniq,
    ! MemberQ[ownNames, Last @ StringSplit[#, "::"]] &
  ];
  allEntries = Join[ownQual, inheritedFilt];

  (* 2) vptr slot *)
  If[Length[allEntries] > 0,
    appendSubobj[
      "vptr", "Vptr", 8, 8,
      "ClassOfVtable" -> className,
      "VTableEntries" -> allEntries
    ];
  ];

  (* 3) one vbase\[Hyphen]pointer per virtual base *)
  Scan[
    With[{b = #},
      appendSubobj[
        "vbase:" <> b, "VbaseSlot", 8, 8,
        "IsVirtualBase" -> True,
        "VBaseOf"       -> b
      ]
    ] &,
    virtBaseNames
  ];

  (* 4) flatten each direct non-virtual base *)
  Scan[
    With[{b = #, sl = rec[classes, #]},
      appendSubobj[
        b, "NonVirtualBase",
        sl["TotalSize"], sl["MaxAlign"]
      ]
    ] &,
    nonVirtualNames
  ];

  (* 5) this class\[CloseCurlyQuote]s own field + method\[Hyphen]slot region *)
  fieldTypes     = cls["Fields"][[All, 2]];
  totalFieldSize = Total[typeSize /@ fieldTypes] + 8*Length[allEntries];
  fieldAlign     = If[fieldTypes === {}, 1, Max[typeAlign /@ fieldTypes]];
  appendSubobj[
    className, "Class",
    totalFieldSize,
    Max[fieldAlign, 8]
  ];

  (* 6) actual virtual\[Hyphen]base subobject at end *)
  Scan[
    With[{b = #, sl = rec[classes, #]},
      appendSubobj[
        b, "VirtualBase",
        sl["TotalSize"], sl["MaxAlign"],
        "IsVirtualBase" -> True
      ]
    ] &,
    virtBaseNames
  ];

  (* finalize total size & return *)
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
Options[DrawClassDiagram] = {
  MaxObjectWidth -> 1000,  (* maximum width of the object bar in pixels *)
  SlotHeight      -> 20,   (* height of each vtable slot in pixels *)
  VTableWidth     -> 60,   (* width of the vtable panel in pixels *)
  GapHeight       -> 10,   (* vertical gap between object and vtable in pixels *)
  ArrowSize       -> 0.03  (* relative arrowhead size *)
};

DrawClassDiagram[
  layout_Association,
  opts : OptionsPattern[]
] := Module[
  {
    subobjs    = layout["Subobjects"],
    vtbl       = layout["VTableEntries"],
    totalSize  = layout["TotalSize"],
    (* pull options *)
    maxOW      = OptionValue[MaxObjectWidth],
    slotH      = OptionValue[SlotHeight],
    vW         = OptionValue[VTableWidth],
    gapH       = OptionValue[GapHeight],
    aSz        = OptionValue[ArrowSize],
    (* derived dims *)
    nSlots, objW, scale, yObj,
    ptr, ptrX
  },

  nSlots = Length[vtbl];

  (* 1) Compute object-bar width (capped at MaxObjectWidth) *)
  objW  = Min[maxOW, totalSize*slotH];
  scale = objW/totalSize;                 (* pixels per \[OpenCurlyDoubleQuote]byte\[CloseCurlyDoubleQuote] *)
  yObj  = nSlots*slotH + gapH;            (* y-coordinate of object bar *)

  (* 2) Locate the vptr slot for arrow origin *)
  ptr   = SelectFirst[subobjs, #["Kind"] === "Vptr"&, None];
  ptrX  = If[ptr =!= None,
            (ptr["Offset"] + ptr["Size"]/2)*scale,
            objW/2
          ];

  Graphics[
    {
      (* \[HorizontalLine]\[HorizontalLine] Object bar \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
      FaceForm[None], EdgeForm[Black],
      (* outer rectangle *)
      Rectangle[{0, yObj}, {objW, yObj + slotH}],
      (* each subobject *)
      Table[
        {
          EdgeForm[GrayLevel[0.7]], FaceForm[None],
          Rectangle[
            { sub["Offset"]*scale,         yObj},
            {(sub["Offset"] + sub["Size"])*scale, yObj + slotH}
          ],
          Text[
            sub["Name"],
            { (sub["Offset"] + sub["Size"]/2)*scale, yObj + slotH/2 }
          ]
        },
        {sub, subobjs}
      ],

      (* \[HorizontalLine]\[HorizontalLine] VTable panel \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
      FaceForm[None], EdgeForm[Black],
      (* outer rectangle *)
      Rectangle[{0, 0}, {vW, nSlots*slotH}],
      (* each slot & label *)
      Table[
        {
          EdgeForm[Black], FaceForm[None],
          Rectangle[
            {0,              slotH*(nSlots - i)},
            {vW,             slotH*(nSlots - i + 1)}
          ],
          Text[
            vtbl[[i]],
            {vW/2, slotH*(nSlots - i) + slotH/2}
          ]
        },
        {i, nSlots}
      ],

      (* \[HorizontalLine]\[HorizontalLine] Vertical arrow from vptr \[RightArrow] vtable mid \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
      Arrowheads[aSz],
      If[ptr =!= None,
        Arrow[{
          {ptrX,                yObj},
          {ptrX,                slotH*nSlots},
          {0.9*vW,              slotH*nSlots}   (* small horizontal kick *)
        }],
        {}
      ]
    },
    PlotRange   -> {{0, Max[objW, vW]}, {0, yObj + slotH}},
    ImageSize   -> {Max[objW, vW], Automatic},
    Axes        -> False,
    Frame       -> False
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

