(* ::Package:: *)

(*CppClassLayoutParser.wl A Wolfram Language application to: 
  1. Parse C++ class headers via Clang JSON AST 
  2. Compute object-layout subobjects (including virtual bases) 
  3. Render box diagrams showing subobject offsets 
Usage:
  1. Save this file (CppClassLayoutParser.wl) to a directory on your system.
  2. In the Wolfram Desktop or Cloud,open a new Notebook.
  3. Load the definitions by evaluating:<<"path/to/CppClassLayoutParser.wl" 
  4. Point to your C++ header,e.g.:classes.hpp 
  5. Run:classes=ParseCppToClasses["classes.hpp"];
		   layout=ComputeLayout[classes];
           DrawLayout[layout];
You can customize:-Compiler flags in ParseCppToClasses by editing the RunProcess call.
-Unit sizes or offsets in ComputeLayout if you want real byte sizes.
-Diagram styling in DrawLayout (e.g.colors,labels).*)
(*Public functions:ParseCppToClasses,ComputeLayout,DrawLayout*)


(* ::Package:: *)
(**)


BeginPackage["CppClassLayoutParser`"]

(*  \[HorizontalLine]\[HorizontalLine]\[HorizontalLine] Public symbols go here \[Dash] just the names \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]  *)
ParseCppToClasses::usage = 
  "ParseCppToClasses[file] returns an Association of classes.";
ComputeLayout::usage     = 
  "ComputeLayout[classes, name] gives <|\"Layout\"\[RightArrow]\[Ellipsis], \"TotalSize\"\[RightArrow]\[Ellipsis]|>.";
DrawLayout::usage        = 
  "DrawLayout[list] draws a box diagram.";
ParseAndDrawLayout::usage =
  "Interactive helper that combines the three steps.";

(*  \[HorizontalLine]\[HorizontalLine]\[HorizontalLine] Error messages \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]  *)
ParseCppToClasses::noFile = "File `1` not found.";
ParseCppToClasses::badAST = "clang could not produce a JSON AST for `1`.";
ComputeLayout::noClass    = "Class `1` not in the parsed association.";

Begin["`Private`"];

(* ---------------------------------------------------------------------------- *)
(* ParseCppToClasses                                                        *)
(* ---------------------------------------------------------------------------- *)
ParseCppToClasses[file_String] := Module[{out, json, nodes, classes},
  If[!StringQ[file] || !FileExistsQ[file],
    Message[ParseCppToClasses::noFile, file]; Return[$Failed]
  ];
  out = RunProcess[
    {"clang", "-x", "c++", "-std=c++17", "-fsyntax-only", "-fno-color-diagnostics",
     "-Xclang", "-ast-dump=json", file}, "StandardOutput"];
  If[!StringQ[out] || StringTrim[out] === "",
    Message[ParseCppToClasses::badAST, file]; Return[$Failed]
  ];
  json = Quiet @ Check[ ImportString[out, "RawJSON"], $Failed ];
  If[!AssociationQ[json],
    Message[ParseCppToClasses::badAST, file]; Return[$Failed]
  ];
  nodes = Lookup[json, "inner", {}];
  classes = Association @ Reap[
    Scan[Function[node,
      If[
        MatchQ[node, _Association] && node["kind"] === "CXXRecordDecl" &&
        KeyExistsQ[node, "name"] && !Lookup[node, "isImplicit", False],
         (* Print the raw `loc` field so we see exactly what path Clang gave us *)
      Print["Class \[OpenCurlyDoubleQuote]", node["name"], "\[CloseCurlyDoubleQuote] location: ", Lookup[node, "loc", {}] ];
      if[  
        With[{loc = Lookup[node, "loc", <||>]},
		  StringMatchQ[
		    Lookup[loc, "file", ""],
		    "*" <> FileNameTake[file]
		  ]
		],
        Sow[node["name"] -> <|
          "AST"    -> node,
		  "Bases" -> Map[
		      {
		        Lookup[#1, "type", <|"qualType"->""|>]["qualType"],
		        Lookup[#1, "isVirtual", False]
		      } &,
		      Lookup[node, "bases", {}]
		  ],
          "Fields" -> Map[{#1["name"], #1["type"]["qualType"]} &, 
                            Select[Lookup[node, "inner", {}], #1["kind"] === "FieldDecl" &]]
        |>]
        ]
      ]
    ], 
    nodes
    ];
  ][[2]];
  classes
];

(* ---------------------------------------------------------------------------- *)
(* ComputeLayout                                                             *)
(* ---------------------------------------------------------------------------- *)
ComputeLayout[classes_Association, className_String] := Module[{
    cls, nonV, virtV, layout = {}, offs = 0, maxAlign = 1,
    typeSize, typeAlign, addEntry, totalSize
  },
  typeSize["char"]   := 1; typeSize["int"] := 4; typeSize["double"] := 8; typeSize[_] := 8;
  typeAlign[t_] := typeSize[t];
  addEntry[name_, kind_, sz_, al_] := (
    offs = Ceiling[offs/al]*al;
    AppendTo[layout, <|
      "Name"   -> name,
      "Kind"   -> kind,
      "Offset" -> offs,
      "Size"   -> sz
    |>];
    offs += sz; maxAlign = Max[maxAlign, al];
  );
  If[!KeyExistsQ[classes, className],
    Message[ComputeLayout::noClass, className]; Return[$Failed]
  ];
  cls = classes[className];
  nonV   = Select[cls["Bases"], Not[#[[2]]] &];
  virtV  = Select[cls["Bases"], #[[2]] &];
  Scan[Function[b, addEntry[b[[1]], "NonVirtualBase",
    ComputeLayout[classes, b[[1]]]["TotalSize"],
    ComputeLayout[classes, b[[1]]]["TotalSize"]]], nonV];
  Scan[Function[b, addEntry[b[[1]], "VirtualBase",
    ComputeLayout[classes, b[[1]]]["TotalSize"],
    ComputeLayout[classes, b[[1]]]["TotalSize"]]], virtV];
  addEntry["vptr", "Vptr", 8, 8];
  Scan[Function[b, addEntry["vbase:"<>b[[1]], "VbaseSlot", 8, 8]], virtV];
  Scan[Function[f, addEntry[f[[1]], "Field", typeSize[f[[2]]], typeAlign[f[[2]]]]],
       cls["Fields"]];
  totalSize = Ceiling[offs/maxAlign]*maxAlign;
  <|"Layout"->layout, "TotalSize"->totalSize|>
];

(* ---------------------------------------------------------------------------- *)
(* DrawLayout                                                                 *)
(* ---------------------------------------------------------------------------- *)
ClearAll[DrawClassLayoutByKind];

(*
   DrawClassLayoutByKind takes a _raw_ layout list where each element is
   an association of the form:
     <|"Offset"->startByte, "Size"->classSize, "Name"->className, "Kind"->"class"|>
   It will then automatically split each class\[Hyphen]sized block into three pieces:
     1) a vptr slot  (pointer\[Hyphen]sized)   \[RightArrow] row "vptr"
     2) a vbase slot (pointer\[Hyphen]sized)\:2003  \[RightArrow] row "vbase"
     3) the remaining bytes            \[RightArrow] row "class"
   and then draw all of them in a three\[Hyphen]row horizontal bar chart.

   The only option you need to worry about is PointerSize (in bytes).
*)
Options[DrawClassLayoutByKind] = {PointerSize :> ($MachineWordLength/8)};

DrawClassLayoutByKind[
  rawLayout_List,
  OptionsPattern[]
] := Module[
  {
    ps,             (* pointer size in bytes *)
    expanded,       (* the \[OpenCurlyDoubleQuote]expanded\[CloseCurlyDoubleQuote] list after inserting vptr+vbase slots *)
    maxX,           (* farthest\[Hyphen]right X to set PlotRange *)
    yOfKind,        (* map from row\[Hyphen]name to Y\[Hyphen]coordinate *)
    allOffsets,     (* list of all offsets to put tick marks *)
    xTicks,         (* formatted tick list *)
    rects, texts,   (* the graphics primitives to draw *)
    yMin, yMax
  },

  (* 1) Determine how many bytes a pointer occupies on this machine. *)
  ps = OptionValue[PointerSize];

  (*
    2) We expect rawLayout to have exactly one association per \[OpenCurlyDoubleQuote]class\[CloseCurlyDoubleQuote]:
         <|"Offset" -> offsetOfClass, "Size" -> sizeOfClass, 
           "Name" -> className, "Kind" -> "class"|>
    We will break that \[OpenCurlyDoubleQuote]class\[CloseCurlyDoubleQuote]\[Hyphen]sized block into three sub\[Hyphen]blocks:
      \[Bullet] vptr  = the first ps bytes
      \[Bullet] vbase = the next ps bytes
      \[Bullet] class = the remaining (sizeOfClass \[Minus] 2 ps) bytes
    If the incoming \[OpenCurlyDoubleQuote]class\[CloseCurlyDoubleQuote] is smaller than 2 ps, it will still draw something,
    but typically you want all classes \[GreaterEqual] 2 ps in size.
  *)
  expanded = rawLayout // Flatten[{
    (* For each \[OpenCurlyDoubleQuote]class\[CloseCurlyDoubleQuote] entry, produce three pieces in one combined list *)
    Function[assoc,
      Module[{off, len, nm, remainder},
        off = assoc["Offset"];
        len = assoc["Size"];
        nm = assoc["Name"];
        remainder = len - 2 ps;

        {
          (* 2a) vptr block *)
          <|
            "Offset" -> off,
            "Size"   -> ps,
            "Name"   -> nm <> "\[CenterDot]vptr",
            "Kind"   -> "vptr"
          |>,

          (* 2b) vbase block *)
          <|
            "Offset" -> off + ps,
            "Size"   -> ps,
            "Name"   -> nm <> "\[CenterDot]vbase",
            "Kind"   -> "vbase"
          |>,

          (* 2c) class block (the \[OpenCurlyDoubleQuote]rest\[CloseCurlyDoubleQuote] of the object) *)
          <|
            "Offset" -> off + 2 ps,
            "Size"   -> Max[0, remainder],  (* guard against negative *)
            "Name"   -> nm,
            "Kind"   -> "class"
          |>
        }
      ]
    ],
    (* in principle, if rawLayout already contained vptr/vbase/class entries you
       could skip\[LongDash]but here we assume \[OpenCurlyDoubleQuote]rawLayout\[CloseCurlyDoubleQuote] is just a list of pure-"class" blocks. *)
    rawLayout
  }];

  (*
    3) After expansion, we have a list of small chunks, each with a \[OpenCurlyDoubleQuote]Kind\[CloseCurlyDoubleQuote] of
       "vptr", "vbase", or "class".  We now choose which vertical row they sit on.
    Row assignment:
      \[OpenCurlyDoubleQuote]vptr\[CloseCurlyDoubleQuote]  \[RightArrow] row 1
      \[OpenCurlyDoubleQuote]vbase\[CloseCurlyDoubleQuote] \[RightArrow] row 2
      \[OpenCurlyDoubleQuote]class\[CloseCurlyDoubleQuote] \[RightArrow] row 3
  *)
  yOfKind = <|"vptr" -> 1, "vbase" -> 2, "class" -> 3|>;

  (*
    4) Among all expanded pieces, find the farthest\[Hyphen]right byte:
       This allows us to set PlotRange\[RightArrow]{{0,maxX},{\[Ellipsis]}}.
  *)
  maxX = Max[(#["Offset"] + #["Size"]) & /@ expanded];

  (*
    5a) Build a Rectangle primitive for each small chunk:
         Rectangle[{x0, y\[Minus]0.4}, {x1, y+0.4}]
       so that each row is 0.8 \[OpenCurlyDoubleQuote]high\[CloseCurlyDoubleQuote] total, centered at y = 1,2, or 3.
  *)
  rects = expanded // Map[
    Function[assoc,
      Module[{x0, w, y},
        x0 = assoc["Offset"];
        w  = assoc["Size"];
        y  = yOfKind[assoc["Kind"]];
        Rectangle[{x0,       y - 0.4},
                  {x0 + w,   y + 0.4}]
      ]
    ]
  ];

  (*
    5b) Build a Text primitive in the center of each rectangle
         That label shows \[OpenCurlyDoubleQuote]Name (Size)\[CloseCurlyDoubleQuote].
  *)
  texts = expanded // Map[
    Function[assoc,
      Module[{x0, w, y, nm, sz},
        x0 = assoc["Offset"];
        w  = assoc["Size"];
        y  = yOfKind[assoc["Kind"]];
        nm = assoc["Name"];
        sz = assoc["Size"];
        Text[
          nm <> "\n(" <> ToString[sz] <> ")",
          {x0 + w/2, y},
          {0, 0}
        ]
      ]
    ]
  ];

  (*
    6) Collect all distinct \[OpenCurlyDoubleQuote]Offset\[CloseCurlyDoubleQuote] values so we can tick them on the X axis.
       We only tick on the X axis; no Y\[Hyphen]tick labels are needed.
  *)
  allOffsets = Sort@DeleteDuplicates[expanded[[All, "Offset"]]];
  xTicks     = ( # -> ToString[#] ) & /@ allOffsets;

  (*
    7) Set up vertical limits so rows 1,2,3 all fit:
       Our rectangles run from y=0.6 \[Dash] 1.4 (for row=1), 1.6 \[Dash] 2.4 (row=2), 2.6 \[Dash] 3.4 (row=3).
       So we choose PlotRange\[RightArrow]{yMin,yMax} = {0.5, 3.5}.
  *)
  yMin = 0.5;
  yMax = 3.5;

  (*
    8) Finally draw everything with Graphics[\[Ellipsis]].
       We remove any PlotTheme, because low\[Hyphen]level Graphics does not accept it.
  *)
  Graphics[
    {
      (* a) Draw all rectangles in light gray with a black edge *)
      EdgeForm[Black],
      FaceForm[LightGray],
      rects,

      (* b) Draw all text labels in black on top of the rectangles *)
      Black,
      texts
    },
    Axes      -> True,
    Ticks     -> {xTicks, None},            (* X ticks only, no Y ticks *)
    PlotRange -> {{0, maxX}, {yMin, yMax}},
    ImageSize -> 800,

    (* Optional styling for axes (so you still get a neat horizontal axis) *)
    AxesStyle -> Directive[Black, Thickness[0.001]],
    AxesLabel -> {None, None},

    (* Put some labels on the right\[Hyphen]hand side to explain which row is which *)
    Epilog -> {
      Text[
        Style["Row 1\n(vptr slots)", Italic, 12],
        Scaled[{0.02, 0.93}],
        {-1, 0}
      ],
      Text[
        Style["Row 2\n(vbase slots)", Italic, 12],
        Scaled[{0.02, 0.57}],
        {-1, 0}
      ],
      Text[
        Style["Row 3\n(class data)", Italic, 12],
        Scaled[{0.02, 0.21}],
        {-1, 0}
      ]
    }
  ]
]



(* ---------------------------------------------------------------------------- *)
(* ParseAndDrawLayout (Interactive)                                            *)
(* ---------------------------------------------------------------------------- *)
ParseAndDrawLayout[] := Module[{file, cls, names, choice, lay},
  file = SystemDialogInput["FileOpen"]; If[!StringQ[file]||!FileExistsQ[file], Return[$Canceled]];
  cls  = ParseCppToClasses[file]; If[cls===$Failed, Return[$Failed]];
  names= Keys[cls]; If[names==={}, Message[ParseAndDrawLayout::noClasses]; Return[$Failed]];
  choice = ChoiceDialog["Select class:", Thread[names->names]]; If[!StringQ[choice], Return[$Canceled]];
  lay = ComputeLayout[cls, choice]["Layout"]; DrawLayout[lay]
];

ParseAndDrawLayout[path_String, className_String] := Module[{cls, res},
  cls = ParseCppToClasses[path]; If[cls===$Failed, Return[$Failed]];
  res = ComputeLayout[cls, className]; DrawLayout[res["Layout"]]
];

ComputeLayoutExample[path_String, className_String]:= Module[{cls,res},
  cls = ParseCppToClasses[path]; If[cls===$Failed, Return[$Failed]];
  res = ComputeLayout[cls, className]
];

End[];

EndPackage[];




