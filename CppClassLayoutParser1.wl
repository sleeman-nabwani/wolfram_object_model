(* ::Package:: *)

(*
   CppClassLayoutParser.wl
  C++ Class Memory Layout Visualization Tool
  Parses C++ headers via clang AST and creates publication-quality memory layout diagrams
*)

BeginPackage["CppClassLayoutParser`"];

ParseCppToClasses::usage =
  "ParseCppToClasses[path_String] parses a C++ header via clang's JSON AST and returns an Association of class definitions.";

ComputeClassLayout::usage =
  "ComputeClassLayout[classes_Association, className_String] computes the memory layout for the specified class, returning an Association with Layout, VirtualBases, TotalSize, MaxAlign, and VTableEntries.";
  
DrawClassDiagram::usage = 
  "DrawClassDiagram[layout_Association] creates a visualization of the class memory layout with vtables, virtual bases, and connecting arrows.";

ParseAndDrawClassLayout::usage =
  "ParseAndDrawClassLayout[] opens a file dialog to select a C++ header file and class to visualize.";

ParseAndDrawClassLayoutPath::usage =
  "ParseAndDrawClassLayoutPath[path_String] or ParseAndDrawClassLayoutPath[path_String, className_String] parses and visualizes a specific C++ file and optionally a specific class.";

RescaleClassDiagram::usage = 
  "RescaleClassDiagram[diagram, width, height] rescales a class diagram to fit exactly within the specified dimensions, maximizing the use of available space.";

Begin["`Private`"];

(* --- Type size and alignment helpers --- *)
typeSize["char"]   := 1;
typeSize["int"]    := 4;
typeSize["double"] := 8;
typeSize[_]        := 8;  (* Default to pointer size *)
typeAlign[t_]      := typeSize[t];

(* --- Collect virtual bases recursively --- *)
getVirtBases[classes_, cls_] := Module[{rec, bs, v, nv},
  rec = getVirtBases[classes, #]&;
  If[!KeyExistsQ[classes, cls], Return[{}]];
  bs = Lookup[classes[cls], "Bases", {}];
  v  = Cases[bs, {b_, True} :> b];   (* Virtual bases *)
  nv = Cases[bs, {b_, False} :> b];  (* Non-virtual bases *)
  DeleteDuplicates@Join[v, Flatten[rec /@ nv]]
];

(* --- Parse C++ via clang AST dump --- *)
ParseCppToClasses[file_String] := Module[
  {out, json, nodes, classes},
  If[!FileExistsQ[file], 
    Message[ParseCppToClasses::nofile, file]; 
    Return[$Failed]
  ];
  
  out = RunProcess[{
      "clang", "-x", "c++", "-std=c++17", "-fsyntax-only", 
      "-fno-color-diagnostics", "-Xclang", "-ast-dump=json", file
  }, "StandardOutput"];
  
  json = Quiet@Check[ImportString[out, "RawJSON"], $Failed];
  If[!AssociationQ[json], 
    Message[ParseCppToClasses::parsefail, file]; 
    Return[$Failed]
  ];
  
  nodes = Cases[json, a_Association /; a["kind"] === "CXXRecordDecl", Infinity];
  classes = Association@Reap[
    Scan[
      Function[node,
	        If[
		      KeyExistsQ[node, "name"] &&
		      !Lookup[node, "isImplicit", False] &&
		      Lookup[node, "completeDefinition", False],
		      Sow[
		        node["name"] -> <|
              "Bases" -> Map[
                {#["type"]["qualType"], Lookup[#, "isVirtual", False]}&,
                Lookup[node, "bases", {}]
              ],
              "Fields" -> Map[
                {#["name"], #["type"]["qualType"]}&,
                Select[node["inner"], #["kind"] === "FieldDecl"&]
              ],
              "VTableEntries" -> Map[
                Lookup[#, "name"]&,
                Select[node["inner"],
                  (#["kind"] === "CXXMethodDecl" && Lookup[#, "virtual", False])&
				      ]
				    ]                             
		     |>
		   ]
        ]
      ],
      nodes
    ]
  ][[2]];
  classes
];

ParseCppToClasses::nofile = "File `1` does not exist.";
ParseCppToClasses::parsefail = "Failed to parse C++ file `1`. Check that clang is installed and the file has valid C++ syntax.";

(* --- Compute hierarchical memory layout --- *)
Clear[ComputeClassLayout];
ComputeClassLayout::noClass = "Class `1` not found in the parsed classes.";

ComputeClassLayout[classes_Association, cls_String] := Module[
  {rec, data, bases, nv, virt, layout = {}, offs = 0, maxA = 1,
   append, inherited, ownVT, allVT, vbLayouts, size, align, vtableEntries},
  
  If[!KeyExistsQ[classes, cls],
    Message[ComputeClassLayout::noClass, cls];
    Return[$Failed]
  ];

  rec = ComputeClassLayout[classes, #]&;
  data = classes[cls];
  bases = data["Bases"];
  nv = Cases[bases, {b_, False} :> b];
  virt = getVirtBases[classes, cls];
  
  (* Helper to append a subobject with proper alignment *)
  append[name_, kind_, sz_, al_, meta_:<||>] := Module[{o},
    o = Ceiling[offs/al]*al;
    AppendTo[layout,
      Join[<|"Name"->name, "Kind"->kind, "Offset"->o, "Size"->sz, "Align"->al|>, meta]
    ];
    offs = o + sz; 
    maxA = Max[maxA, al];
  ];
  
  (* Build virtual function table with proper overriding *)
  inherited = Flatten[rec[#]["VTableEntries"] & /@ nv];
  ownVT = (cls <> "::" <> #)& /@ data["VTableEntries"];
  
  (* Build final VTable order: derived functions first, then non-overridden inherited *)
  ownFuncNames = Last@StringSplit[#, "::"]& /@ ownVT;
  
  (* Get inherited functions that are NOT overridden by this class *)
  nonOverriddenInherited = Select[inherited, 
    !MemberQ[ownFuncNames, Last@StringSplit[#, "::"]]&
  ];
  
  (* Final VTable: own functions first, then non-overridden inherited *)
  allVT = Join[ownVT, nonOverriddenInherited];
  
  (* Add vptr slot if this class introduces virtual functions *)
  baseHasVirtual = Length[inherited] > 0;
  needsVptr = (Length[ownVT] > 0 && !baseHasVirtual) || (Length[allVT] > 0 && Length[nv] == 0);
  
  If[needsVptr,
    (* All vptr slots should point to the main class's VTable *)
    append["vptr", "Vptr", 8, 8,
      <|"ClassOfVtable"->cls, "VTableEntries"->allVT|>]
  ];
  
  (* Add vbase slots for direct virtual inheritance *)
  directVirtualBases = Cases[bases, {b_, True} :> b];
  Scan[
    append["vbase:"<>#, "VbaseSlot", 8, 8,
      <|"IsVirtualBase"->True, "VBaseOf"->#|>] &,
    directVirtualBases
  ];
  
  (* Add non-virtual base subobjects *)
  Scan[
    Function[b,
      Module[{bl = rec[b], modifiedLayout},
        (* Fix nested vptr slots to point to main class *)
        modifiedLayout = bl["Layout"] /. 
          (item_Association /; item["Kind"] === "Vptr") :> 
            Join[item, <|"ClassOfVtable" -> cls|>];
        
        append[b, "NonVirtualBase", bl["TotalSize"], bl["MaxAlign"],
          <|"Layout" -> modifiedLayout|>]
      ]
    ],
    nv
  ];
  
  (* Add this class's own fields *)
  size = Total[typeSize /@ data["Fields"][[All,2]]];
  align = If[data["Fields"] === {}, 1, Max[typeAlign /@ data["Fields"][[All,2]]]];
  append[cls, "Class", size, align];
  
  (* Create virtual base layouts *)
  vbLayouts = Table[
    Module[{vb = rec[v], modifiedVbLayout},
      (* Fix nested vptr slots in virtual bases to point to main class *)
      modifiedVbLayout = vb["Layout"] /. 
        (item_Association /; item["Kind"] === "Vptr") :> 
          Join[item, <|"ClassOfVtable" -> cls|>];
      
      <|
        "ClassName" -> v,
        "Layout" -> modifiedVbLayout,
        "TotalSize" -> vb["TotalSize"],
        "MaxAlign" -> vb["MaxAlign"],
        "VTableEntries" -> vb["VTableEntries"]
      |>
    ],
    {v, virt}
  ];
  
  <|
    "ClassName" -> cls,
    "Layout" -> layout,
    "VirtualBases" -> vbLayouts,
    "TotalSize" -> Ceiling[offs/maxA]*maxA,
    "MaxAlign" -> maxA,
    "VTableEntries" -> allVT
  |>
];

(* --- COMPLETELY FIXED Drawing with perfect anchor system --- *)
Clear[DrawClassDiagram];
Options[DrawClassDiagram] = {
  UnitWidth -> 25,           (* Default: 25 pixels per memory slot *)
  SlotHeight -> 15,          (* Default: 15 pixels slot height *)
  VTableHeight -> 12,        (* Default: 12 pixels per vtable entry *)
  PanelGap -> 10,            (* Default: 10 pixels between diagram panels *)
  ColorFunctions -> True,    (* Default: Enable color coding for different element types *)
  ArrowSize -> 0.01          (* Default: 0.01 arrow head size *)
};

DrawClassDiagram[layout_, opts:OptionsPattern[]] := Module[
  {
    (* Data extraction *)
    main = layout["Layout"],
    vbs = layout["VirtualBases"],
    vtables = layout["VTableEntries"],
    mainClassName = layout["ClassName"],
    
    (* Options *)
    U = OptionValue[UnitWidth],
    H = OptionValue[SlotHeight],
    vH = OptionValue[VTableHeight],
    gap = OptionValue[PanelGap],
    useColors = OptionValue[ColorFunctions],
    arrowSize = OptionValue[ArrowSize],
    
    (* Layout dimensions *)
    mainBarWidth, vtableWidth, vtableHeight, totalVbWidth,
    mainY, vtableX, vtableY, vbY, totalWidth, totalHeight,
    
    (* Graphics collections *)
    graphics = {}, arrows = {},
    
    (* COMPLETELY FIXED: Simple and reliable anchor system *)
    vptrAnchors = {}, vbaseAnchors = <||>, vtableAnchor = None, virtualBaseAnchors = <||>,
    
    (* Helper functions *)
    getColor, drawSlot, drawMainBar, drawVTable, drawVirtualBases
  },
  
  (* Input validation *)
  If[!AssociationQ[layout], Return[$Failed]];
  If[main === {}, main = {}];
  If[vbs === {}, vbs = {}];
  If[vtables === {}, vtables = {}];
  
  (* Calculate layout dimensions *)
  mainBarWidth = Length[main] * U;
  vtableHeight = If[vtables === {}, 0, Length[vtables] * vH];
  vtableWidth = If[vtables === {}, 0, Max[60, Max[StringLength /@ vtables] * 3]];
  totalVbWidth = If[vbs === {}, 0, Total[Max[80, Length[#["Layout"]] * U * 0.7] & /@ vbs] + If[Length[vbs] > 1, (Length[vbs] - 1)*gap, 0]];
  
  (* FIXED: Proper positioning - main content on left, VTable on right *)
  mainY = 2*gap + If[vtables === {}, 0, vtableHeight + gap];
  vtableY = gap;
  vbY = mainY + H + gap;
  
  (* FIXED: VTable positioned to the right of main content area *)
  vtableX = gap + mainBarWidth + gap;
  
  (* FIXED: Calculate total dimensions to include virtual bases positioned to the right *)
  totalWidth = gap + mainBarWidth + gap + vtableWidth + gap + totalVbWidth + gap;
  totalHeight = gap + If[vtables === {}, 0, vtableHeight + gap] + H + gap + If[vbs === {}, 0, H + gap];
  
  (* Color scheme for different element types *)
  getColor[kind_] := Which[
    !useColors, GrayLevel[0.95],
    kind === "Vptr", RGBColor[1, 0.7, 0.7],          (* Salmon for vptr *)
    kind === "VbaseSlot", RGBColor[0.6, 0.6, 1],     (* Blue for vbase slots *)
    kind === "NonVirtualBase", RGBColor[1, 1, 0.9],  (* Very light yellow *)
    kind === "Class", GrayLevel[0.8],                (* Light gray *)
    True, White
  ];
  
  (* COMPLETELY FIXED: Draw a single memory slot with perfect anchor registration *)
  drawSlot[item_, x_, y_, width_] := Module[
    {color = getColor[item["Kind"]], label, nestedGraphics = {}},
    
    (* Determine display label *)
    label = Switch[item["Kind"],
      "Vptr", "vptr",
      "VbaseSlot", "vbase:" <> item["VBaseOf"],
      _, item["Name"]
    ];
    
    (* COMPLETELY FIXED: Simple anchor registration with AppendTo *)
    Switch[item["Kind"],
      "Vptr", 
        AppendTo[vptrAnchors, {x + width/2, y + H/2}],
      "VbaseSlot", 
        Module[{className = item["VBaseOf"]},
          If[!KeyExistsQ[vbaseAnchors, className], vbaseAnchors[className] = {}];
          AppendTo[vbaseAnchors[className], {x + width/2, y + H}];
        ]
    ];
    
    (* Handle nested layouts for base classes *)
    If[item["Kind"] === "NonVirtualBase" && KeyExistsQ[item, "Layout"] && Length[item["Layout"]] > 0,
      Module[{nestedLayout = item["Layout"], nestedWidth = width / Length[item["Layout"]], nestedX = x},
        Do[
          Module[{nestedItem = nestedLayout[[j]]},
            (* COMPLETELY FIXED: Register nested anchors simply *)
            Switch[nestedItem["Kind"],
              "Vptr", 
                AppendTo[vptrAnchors, {nestedX + nestedWidth/2, y + H/2}],
              "VbaseSlot", 
                Module[{className = nestedItem["VBaseOf"]},
                  If[!KeyExistsQ[vbaseAnchors, className], vbaseAnchors[className] = {}];
                  AppendTo[vbaseAnchors[className], {nestedX + nestedWidth/2, y + H}];
                ]
            ];
            
            (* Draw nested slot *)
            AppendTo[nestedGraphics, {
              {EdgeForm[{Black, Thickness[0.001]}], FaceForm[getColor[nestedItem["Kind"]]],
               Rectangle[{nestedX, y + 2}, {nestedX + nestedWidth, y + H - 2}]},
              Text[Switch[nestedItem["Kind"],
                "Vptr", "vptr",
                "VbaseSlot", "vbase:" <> nestedItem["VBaseOf"],
                _, nestedItem["Name"]
              ], 
              {nestedX + nestedWidth/2, y + H/2},
              BaseStyle -> {FontFamily -> "Arial", FontSize -> 8, FontWeight -> Bold}]
            }];
            nestedX += nestedWidth;
          ],
          {j, Length[nestedLayout]}
        ]
      ]
    ];
    
    (* Draw main slot rectangle and label *)
    {
      {EdgeForm[{Black, Thickness[0.002]}], FaceForm[color],
       Rectangle[{x, y}, {x + width, y + H}]},
      Text[label, {x + width/2, y + H/2},
           BaseStyle -> {FontFamily -> "Arial", FontSize -> 8, FontWeight -> Bold}],
      nestedGraphics
    }
  ];
  
  (* Draw main object memory bar *)
  drawMainBar[] := Module[{x = gap},
    AppendTo[graphics, {
      {EdgeForm[{Black, Thickness[0.003]}], FaceForm[None],
       Rectangle[{gap, mainY}, {gap + mainBarWidth, mainY + H}]},
      Text["Object: " <> mainClassName, 
           {gap + mainBarWidth/2, mainY + H + 15},
           BaseStyle -> {FontFamily -> "Arial", FontSize -> 8, FontWeight -> Bold}]
    }];
    
    Do[
      AppendTo[graphics, drawSlot[main[[i]], x, mainY, U]];
      x += U;
    , {i, Length[main]}]
  ];
  
  (* COMPLETELY FIXED: Draw VTable with proper anchor *)
  drawVTable[] := Module[{},
    If[vtables === {}, Return[]];
    
    (* COMPLETELY FIXED: Simple VTable anchor assignment *)
    vtableAnchor = {vtableX, vtableY + vtableHeight};
    
    AppendTo[graphics, {
      {EdgeForm[{Black, Thickness[0.002]}], FaceForm[White],
       Rectangle[{vtableX, vtableY}, {vtableX + vtableWidth, vtableY + vtableHeight}]},
      Text["VTable: " <> mainClassName, 
           {vtableX + vtableWidth/2, vtableY + vtableHeight + 12},
           BaseStyle -> {FontFamily -> "Arial", FontSize -> 8, FontWeight -> Bold}]
    }];
    
    (* Draw VTable entries from bottom to top (index 1 at bottom, highest index at top) *)
    Do[
      AppendTo[graphics, {
        {EdgeForm[{GrayLevel[0.8], Thickness[0.001]}], FaceForm[White],
         Rectangle[{vtableX, vtableY + (Length[vtables]-i)*vH}, {vtableX + vtableWidth, vtableY + (Length[vtables]-i+1)*vH}]},
        Text[vtables[[i]], 
             {vtableX + vtableWidth/2, vtableY + (Length[vtables]-i)*vH + vH/2},
             BaseStyle -> {FontFamily -> "Courier", FontSize -> 8, FontWeight -> Bold}]
      }]
    , {i, Length[vtables]}]
  ];
  
  (* Draw virtual base panels *)
  drawVirtualBases[] := Module[{x = gap},
    Do[
      Module[{vb = vbs[[i]], vbWidth = Max[80, Length[vb["Layout"]] * U * 0.7], vbX = gap + mainBarWidth + gap + vtableWidth + gap + x, vbLayout = vb["Layout"]},
        (* COMPLETELY FIXED: Store virtual base anchor with width information *)
        virtualBaseAnchors[vb["ClassName"]] = <|"Position" -> {vbX + vbWidth/2, vbY}, "Width" -> vbWidth, "LeftEdge" -> vbX|>;
        
        AppendTo[graphics, {
          {EdgeForm[{Blue, Dashed, Thickness[0.003]}], FaceForm[None],
           Rectangle[{vbX, vbY}, {vbX + vbWidth, vbY + H}]},
          Text["Virtual Base: " <> vb["ClassName"],
               {vbX + vbWidth/2, vbY - 12},
               BaseStyle -> {FontFamily -> "Arial", FontSize -> 8, 
                            FontColor -> Blue, FontWeight -> Bold}]
        }];
        
        (* Draw internal structure of virtual base *)
        If[Length[vbLayout] > 0,
          Module[{nestedWidth = vbWidth / Length[vbLayout], nestedX = vbX},
            Do[
              Module[{nestedItem = vbLayout[[j]]},
                (* COMPLETELY FIXED: Register nested anchors simply *)
                Switch[nestedItem["Kind"],
                  "Vptr", 
                    AppendTo[vptrAnchors, {nestedX + nestedWidth/2, vbY + H/2}],
                  "VbaseSlot", 
                    Module[{className = nestedItem["VBaseOf"]},
                      If[!KeyExistsQ[vbaseAnchors, className], vbaseAnchors[className] = {}];
                      AppendTo[vbaseAnchors[className], {nestedX + nestedWidth/2, vbY + H}];
                    ]
                ];
                
                AppendTo[graphics, {
                  {EdgeForm[{Blue, Thickness[0.001]}], FaceForm[getColor[nestedItem["Kind"]]],
                   Rectangle[{nestedX + 2, vbY + 2}, {nestedX + nestedWidth - 2, vbY + H - 2}]},
                  Text[Switch[nestedItem["Kind"],
                    "Vptr", "vptr",
                    "VbaseSlot", "vbase:" <> nestedItem["VBaseOf"],
                    _, nestedItem["Name"]
                  ], 
                  {nestedX + nestedWidth/2, vbY + H/2},
                  BaseStyle -> {FontFamily -> "Arial", FontSize -> 8, FontWeight -> Bold}]
                }];
                nestedX += nestedWidth;
              ],
              {j, Length[vbLayout]}
            ]
          ]
        ];
        
        x += vbWidth + gap;
      ]
    , {i, Length[vbs]}]
  ];
  
  (* COMPLETELY FIXED: Draw connection arrows with foolproof logic *)
  drawArrows[] := Module[{},
    (* 1) COMPLETELY FIXED: Black L-shaped arrows from vptr to VTable *)
    If[Length[vptrAnchors] > 0 && vtableAnchor =!= None,
      Do[
        Module[{src, tgt, adjustedSrc, adjustedTgt, midPt},
          src = vptrAnchors[[i]];
          tgt = vtableAnchor;
          
          (* Perfect L-shaped arrow: up then right *)
          adjustedSrc = {src[[1]], src[[2]] - H/2};  (* Top of vptr slot *)
          adjustedTgt = tgt;                          (* Left edge of VTable *)
          midPt = {adjustedSrc[[1]], adjustedTgt[[2]]};
          
          AppendTo[arrows, {
            Black, Thickness[0.003], Arrowheads[{0, 0.03}],
            Arrow[{adjustedSrc, midPt, adjustedTgt}]
          }];
        ],
        {i, Length[vptrAnchors]}
      ];
    ];
    
    (* 2) COMPLETELY FIXED: Blue L-shaped arrows from vbase slots to virtual bases *)
    KeyValueMap[
      Function[{className, vbaseSrcs},
        If[KeyExistsQ[virtualBaseAnchors, className],
          Module[{tgtInfo = virtualBaseAnchors[className], tgt, vbWidth, leftEdge},
            tgt = tgtInfo["Position"];
            vbWidth = tgtInfo["Width"];
            leftEdge = tgtInfo["LeftEdge"];
            
            Do[
              Module[{src, adjustedSrc, adjustedTgt, midPt},
                src = vbaseSrcs[[i]];
                
                (* Perfect L-shaped arrow: down then right *)
                adjustedSrc = {src[[1]], src[[2]]};  (* Bottom of vbase slot *)
                adjustedTgt = {leftEdge, tgt[[2]] + H/2};  (* Left edge of virtual base *)
                midPt = {adjustedSrc[[1]], adjustedTgt[[2]]};
                
                AppendTo[arrows, {
                  Blue, Thickness[0.003], Arrowheads[{0, 0.03}],
                  Arrow[{adjustedSrc, midPt, adjustedTgt}]
                }];
              ],
              {i, Length[vbaseSrcs]}
            ];
          ]
        ]
      ],
      vbaseAnchors
    ];
  ];
  
  (* Build the complete diagram *)
  drawVirtualBases[];
  drawMainBar[];
  drawVTable[];
  drawArrows[];
  
  (* Create base graphics object *)
  Graphics[
    Join[graphics, arrows],
    PlotRange -> {{0, totalWidth}, {0, totalHeight}},
    Background -> White,
    Axes -> False,
    Frame -> False
  ]
];

(* --- Helper function for rescaling diagrams --- *)
RescaleClassDiagram[diagram_Graphics, width_:800, height_:600] := Show[
  diagram,
  PlotRange -> All,              (* Use full data range of all primitives *)
  PlotRangePadding -> None,      (* No extra margin around range *)
  ImagePadding -> None,          (* No extra white border *)
  AspectRatio -> Automatic,
  ImageSize -> {width, height}   (* Custom size *)
];

(* --- Interactive helper functions --- *)
Clear[ParseAndDrawClassLayout];
ParseAndDrawClassLayout[] := Module[{file, cls, choice, layout},
  file = SystemDialogInput["FileOpen", WindowTitle -> "Select C++ Header File"];
  If[!StringQ[file], Return[$Canceled]];
  
  cls = ParseCppToClasses[file];
  If[cls === $Failed, Return[$Failed]];
  
  If[Length[Keys[cls]] == 0,
    Message[ParseAndDrawClassLayout::noclasses, file];
    Return[$Failed]
  ];
  
  choice = ChoiceDialog["Select class to visualize", Thread[Keys[cls] -> Keys[cls]]];
  If[!StringQ[choice], Return[$Canceled]];
  
  layout = ComputeClassLayout[cls, choice];
  If[layout === $Failed, Return[$Failed]];
  
  DrawClassDiagram[layout]
];

ParseAndDrawClassLayout::noclasses = "No classes found in file `1`.";

Clear[ParseAndDrawClassLayoutPath];
ParseAndDrawClassLayoutPath[path_String] := Module[{cls, layout, choice },
  cls = ParseCppToClasses[path];
  If[cls === $Failed, Return[$Failed]];
  
  If[Length[Keys[cls]] == 0,
    Message[ParseAndDrawClassLayoutPath::noclasses, path];
    Return[$Failed]
  ];
  
  choice = ChoiceDialog["Select class to visualize", Thread[Keys[cls] -> Keys[cls]]];
  If[!StringQ[choice], Return[$Canceled]];
  
  layout = ComputeClassLayout[cls, choice];
  If[layout === $Failed, Return[$Failed]];
  
  DrawClassDiagram[layout]
];

ParseAndDrawClassLayoutPath[path_String, className_String] := Module[{cls, layout},
  cls = ParseCppToClasses[path]; 
  If[cls === $Failed, Return[$Failed]];
  
  layout = ComputeClassLayout[cls, className];
  If[layout === $Failed, Return[$Failed]];
  
  DrawClassDiagram[layout]
];

ParseAndDrawClassLayoutPath::noclasses = "No classes found in file `1`.";

End[]; 
EndPackage[];
