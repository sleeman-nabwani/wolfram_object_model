(* ::Package:: *)

(*
   CppClassLayoutParser.wl
  C++ Class Memory Layout Visualization Tool
  Parses C++ headers via clang AST and creates publication-quality memory layout diagrams
*)

(* Clear any existing symbols to prevent shadowing *)
Quiet[Remove["CppClassLayoutParser`*"]];
Quiet[Remove["ParseCppToClasses", "ComputeClassLayout", "DrawClassDiagram", "ParseAndDrawClassLayout", "ParseAndDrawClassLayoutPath", "RescaleClassDiagram"]];

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

(* --- Collect direct virtual bases only --- *)
getDirectVirtBases[classes_, cls_] := Module[{bs},
  If[!KeyExistsQ[classes, cls], Return[{}]];
  bs = Lookup[classes[cls], "Bases", {}];
  Cases[bs, {b_, True} :> b]  (* Only direct virtual bases *)
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

(* --- Restore recursive collection of all virtual bases for panel display --- *)
getVirtBases[classes_, cls_] := Module[{rec, bs, v, nv},
  rec = getVirtBases[classes, #]&;
  If[!KeyExistsQ[classes, cls], Return[{}]];
  bs = Lookup[classes[cls], "Bases", {}];
  v  = Cases[bs, {b_, True} :> b];   (* Virtual bases *)
  nv = Cases[bs, {b_, False} :> b];  (* Non-virtual bases *)
  DeleteDuplicates@Join[v, Flatten[rec /@ nv]]
];

ComputeClassLayout[classes_Association, cls_String] := Module[
  {rec, data, bases, nv, virt, layout = {}, offs = 0, maxA = 1,
   append, inherited, ownVT, allVT, vbLayouts, size, align, vtableEntries,
   resolveVTableEntries, directVirtBases, allVirtBases
  },
  
  If[!KeyExistsQ[classes, cls],
    Message[ComputeClassLayout::noClass, cls];
    Return[$Failed]
  ];

  rec = ComputeClassLayout[classes, #]&;
  data = classes[cls];
  bases = data["Bases"];
  nv = Cases[bases, {b_, False} :> b];
  directVirtBases = getDirectVirtBases[classes, cls];  (* Only direct virtual bases for vbase slots *)
  allVirtBases = getVirtBases[classes, cls];            (* All virtual bases for panel display *)
  
  (* ENHANCED: Helper function to resolve VTable entries with proper override semantics *)
  resolveVTableEntries[targetClassName_String, baseClassName_String] := Module[{
    baseClassFunctions, derivedClassFunctions, resolvedEntries = {}
  },
    (* Get base class function names (without class prefix) *)
    baseClassFunctions = If[classes =!= None && KeyExistsQ[classes, baseClassName],
      classes[baseClassName]["VTableEntries"],
      {}
    ];
    
    (* Get main derived class function names for override checking *)
    derivedClassFunctions = If[classes =!= None && KeyExistsQ[classes, targetClassName],
      classes[targetClassName]["VTableEntries"],
      {}
    ];
    
    (* For each function in the base class *)
    Do[
      Module[{baseFunctionName = baseClassFunctions[[i]], finalFunction, bestOverride},
        (* Start with the base class version *)
        bestOverride = baseClassName;
        
        (* Check if target derived class overrides this function *)
        If[MemberQ[derivedClassFunctions, baseFunctionName],
          bestOverride = targetClassName;
        ];
        
        finalFunction = bestOverride <> "::" <> baseFunctionName;
        AppendTo[resolvedEntries, finalFunction];
      ]
    , {i, Length[baseClassFunctions]}];
    
    resolvedEntries
  ];
  
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
  
  (* Add vbase slots for ONLY direct virtual bases *)
  Scan[
    append["vbase:"<>#, "VbaseSlot", 8, 8,
      <|"IsVirtualBase"->True, "VBaseOf"->#|>] &,
    directVirtBases
  ];
  
  (* Add non-virtual base subobjects *)
  Module[{baseIndex = 0},
  Scan[
    Function[b,
        Module[{bl = rec[b], modifiedLayout, isPrimaryBase},
          baseIndex++;
          isPrimaryBase = (baseIndex == 1);  (* Only first non-virtual base is primary *)
          
          (* ENHANCED: Properly resolve VTable entries for base classes *)
          modifiedLayout = If[isPrimaryBase,
            (* Primary base: modify vptr to point to main class with main class VTable entries *)
            bl["Layout"] /. 
          (item_Association /; item["Kind"] === "Vptr") :> 
                Join[item, <|"ClassOfVtable" -> cls, "VTableEntries" -> allVT|>],
            (* Secondary bases: keep original vptr but resolve VTable entries correctly *)
            bl["Layout"] /. 
          (item_Association /; item["Kind"] === "Vptr") :> 
                Join[item, <|"VTableEntries" -> resolveVTableEntries[cls, b]|>]
          ];
        
        append[b, "NonVirtualBase", bl["TotalSize"], bl["MaxAlign"],
          <|"Layout" -> modifiedLayout|>]
      ]
    ],
    nv
    ];
  ];
  
  (* Add this class's own fields *)
  size = Total[typeSize /@ data["Fields"][[All,2]]];
  align = If[data["Fields"] === {}, 1, Max[typeAlign /@ data["Fields"][[All,2]]]];
  append[cls, "Class", size, align];
  
  (* Create virtual base layouts recursively for all virtual bases *)
  vbLayouts = Table[
    Module[{vbLayout},
      vbLayout = ComputeClassLayout[classes, v];
      <|
        "ClassName" -> v,
        "Layout" -> vbLayout["Layout"],
        "TotalSize" -> Max[vbLayout["TotalSize"], 1],
        "MaxAlign" -> vbLayout["MaxAlign"],
        "VTableEntries" -> vbLayout["VTableEntries"],
        "VirtualBases" -> vbLayout["VirtualBases"]
      |>
    ],
    {v, allVirtBases}
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
  ArrowSize -> 0.01,         (* Default: 0.01 arrow head size *)
  SeparateVTables -> True,   (* CHANGED: Always show separate VTables by default *)
  MergeVTables -> False,     (* NEW: Option to merge VTables with first vptr *)
  BaseLabelFontSize -> 16    (* NEW: User-configurable base font size for labels *)
};

DrawClassDiagram[layout_, opts:OptionsPattern[], classes_Association:None] := Module[
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
    separateVTables = OptionValue[SeparateVTables],
    mergeVTables = OptionValue[MergeVTables],
    baseLabelFontSize = OptionValue[BaseLabelFontSize],
    
    (* --- UNIFORM BLOCK SIZE FOR ALL SLOTS AND CLASSES --- *)
    uniformBlockWidth = 100,
    Uscaled,
    
    (* NEW: Multiple VTable system *)
    vtableStructures = {},  (* List of {className, vtableEntries, position} *)
    vtableAnchors = <||>,   (* Association: className -> {x, y} *)
    
    (* Layout dimensions *)
    mainBarWidth, totalVTableWidth, totalVTableHeight, totalVbWidth,
    mainY, vtableStartX, vtableY, vbY, totalWidth, totalHeight, minVTableY = 0,
    
    (* Graphics collections *)
    graphics = {}, arrows = {},
    
    (* ENHANCED: Separate anchor system for multiple VTables *)
    vptrAnchors = {},           (* List of {position, className} *)
    vbaseAnchors = <||>, 
    virtualBaseAnchors = <||>,
    
    (* Helper functions *)
    getColor, drawSlot, drawMainBar, drawVTables, drawVirtualBases, collectVTableStructures, updateDimensions,
  
    (* Dynamic scaling: set a max total width for main object and virtual base panels *)
    maxBarWidth, mainNumSlots,
    
    (* --- DYNAMIC SCALING FOR ARROWS AND FONTS (AFTER FINAL DIMENSIONS) --- *)
    diagramRefSize, arrowThickness, arrowHeadSize, labelFontSize
  },
  
  (* SET UNIFORM BLOCK SIZE AT THE BEGINNING *)
  Uscaled = uniformBlockWidth;
  
  (* --- DYNAMIC SCALING FOR ARROWS AND FONTS (AFTER FINAL DIMENSIONS) --- *)
  diagramRefSize = Max[totalWidth, totalHeight, 400];
  arrowThickness = 0.0025 * (400/diagramRefSize);
  arrowHeadSize = 0.03 * (400/diagramRefSize);
  labelFontSize = baseLabelFontSize * (400/diagramRefSize);
  
  (* --- STRICT SLOT-TYPE-BASED SIZING (ABSOLUTE UNIFORMITY) --- *)
  vptrWidth = 60;
  vbaseWidth = 80;
  fieldWidth = 80;
  classBlockWidth = 120;
  defaultWidth = 100;

  getSlotWidth[item_] := Switch[item["Kind"],
    "Vptr", vptrWidth,
    "VbaseSlot", vbaseWidth,
    "Field", fieldWidth,
    "NonVirtualBase" | "Class", 
      Module[{nested = If[KeyExistsQ[item, "Layout"], item["Layout"], {}]},
        If[Length[nested] > 0, Total[getSlotWidth[#] & /@ nested], classBlockWidth]
      ],
    _, defaultWidth
  ];
  
  (* Dynamic scaling calculations go here *)
  maxBarWidth = 800;
  mainNumSlots = Max[1, Length[main]];
  mainBarWidth = Total[getSlotWidth[#] & /@ main];
  
  (* INITIAL: Set placeholder VTable dimensions - will be updated after collection *)
  totalVTableWidth = 0;
  totalVTableHeight = If[vtables === {}, 0, Length[vtables] * vH];
  
  (* ENHANCED: Calculate virtual base width using dynamic slot widths *)
  totalVbWidth = If[vbs === {}, 0, 
    Total[Module[{vbLayout = #["Layout"]}, 
      If[Length[vbLayout] > 0, 
        Total[getSlotWidth[#] & /@ vbLayout], 
        classBlockWidth
      ]
    ] & /@ vbs] + If[Length[vbs] > 1, (Length[vbs] - 1)*gap, 0]
  ];
  
  (* FIXED: Proper positioning - virtual bases at top, main in middle, VTables at bottom *)
  vtableY = gap;  (* VTables at bottom (lowest Y) *)
  mainY = vtableY + If[vtables === {}, 0, totalVTableHeight + gap];  (* Main bar above VTables *)
  vbY = mainY + H + gap;  (* Virtual bases at top (highest Y) *)
  
  (* FIXED: VTable positioned to the right of main content area *)
  vtableStartX = gap + mainBarWidth + gap;
  
  (* PLACEHOLDER: Calculate total dimensions - will be updated after VTable collection *)
  totalWidth = gap + mainBarWidth + gap + totalVTableWidth + gap + totalVbWidth + gap;
  totalHeight = gap + If[vbs === {}, 0, H + gap] + If[vtables === {}, 0, totalVTableHeight + gap] + H + gap;
  
  (* Color scheme for different element types *)
  getColor[kind_] := Which[
    !useColors, GrayLevel[0.95],
    kind === "Vptr", RGBColor[1, 0.7, 0.7],          (* Salmon for vptr *)
    kind === "VbaseSlot", RGBColor[0.6, 0.6, 1],     (* Blue for vbase slots *)
    kind === "NonVirtualBase", RGBColor[1, 1, 0.9],  (* Very light yellow *)
    kind === "Class", GrayLevel[0.8],                (* Light gray *)
    True, White
  ];
  
  (* FIXED: Draw uniform block but show internal structure for subobjects, with correct width *)
  drawSlot[item_, x_, y_, width_:Automatic] := Module[
    {color = getColor[item["Kind"]], label, nestedGraphics = {}, actualWidth},
    
    label = Switch[item["Kind"],
      "Vptr", "vptr",
      "VbaseSlot", "vbase:" <> item["VBaseOf"],
      _, item["Name"]
    ];
    
    (* Register anchors for arrows *)
    Switch[item["Kind"],
      "Vptr", 
        Module[{targetClass = If[KeyExistsQ[item, "ClassOfVtable"], item["ClassOfVtable"], mainClassName]},
          AppendTo[vptrAnchors, {{x + getSlotWidth[item]/2, y + H/2}, targetClass}]
        ],
      "VbaseSlot", 
        Module[{className = item["VBaseOf"]},
          If[!KeyExistsQ[vbaseAnchors, className], vbaseAnchors[className] = {}];
          AppendTo[vbaseAnchors[className], {x + getSlotWidth[item]/2, y + H}];
        ]
    ];
    
    If[(item["Kind"] === "NonVirtualBase" || item["Kind"] === "Class") && KeyExistsQ[item, "Layout"] && Length[item["Layout"]] > 0,
      Module[{nestedLayout = item["Layout"], subX = x, subWidthList},
        subWidthList = getSlotWidth /@ nestedLayout;
        actualWidth = Total[subWidthList];
        (* Draw the outer rectangle for the subobject at the correct width *)
            AppendTo[nestedGraphics, {
          {EdgeForm[{Black, Thickness[0.0015]}], FaceForm[color],
           Rectangle[{x, y}, {x + actualWidth, y + H}]},
          Text[label, {x + actualWidth/2, y + H/2},
              BaseStyle -> {FontFamily -> "Arial", FontSize -> labelFontSize, FontWeight -> Bold}]
            }];
        (* Recursively draw the nested layout, each at its fixed width *)
        Do[
          Module[{nestedItem = nestedLayout[[j]], slotW = subWidthList[[j]]},
            AppendTo[nestedGraphics, drawSlot[nestedItem, subX, y, slotW]];
            subX += slotW;
          ];
        , {j, Length[nestedLayout]}];
        Return[nestedGraphics];
      ]
    ];
    
    (* Default: draw as a single uniform block *)
    actualWidth = If[width === Automatic, getSlotWidth[item], width];
    {
      {EdgeForm[{Black, Thickness[0.0015]}], FaceForm[color],
       Rectangle[{x, y}, {x + actualWidth, y + H}]},
      Text[label, {x + actualWidth/2, y + H/2},
           BaseStyle -> {FontFamily -> "Arial", FontSize -> labelFontSize, FontWeight -> Bold}]
    }
  ];
  
  (* Draw main object memory bar *)
  drawMainBar[] := Module[{x = gap},
    AppendTo[graphics, {
      {EdgeForm[{Black, Thickness[0.0015]}], FaceForm[None],
       Rectangle[{gap, mainY}, {gap + mainBarWidth, mainY + H}]},
      Text["Object: " <> mainClassName, 
           {gap + mainBarWidth/2, mainY + H + 15},
           BaseStyle -> {FontFamily -> "Arial", FontSize -> labelFontSize, FontWeight -> Bold}]
    }];
    
    Do[
      Module[{slotWidth = getSlotWidth[main[[i]]]},
        AppendTo[graphics, drawSlot[main[[i]], x, mainY, slotWidth]];
        x += slotWidth;
      ]
    , {i, Length[main]}]
  ];
  
  (* ENHANCED: Draw multiple VTables *)
  drawVTables[] := Module[{},
    If[Length[vtableStructures] == 0, Return[]];
    
    (* Draw each VTable *)
    Do[
      Module[{vtableStruct = vtableStructures[[i]], className, vtableEntries, pos, 
              vtableWidth, vtableHeight, vtableX, vtableY},
        className = vtableStruct["ClassName"];
        vtableEntries = vtableStruct["VTableEntries"];
        pos = vtableStruct["Position"];
        vtableX = pos[[1]];
        vtableY = pos[[2]];
        vtableWidth = Max[60, Max[StringLength /@ vtableEntries] * 3];
        vtableHeight = vtableStruct["Height"];  (* Use stored height for cascading *)
        
        (* Draw VTable border and label *)
    AppendTo[graphics, {
      {EdgeForm[{Black, Thickness[0.002]}], FaceForm[White],
       Rectangle[{vtableX, vtableY}, {vtableX + vtableWidth, vtableY + vtableHeight}]},
          Text["VTable: " <> className, 
               {vtableX + vtableWidth/2, vtableY + vtableHeight + 5},
           BaseStyle -> {FontFamily -> "Arial", FontSize -> labelFontSize, FontWeight -> Bold}]
    }];
    
        (* Draw VTable entries from bottom to top *)
    Do[
      AppendTo[graphics, {
        {EdgeForm[{GrayLevel[0.8], Thickness[0.001]}], FaceForm[White],
             Rectangle[{vtableX, vtableY + (Length[vtableEntries]-j)*vH}, 
                      {vtableX + vtableWidth, vtableY + (Length[vtableEntries]-j+1)*vH}]},
            Text[vtableEntries[[j]], 
                 {vtableX + vtableWidth/2, vtableY + (Length[vtableEntries]-j)*vH + vH/2},
             BaseStyle -> {FontFamily -> "Courier", FontSize -> labelFontSize, FontWeight -> Bold}]
      }]
        , {j, Length[vtableEntries]}];
      ]
    , {i, Length[vtableStructures]}];
  ];
  
  (* ENHANCED: Collect VTable structures for ALL base classes with virtual functions *)
  collectVTableStructures[] := Module[{
    allBases = {},           (* List of all base classes with their VTable info *)
    shouldMerge = False,     (* Whether to merge first base VTable *)
    firstNonVirtualBase = None,
    currentY = vtableY,
    currentX = vtableStartX, (* FIXED: Use currentX for horizontal positioning *)
    
    (* ENHANCED: Complete function overriding logic *)
    resolveVTableEntries,
    
    (* ENHANCED: Extract class info from layout when classes parameter is None *)
    extractedClasses = If[classes === None, <||>, classes]
  },
    
    (* SIMPLIFIED: Direct VTable extraction when classes is None *)
    If[classes === None,
      (* Simple mode: extract VTables directly from layout *)
      
      (* Add main class if it has virtual functions *)
      If[Length[vtables] > 0,
        AppendTo[allBases, <|
          "ClassName" -> mainClassName,
          "VTableEntries" -> vtables,
          "IsVirtual" -> False,
          "IsPrimary" -> True
        |>];
      ];
      
      (* Add virtual bases *)
      Do[
        Module[{vb = vbs[[i]], vtableEntries},
          vtableEntries = vb["VTableEntries"];
          If[Length[vtableEntries] > 0,
            AppendTo[allBases, <|
              "ClassName" -> vb["ClassName"],
              "VTableEntries" -> vtableEntries,
              "IsVirtual" -> True,
              "IsPrimary" -> False
            |>];
          ];
        ];
      , {i, Length[vbs]}];
      
      (* Add non-virtual bases *)
      Module[{baseIndex = 0},
        Do[
          Module[{item = main[[i]]},
            If[item["Kind"] === "NonVirtualBase",
              baseIndex++;
              Module[{baseClassName = item["Name"], isPrimary = (baseIndex == 1)},
                If[!isPrimary && KeyExistsQ[item, "Layout"],
                  Module[{baseLayout = item["Layout"], hasVptr = False, extractedVTableEntries = {}},
                    Do[
                      If[baseLayout[[j]]["Kind"] === "Vptr",
                        hasVptr = True;
                        If[KeyExistsQ[baseLayout[[j]], "VTableEntries"],
                          extractedVTableEntries = baseLayout[[j]]["VTableEntries"];
                        ];
                        Break[];
                      ];
                    , {j, Length[baseLayout]}];
                    
                    If[hasVptr && Length[extractedVTableEntries] > 0,
                      AppendTo[allBases, <|
                        "ClassName" -> baseClassName,
                        "VTableEntries" -> extractedVTableEntries,
                        "IsVirtual" -> False,
                        "IsPrimary" -> False
                      |>];
                    ];
                  ];
                ];
              ];
            ];
          ];
        , {i, Length[main]}];
      ];
      
    , (* COMPLEX MODE: Use resolveVTableEntries function when classes is provided *)
      
      (* Helper function to resolve which functions are actually called *)
      resolveVTableEntries[className_String] := Module[{
        classVTableEntries, inheritedEntries, finalEntries
      },
        (* Original logic when classes parameter is provided *)
        (* Get this class's virtual functions *)
        classVTableEntries = If[extractedClasses =!= None && KeyExistsQ[extractedClasses, className],
          (className <> "::" <> #)& /@ extractedClasses[className]["VTableEntries"],
          {}
        ];
        
        (* ENHANCED: Complete function overriding logic *)
        If[className === mainClassName,
          (* For the main class, return its own VTable entries *)
          classVTableEntries,
          (* For base classes, resolve which version of each function would actually be called *)
          Module[{
            baseClassFunctions, derivedClassFunctions, resolvedEntries = {},
            allDerivedClasses, inheritanceChain
          },
            (* Get base class function names (without class prefix) *)
            baseClassFunctions = If[extractedClasses =!= None && KeyExistsQ[extractedClasses, className],
              extractedClasses[className]["VTableEntries"],
              {}
            ];
            
            (* Get main derived class function names for override checking *)
            derivedClassFunctions = If[extractedClasses =!= None && KeyExistsQ[extractedClasses, mainClassName],
              extractedClasses[mainClassName]["VTableEntries"],
              {}
            ];
            
            (* Get all intermediate classes that might override functions *)
            allDerivedClasses = If[extractedClasses =!= None, Keys[extractedClasses], {}];
            
            (* For each function in the base class VTable *)
            Do[
              Module[{baseFunctionName = baseClassFunctions[[i]], finalFunction, bestOverride},
                (* Start with the base class version *)
                bestOverride = className;
                
                (* Check if main derived class overrides this function *)
                If[MemberQ[derivedClassFunctions, baseFunctionName],
                  bestOverride = mainClassName;
                ];
                
                (* FUTURE: Could add logic to check intermediate classes in inheritance chain *)
                (* For now, check direct inheritance path: base -> main *)
                
                finalFunction = bestOverride <> "::" <> baseFunctionName;
                AppendTo[resolvedEntries, finalFunction];
              ]
            , {i, Length[baseClassFunctions]}];
            
            resolvedEntries
          ]
        ]
      ];
      
      (* 1. COLLECT ALL CLASSES WITH VIRTUAL FUNCTIONS *)
      
      (* Add main class if it has virtual functions *)
      If[Length[vtables] > 0,
        AppendTo[allBases, <|
          "ClassName" -> mainClassName,
          "VTableEntries" -> vtables,
          "IsVirtual" -> False,
          "IsPrimary" -> True  (* FIXED: Main class is always primary *)
        |>];
      ];
      
      (* Add virtual bases - VTableEntries are directly available *)
      Do[
        Module[{vb = vbs[[i]], vtableEntries},
          vtableEntries = vb["VTableEntries"];
          If[Length[vtableEntries] > 0,
            AppendTo[allBases, <|
              "ClassName" -> vb["ClassName"],
              "VTableEntries" -> vtableEntries,
              "IsVirtual" -> True,
              "IsPrimary" -> False
            |>];
          ];
        ]
      , {i, Length[vbs]}];
      
      (* ENHANCED: Extract VTable info from layout structure when classes not available *)
      Module[{baseIndex = 0, primaryBaseName = None},
        (* Identify all non-virtual bases from main layout *)
        Do[
          Module[{item = main[[i]]},
            If[item["Kind"] === "NonVirtualBase",
              baseIndex++;
              Module[{baseClassName = item["Name"], isPrimary = (baseIndex == 1)},
                (* Track primary base name *)
                If[isPrimary, primaryBaseName = baseClassName];
                
                (* ENHANCED: Add secondary bases (not primary) that have vptrs *)
                If[!isPrimary && KeyExistsQ[item, "Layout"],
                  Module[{baseLayout = item["Layout"], hasVptr = False, extractedVTableEntries = {}},
                    (* Check if this base has a vptr and extract VTable entries *)
                    Do[
                      If[baseLayout[[j]]["Kind"] === "Vptr",
                        hasVptr = True;
                        (* Extract VTable entries from the vptr slot itself *)
                        If[KeyExistsQ[baseLayout[[j]], "VTableEntries"],
                          extractedVTableEntries = baseLayout[[j]]["VTableEntries"];
                        ];
                        Break[];
                      ];
                    , {j, Length[baseLayout]}];
                    
                    (* Add secondary base with its own VTable using extracted entries *)
                    If[hasVptr && !MemberQ[allBases[[All, "ClassName"]], baseClassName],
                      If[Length[extractedVTableEntries] > 0,
                        AppendTo[allBases, <|
                          "ClassName" -> baseClassName,
                          "VTableEntries" -> extractedVTableEntries,
                          "IsVirtual" -> False,
                          "IsPrimary" -> False
                        |>];
                      ];
                    ];
                  ];
                ];
              ];
            ];
          ]
        , {i, Length[main]}];
      ];
    ];
    
    (* 2. DETERMINE MERGE LOGIC - Always separate for correct C++ behavior *)
    shouldMerge = False;
    
    (* 3. CREATE VTABLES FOR COLLECTED CLASSES *)
    If[separateVTables && !shouldMerge,
      (* SEPARATE MODE: Individual VTables for classes that need them *)
      
      (* Create VTable for each class with cascading Y positioning *)
      Do[
        Module[{baseInfo = allBases[[i]], vtableEntries, vtableWidth, vtableHeight, newY},
          vtableEntries = baseInfo["VTableEntries"];
          vtableWidth = Max[60, Max[StringLength /@ vtableEntries] * 3];
          vtableHeight = Length[vtableEntries] * vH;
          
          (* CASCADING Y CALCULATION: prev_y - prev_vtable_height - gap *)
          If[Length[vtableStructures] > 0,
            Module[{prevVTable = vtableStructures[[-1]]},
              newY = prevVTable["Position"][[2]] - prevVTable["Height"] - gap;
            ],
            newY = currentY;  (* First VTable uses initial Y *)
          ];
          
          AppendTo[vtableStructures, <|
            "ClassName" -> baseInfo["ClassName"],
            "VTableEntries" -> vtableEntries,
            "Position" -> {currentX, newY},
            "Height" -> vtableHeight  (* Store height for next cascade *)
          |>];
          vtableAnchors[baseInfo["ClassName"]] = {currentX, newY};
          currentX += vtableWidth + gap;  (* Move right for next VTable *)
        ]
      , {i, Length[allBases]}];
      
    , (* MERGED MODE: Single VTable with merged entries *)
      
      If[Length[vtables] > 0,
        Module[{vtableHeight = Length[vtables] * vH},
          AppendTo[vtableStructures, <|
            "ClassName" -> mainClassName,
            "VTableEntries" -> vtables,
            "Position" -> {vtableStartX, vtableY},
            "Height" -> vtableHeight
          |>];
          vtableAnchors[mainClassName] = {vtableStartX, vtableY};
          
          (* In merged mode, all base vptrs should point to main class VTable *)
          Do[
            vtableAnchors[allBases[[i]]["ClassName"]] = {vtableStartX, vtableY};
          , {i, Length[allBases]}];
        ];
      ];
    ];
  ];
  
  (* ENHANCED: Helper function to recursively collect all virtual bases *)
  flattenAllVirtualBases[vbsList_] := Module[{allVbs = {}, processed = {}},
    (* Recursive function to collect virtual bases *)
    Module[{collectVBs},
      collectVBs[vbList_] := Do[
        Module[{vb = vbList[[i]]},
          If[!MemberQ[processed, vb["ClassName"]],
            AppendTo[processed, vb["ClassName"]];
            AppendTo[allVbs, vb];
            (* Recursively process nested virtual bases *)
            If[KeyExistsQ[vb, "VirtualBases"] && Length[vb["VirtualBases"]] > 0,
              collectVBs[vb["VirtualBases"]];
            ];
          ];
        ]
      , {i, Length[vbList]}];
      
      collectVBs[vbsList];
    ];
    allVbs
  ];
  
  (* ENHANCED: Draw virtual base panels with detailed slot display *)
  drawVirtualBases[] := Module[{
    x = gap, 
    currentVbY = vbY,  (* Start at base Y position *)
    vbStructures = {},  (* Track VB positions and heights for cascade *)
    allVirtualBases    (* Flattened list of all virtual bases *)
  },
    (* FIXED: Get all virtual bases recursively *)
    allVirtualBases = flattenAllVirtualBases[vbs];
    
    Do[
      Module[{vb = allVirtualBases[[i]], vbLayout, vbBarWidth, vbX, vbHeight, newVbY, currentX},
        (* Safely extract layout *)
        vbLayout = If[AssociationQ[vb] && KeyExistsQ[vb, "Layout"], vb["Layout"], {}];
        
        (* FIXED: Use slot-type-based width for virtual bases based on content *)
        vbBarWidth = If[Length[vbLayout] > 0, 
          Total[getSlotWidth[#] & /@ vbLayout], 
          classBlockWidth
        ];
        vbHeight = H;  (* Virtual base height (single row) *)
        vbX = gap + mainBarWidth + gap + totalVTableWidth + gap + x;
        
        (* UPWARD CASCADING Y CALCULATION: prev_y + prev_height + gap *)
        If[Length[vbStructures] > 0,
          Module[{prevVb = vbStructures[[-1]]},
            newVbY = prevVb["Position"][[2]] + prevVb["Height"] + gap;
          ],
          newVbY = currentVbY;  (* First virtual base uses initial Y *)
        ];
        
        (* Store VB structure info for next cascade *)
        AppendTo[vbStructures, <|
          "ClassName" -> vb["ClassName"],
          "Position" -> {vbX, newVbY},
          "Width" -> vbBarWidth,
          "Height" -> vbHeight
        |>];
        
        (* Store virtual base anchor with cascaded position *)
        virtualBaseAnchors[vb["ClassName"]] = <|
          "Position" -> {vbX + vbBarWidth/2, newVbY}, 
          "Width" -> vbBarWidth, 
          "LeftEdge" -> vbX
        |>;
        
        (* Draw outer border at cascaded position *)
        AppendTo[graphics, {
          {EdgeForm[{Blue, Dashed, Thickness[0.0015]}], FaceForm[None],
           Rectangle[{vbX, newVbY}, {vbX + vbBarWidth, newVbY + H}]},
          Text["Virtual Base: " <> vb["ClassName"],
               {vbX + vbBarWidth/2, newVbY - 12},
               BaseStyle -> {FontFamily -> "Arial", FontSize -> labelFontSize, 
                            FontColor -> Blue, FontWeight -> Bold}]
        }];
        
        (* Draw each slot with slot-type-based width using drawSlot to register anchors *)
        currentX = vbX;
        Do[
          Module[{slotWidth = getSlotWidth[vbLayout[[j]]]},
            AppendTo[graphics, drawSlot[vbLayout[[j]], currentX, newVbY, slotWidth]];
            currentX += slotWidth;
          ]
          , {j, Length[vbLayout]}];
        
        x += vbBarWidth + gap;  (* Move right for next virtual base *)
      ]
    , {i, Length[allVirtualBases]}]
  ];
  
  (* ENHANCED: Draw connection arrows for multiple VTables *)
  drawArrows[] := Module[{},
    (* 1) ENHANCED: Black L-shaped arrows from vptr to corresponding VTable *)
    If[Length[vptrAnchors] > 0,
      Do[
        Module[{anchorData = vptrAnchors[[i]], src, targetClass, tgt, adjustedSrc, adjustedTgt, midPt, 
                vtableStruct, vtableWidth, vtableHeight, vtablePos},
          src = anchorData[[1]];
          targetClass = anchorData[[2]];
          
          (* Find the corresponding VTable structure to get dimensions *)
          vtableStruct = FirstCase[vtableStructures, 
            s_Association /; s["ClassName"] === targetClass :> s, 
            Missing[]];
          
          If[!MissingQ[vtableStruct],
            (* Calculate VTable dimensions *)
            vtableWidth = Max[60, Max[StringLength /@ vtableStruct["VTableEntries"]] * 3];
            vtableHeight = vtableStruct["Height"];
            vtablePos = vtableStruct["Position"];
            
            (* CORRECTED INTELLIGENT ARROW POSITIONING: upper left if src_x > tgt_x, upper right otherwise *)
            adjustedSrc = {src[[1]], src[[2]] - H/2};  (* Top of vptr slot *)
            adjustedTgt = If[src[[1]] > vtablePos[[1]], 
              {vtablePos[[1]] + vtableWidth, vtablePos[[2]] + vtableHeight},   (* Upper right corner *)
              {vtablePos[[1]], vtablePos[[2]] + vtableHeight}        (* Upper left corner *)
            ];
            midPt = {adjustedSrc[[1]], adjustedTgt[[2]]};
          
            AppendTo[arrows, {
              Black, Thickness[arrowThickness], Arrowheads[{0, arrowHeadSize}],
              Arrow[{adjustedSrc, midPt, adjustedTgt}]
            }];
          ];
        ],
        {i, Length[vptrAnchors]}
      ];
    ];
    
    (* 2) ENHANCED: Blue L-shaped arrows from vbase slots to virtual bases *)
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
                
                (* CORRECTED INTELLIGENT ARROW POSITIONING WITH LEFT OFFSET: shifted left by vbWidth *)
                adjustedSrc = {src[[1]], src[[2]]};  (* Bottom of vbase slot *)
                adjustedTgt = If[src[[1]] > leftEdge, 
                  {leftEdge - vbWidth, tgt[[2]] + H},              (* Upper left corner shifted left by vbWidth *)
                  {leftEdge, tgt[[2]] + H}                         (* Upper right corner shifted left by vbWidth *)
                ];
                midPt = {adjustedSrc[[1]], adjustedTgt[[2]]};
                    
                AppendTo[arrows, {
                  Blue, Thickness[arrowThickness], Arrowheads[{0, arrowHeadSize}],
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
  
  (* UPDATE: Calculate actual dimensions including cascaded virtual bases AND all VTables *)
  updateDimensions[] := Module[{actualVTableWidth = 0, maxVbY = vbY, maxVTableY = 0, localMinVTableY = 0, calculatedTotalWidth},
    (* Calculate actual total width of all VTables *)
    If[Length[vtableStructures] > 0,
      Module[{minX = Infinity, maxX = -Infinity},
        Do[
          Module[{vtableStruct = vtableStructures[[i]], pos, vtableWidth},
            pos = vtableStruct["Position"];
            vtableWidth = Max[60, Max[StringLength /@ vtableStruct["VTableEntries"]] * 3];
            minX = Min[minX, pos[[1]]];
            maxX = Max[maxX, pos[[1]] + vtableWidth];
          ]
        , {i, Length[vtableStructures]}];
        actualVTableWidth = maxX - minX;
      ];
    ];
    
    (* FIXED: Calculate Y range of all VTables for proper plot range *)
    If[Length[vtableStructures] > 0,
      Do[
        Module[{vtableStruct = vtableStructures[[i]], pos, vtableHeight},
          pos = vtableStruct["Position"];
          vtableHeight = vtableStruct["Height"];
          localMinVTableY = Min[localMinVTableY, pos[[2]]];                    (* Lowest VTable Y *)
          maxVTableY = Max[maxVTableY, pos[[2]] + vtableHeight];     (* Highest VTable Y *)
        ]
      , {i, Length[vtableStructures]}];
    ];
    
    (* Calculate maximum Y position of cascaded virtual bases *)
    If[Length[vbs] > 0,
      Module[{currentVbY = vbY, x = gap},
        Do[
          Module[{vb = vbs[[i]], vbLayout, vbHeight},
            vbLayout = If[AssociationQ[vb] && KeyExistsQ[vb, "Layout"], vb["Layout"], {}];
            vbHeight = H;
            
            (* Calculate cascaded Y position *)
            If[i > 1,
              currentVbY = currentVbY + vbHeight + gap;
            ];
            
            (* Track maximum Y *)
            maxVbY = Max[maxVbY, currentVbY + vbHeight];
          ]
        , {i, Length[vbs]}];
      ];
    ];
    
    (* CRITICAL FIX: Calculate total width properly *)
    calculatedTotalWidth = gap + mainBarWidth + gap + actualVTableWidth + gap + totalVbWidth + gap;
    
    (* Update outer scope variables - CRITICAL FIX *)
    minVTableY = localMinVTableY;
    totalVTableWidth = actualVTableWidth;
    totalWidth = calculatedTotalWidth;
    
    (* FIXED: Include VTable Y range in total height calculation *)
    totalHeight = Max[
      gap + If[vbs === {}, 0, maxVbY - vbY + gap] + H + gap,              (* VB height *)
      gap + maxVTableY + gap,                                             (* Top VTable *)
      Abs[minVTableY] + maxVTableY + 2*gap                               (* Full VTable range *)
    ];
  ];
  
  (* Build the complete diagram *)
  collectVTableStructures[];
  updateDimensions[];  (* FIXED: Update dimensions after VTable collection *)
  
  (* --- ENSURE VTABLE ANCHORS ARE ALWAYS REGISTERED --- *)
  If[Length[vtableStructures] > 0,
    Do[
      Module[{vtableStruct = vtableStructures[[i]]},
        vtableAnchors[vtableStruct["ClassName"]] = vtableStruct["Position"];
      ],
      {i, Length[vtableStructures]}
    ];
  ];
  
  (* --- DYNAMIC SCALING FOR ARROWS AND FONTS (AFTER FINAL DIMENSIONS) --- *)
  diagramRefSize = Max[totalWidth, totalHeight, 400];
  arrowThickness = 0.0025 * (400/diagramRefSize);
  arrowHeadSize = 0.03 * (400/diagramRefSize);
  labelFontSize = baseLabelFontSize * (400/diagramRefSize);
  
  drawVirtualBases[];
  drawMainBar[];
  drawVTables[];
  drawArrows[];
  
  Show[
    Graphics[graphics, Background -> White],
    Graphics[arrows],
    PlotRange -> All,
    PlotRangePadding -> Scaled[0.08],
    ImageSize -> {mainBarWidth + totalVbWidth + 400, 800},
    Background -> White,
    Axes -> False,
    Frame -> False,
    ImagePadding -> 10
  ]
];

(* --- Helper function for rescaling diagrams --- *)
RescaleClassDiagram[diagram_Graphics, width_:800, height_:600] := Show[
  diagram,
  PlotRange -> All,              (* Use full data range of all primitives *)
  PlotRangePadding -> 1,      (* No extra margin around range *)
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
  
  DrawClassDiagram[layout, SeparateVTables -> True, cls]
];

ParseAndDrawClassLayout[path_String] := Module[{cls, layout, choice },
  cls = ParseCppToClasses[path];
  If[cls === $Failed, Return[$Failed]];
  
  If[Length[Keys[cls]] == 0,
    Message[ParseAndDrawClassLayout::noclasses, path];
    Return[$Failed]
  ];
  
  choice = ChoiceDialog["Select class to visualize", Thread[Keys[cls] -> Keys[cls]]];
  If[!StringQ[choice], Return[$Canceled]];
  
  layout = ComputeClassLayout[cls, choice];
  If[layout === $Failed, Return[$Failed]];
  
  DrawClassDiagram[layout, SeparateVTables -> True, cls]
];

ParseAndDrawClassLayout[path_String, className_String] := Module[{cls, layout},
  cls = ParseCppToClasses[path]; 
  If[cls === $Failed, Return[$Failed]];
  
  layout = ComputeClassLayout[cls, className];
  If[layout === $Failed, Return[$Failed]];
  
  DrawClassDiagram[layout, SeparateVTables -> True, cls]
];

ParseAndDrawClassLayout::noclasses = "No classes found in file `1`.";

(* --- Simple wrapper for easy usage --- *)
DrawClassDiagramSimple[layout_] := DrawClassDiagram[layout, SeparateVTables -> True, None];

(* --- Alternative entry point that always shows separate VTables --- *)
DrawLayout[layout_] := DrawClassDiagram[layout, SeparateVTables -> True, None];

End[]; 
EndPackage[];
