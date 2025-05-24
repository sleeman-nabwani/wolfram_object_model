#!/usr/bin/env wolframscript
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
BeginPackage["CppClassLayoutParser`"];

ParseCppToClasses::usage =
  "ParseCppToClasses[file_String] imports a C++ header using Clang's \"-ast-dump=json\"\n" <>
  "and returns an Association of classes -> <| \"Bases\"->{\[Ellipsis]}, \"Virtual\"->{\[Ellipsis]} |>.";

ComputeLayout::usage =
  "ComputeLayout[classes_Association] takes the output of ParseCppToClasses and\n" <>
  "computes a list of subobject entries <| \"Class\"->name, \"Offset\"->n, \[Ellipsis] |>.";

DrawLayout::usage =
  "DrawLayout[layout_List] renders a box diagram (subobjects at integer offsets).";

ParseAndDrawLayout::usage =
  "ParseAndDrawLayout[file_String] parses the given C++ header path, computes the layout, and draws it.\n" <>
  "ParseAndDrawLayout[] opens a file-picker dialog.";

Begin["`Private`"];

ParseCppToClasses[file_String]:=Module[{json,ast,records,makeEntry},
(*Call Clang to produce JSON AST*)json=RunProcess[{"clang","-std=c++17","-fsyntax-only","-Xclang","-ast-dump=json",file},"StandardOutput"];
ast=ImportString[json,"RawJSON"];
(*Find all class record declarations with names*)records=Cases[ast,node_Association/;node["kind"]==="CXXRecordDecl"&&KeyExistsQ[node,"name"]:>node,Infinity];
(*Convert each record to an entry in the classes Association*)makeEntry[rec_]:=Module[{clsName,baseSpecs,allBases,virtBases},clsName=rec["name"];
baseSpecs=Cases[rec,bs_Association/;bs["kind"]==="CXXBaseSpecifier":>bs,Infinity];
allBases=Flatten@Cases[baseSpecs,bs_Association:>Cases[bs,tr_Association/;tr["kind"]==="TypeRef"&&KeyExistsQ[tr,"name"]:>tr["name"],1],1];
virtBases=Flatten@Cases[baseSpecs,bs_Association/;TrueQ[bs["isVirtual"]]:>Cases[bs,tr_Association/;tr["kind"]==="TypeRef"&&KeyExistsQ[tr,"name"]:>tr["name"],1],1];
clsName-><|"Bases"->allBases,"Virtual"->virtBases|>];
Association[makeEntry/@records]];

ComputeLayout[classes_Association]:=Module[{visited=<||>,offset=0,spots={},unifyVirtual,dfs},unifyVirtual[cls_]:=Module[{vbs},vbs=Lookup[classes[cls],"Virtual",{}];
Scan[If[!KeyExistsQ[visited,#],visited[#]=True;dfs[#]]&,vbs];];
     (*1. place virtual bases once*)dfs[cls_]:=(unifyVirtual[cls];
(*2. expand non-virtual bases*)Scan[If[!MemberQ[Lookup[classes[cls],"Virtual",{}],#],dfs[#]]&,Lookup[classes[cls],"Bases",{}]];
(*3. record this subobject*)AppendTo[spots,<|"Class"->cls,"Offset"->offset,"HasVPtr"->True,"VBases"->Lookup[classes[cls],"Virtual",{}]|>];
offset+=1;);
(*Start from roots (classes that are not a base of any other)*)KeyDrop[classes,Flatten@Lookup[classes,"Bases",{}]]//Keys//Scan[dfs];
spots];

DrawLayout[layout_List] := Module[
  {boxes, maxOffset},
  
  (* 1. Compute the maximum offset for sizing *)
  maxOffset = Max[layout[[All, "Offset"]]];
  
  (* 2. Build a Graphics for each subobject *)
  boxes = Map[
    With[{o = #["Offset"], cls = #["Class"], vbs = #["VBases"]},
      Graphics[
        {
          (* the box representing the subobject *)
          Rectangle[{o, 0}, {o + 1, 1}],
          (* the class name *)
          Text[Style[cls, Bold], {o + 0.5, 0.7}],
          (* optional vbase arrow annotation *)
          If[
            vbs =!= {},
            Text["vbase\[RightArrow]" <> StringRiffle[vbs, ","], {o + 0.5, 0.3}],
            {}
          ]
        },
        (* options *)
        PlotRange    -> {{0, maxOffset + 1}, {0, 1}},
        ImagePadding -> 20
      ]
    ] &,
    layout
  ];
  
  (* 3. Overlay and show them all *)
  Show[boxes]
];



ParseAndDrawLayout[file_String]:=Module[{classes,layout},classes=ParseCppToClasses[file];
layout=ComputeLayout[classes];
DrawLayout[layout];];

(*optional interactive picker*)
ParseAndDrawLayout[]:=Module[{file},file=SystemDialogInput["FileOpen","C++ Header Files"->{"hpp","h"}];
If[StringQ[file],ParseAndDrawLayout[file],Message[ParseAndDrawLayout::cancelled];$Failed]];

End[]; (*`Private`*)
EndPackage[];

