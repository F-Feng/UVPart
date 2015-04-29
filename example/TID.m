(* ::Package:: *)

Needs["HighEnergyPhysics`fc`"];


(* ::Section:: *)
(*Clear Functions*)


Clear[$TID];


(* ::Section:: *)
(*TID Code*)


(* ::Subsection:: *)
(*Distribute*)


$TID[c_,k_,ps_List]/;FreeQ[c,k]||FreeQ[c,_LorentzIndex]=c;
$TID[Longest[c_.] (exp1_+exp2_),k_,ps_List]:=$TID[c exp1,k,ps]+$TID[c exp2,k,ps];
$TID[Longest[c_] exp_,k_,ps_List]/;FreeQ[c,k]||FreeQ[c,_LorentzIndex]:=c $TID[exp,k,ps];


(* ::Subsection:: *)
(*Using TIDL in FeynCalc*)


$TID[FVD[k_,\[Mu]_]//FCI,k_,ps_List]:=TIDL[{k,\[Mu]},ps]


$TID[FVD[k_,\[Mu]1_] FVD[k_,\[Mu]2_]//FCI,k_,ps_List]:=TIDL[{{k,\[Mu]1},{k,\[Mu]2}},ps]


$TID[FVD[k_,\[Mu]1_] FVD[k_,\[Mu]2_] FVD[k_,\[Mu]3_]//FCI,k_,ps_List]:=TIDL[{{k,\[Mu]1},{k,\[Mu]2},{k,\[Mu]3}},ps]


$TID[FVD[k_,\[Mu]1_] FVD[k_,\[Mu]2_] FVD[k_,\[Mu]3_] FVD[k_,\[Mu]4_]//FCI,k_,ps_List]:=TIDL[{{k,\[Mu]1},{k,\[Mu]2},{k,\[Mu]3},{k,\[Mu]4}},ps]


$TID[FVD[k_,\[Mu]1_] FVD[k_,\[Mu]2_] FVD[k_,\[Mu]3_] FVD[k_,\[Mu]4_] FVD[k_,\[Mu]5_]//FCI,k_,ps_List]:=TIDL[{{k,\[Mu]1},{k,\[Mu]2},{k,\[Mu]3},{k,\[Mu]4},{k,\[Mu]5}},ps]
