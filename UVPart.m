(* ::Package:: *)

Needs["HighEnergyPhysics`fc`"];


(* ::Section:: *)
(*Helpers*)


BeginPackage["Private`"];
Off[General::shdw];
Needs["Combinatorica`"];
EndPackage[];


$SPD[idx_List]:=If[OddQ[Length[idx]],0,Product[SPD[idx[[2$i-1]],idx[[2$i]]],{$i,Length[idx]/2}]];


$Exponent[exp_,k_]:=Module[
{tmp,\[Zeta]},
tmp=exp//FCI//ScalarProductExpand//MomentumExpand;
tmp=tmp/.Momentum[k,dim_]:>\[Zeta] Momentum[k,dim];
tmp=tmp//ScalarProductExpand//MomentumExpand;
tmp=Exponent[tmp,\[Zeta]];
Return[tmp];
];


$gNumberFactor[n_]:=With[{nn=n/2},n!/(2^nn nn!)];
$gSquareFactor[n_]:=With[{nn=n/2},$gNumberFactor[n] Product[D+2($i-1),{$i,nn}]];
$preFactor[n_]:=With[{nn=n/2},2^nn nn! Product[D+2($i-1),{$i,nn}]];


(* ::Section:: *)
(*Recursive Definitions*)


$UVPart[c_. (exp1_+exp2_),k_]:=$UVPart[c exp1,k]+$UVPart[c exp2,k];
$UVPart[c_ exp_,k_]/;FreeQ[c,k]:=c $UVPart[exp,k];
$UVPart[c_,k_]/;FreeQ[c,k]:=0;


$UVPart[(p:c_. FCI[SPD[k_]]+r_)^(n_Integer/;n<0) exp_.,k_]/;FreeQ[r,SPD[k]//FCI]&&FreeQ[c,k]:=Module[
{tmp,nexp},
tmp=Factor[p^n exp];
nexp=4+$Exponent[Numerator[tmp],k]-$Exponent[Denominator[tmp],k];
If[nexp<0,Return[0]];
tmp=$UVPart[(p^(n+1) exp)/(c SPD[k]//FCI),k]-$UVPart[(r p^n exp)/(c SPD[k]//FCI),k];
Return[tmp];
];


$SPDQ[k_]:=Return[MatchQ[#,SPD[k,p_]^(n_Integer:1)/;p=!=k&&n>0//FCI]&];

$UVPart[exp_,k_]:=Module[
{tmp,nexp,ns,ps,nsum,pren,pidx,perm,VF,pat},
tmp=Factor[exp];
nexp=4+$Exponent[Numerator[tmp],k]-$Exponent[Denominator[tmp],k];
If[nexp<0,Return[0]];

If[MatchQ[tmp,(RepeatedNull[_?($SPDQ[k])]) SPD[k]^n_Integer//FCI],
pat=VF[tmp]/.VF[(p:RepeatedNull[_?($SPDQ[k])]) SPD[k]^n_Integer//FCI]:>{List[p],n};
tmp=(VF[#]/.VF[SPD[k,px_]^(nx_Integer:1)//FCI]:>{nx,px})&/@(pat[[1]]);
ns=Table[tmp[[i,1]],{i,tmp//Length}];
ps=Table[tmp[[i,2]],{i,tmp//Length}];
nsum=Plus@@ns;
If[OddQ[nsum],Return[0]];
pren=$UV[SPD[k]^(nsum/2+pat[[2]])//FCI,k];
If[pren===0,Return[0]];
pren=Product[$n!,{$n,ns}]/$preFactor[nsum] pren;
pidx=Table[Table[ps[[$j]],{$i,ns[[$j]]}],{$j,Length[ns]}]//Flatten;
perm=Combinatorica`DistinctPermutations[pidx];
tmp=Sum[$SPD[$p]//FCI,{$p,perm}];
tmp=pren tmp;
Return[tmp];
];

If[exp=!=tmp,Return[$UVPart[tmp,k]]];
Return[$UV[exp,k]];
]


(* ::Section:: *)
(*Specific Definition*)


$UV[1,k_]:=0;


$UV[FCI[SPD[k_]]^(n_Integer:1),k_]:=If[n==-2,1/\[Omega],0];
