(* ::Package:: *)

(* ::Section::Closed:: *)
(*Constants*)


Get["constants.m"]


(* ::Section::Closed:: *)
(*Dynamic Equations*)


vs=Table[Table[s[m,n][t],{n,1,3}],{m,1,Sites}];
ds=Table[Table[s[m,n]'[t],{n,1,3}],{m,1,Sites}];
initialss:=Flatten[Table[Table[s[m,n][0]==Subscript[s, 0][m,n],{n,1,3}],{m,1,Sites}]];
\[CapitalDelta]s[m_,ow_]:=Table[D[ow,s[m,n][t]],{n,1,3}];


sw[m_,ow_]:=vs[[m]]  ow-I/2 vs[[m]] \[Cross]\[CapitalDelta]s[m,ow]-1/8 (\[CapitalDelta]s[m,ow]+Table[vs[[m]] .\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]],{n,1,3}]-1/2 vs[[m]]  Sum[\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]][[n]],{n,1,3}]);
swb[m_,ow_]:=vs[[m]]  ow+I/2 vs[[m]] \[Cross]\[CapitalDelta]s[m,ow]-1/8 (\[CapitalDelta]s[m,ow]+Table[vs[[m]] .\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]],{n,1,3}]-1/2 vs[[m]]  Sum[\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]][[n]],{n,1,3}]);


Hs[m_,ow_]:=U/2 sw[m,sw[m,ow][[3]]][[3]]-J navg (sw[m,ow][[1]](Sum[sw[n,1][[1]],{n,1,m-1}]+Sum[sw[n,1][[1]],{n,m+1,Sites}])+sw[m,ow][[2]](Sum[sw[n,1][[2]],{n,1,m-1}]+Sum[sw[n,1][[2]],{n,m+1,Sites}]))-\[Mu] sw[m,ow][[3]];
Hsb[m_,ow_]:=U/2 swb[m,swb[m,ow][[3]]][[3]]-J navg (swb[m,ow][[1]](Sum[swb[n,1][[1]],{n,1,m-1}]+Sum[swb[n,1][[1]],{n,m+1,Sites}])+swb[m,ow][[2]](Sum[swb[n,1][[2]],{n,1,m-1}]+Sum[swb[n,1][[2]],{n,m+1,Sites}]))-\[Mu] swb[m,ow][[3]];


eqnss1=Table[ds[[1,n]]==Simplify[I (Hs[1,vs[[1,n]]]-Hsb[1,vs[[1,n]]])],{n,1,3}]


eqnss:=Flatten[Table[eqnss1/.Flatten[Table[{s[1,n]'[t]->s[m,n]'[t],s[1,n][t]->s[m,n][t],s[m,n][t]->s[1,n][t]},{n,1,3}]],{m,1,Sites}]];


MatrixForm[eqnss]


(* ::Section:: *)
(*Wigner with neg prob*)


svarsingreek={1/2 (\[Beta] Conjugate[\[Alpha]]+\[Alpha] Conjugate[\[Beta]]),1/2 (-I \[Beta] Conjugate[\[Alpha]]+I \[Alpha] Conjugate[\[Beta]]),1/2 (\[Alpha] Conjugate[\[Alpha]]-\[Beta] Conjugate[\[Beta]])};


Norm1=(4-Sqrt[E])/Sqrt[E];


Norm2=E^(-1-1/Sqrt[2]) (4 (2+Sqrt[2])+4 (-2+Sqrt[2]) E^Sqrt[2]+E^(1+1/Sqrt[2]));


Fock1Mag=ProbabilityDistribution[4 a E^(-2 a^2) Abs[(4a^2-1)]/Norm1,{a,0,\[Infinity]}];


Fock2Mag=ProbabilityDistribution[4 a E^(-2 a^2) Abs[(1-8 a^2+8 a^4)]/Norm2,{a,0,\[Infinity]}];


randmag1=RandomVariate[Fock1Mag,{2,Sites,su2Runs}];


randmag2=RandomVariate[Fock2Mag,{Sites,su2Runs}];


metric1=1-2UnitStep[1/2-randmag1];


metric2=1-2UnitStep[randmag2-Sqrt[2-Sqrt[2]]/2]*UnitStep[Sqrt[2+Sqrt[2]]/2-randmag2];


randphase=RandomReal[{0,2\[Pi]},{2,Sites,su2Runs}];


randrest=Table[RandomVariate[NormalDistribution[0,1/2]]+I RandomVariate[NormalDistribution[0,1/2]],{Sites},{su2Runs}];


randupinit[n_]:={randmag2[[n]] E^(I randphase[[1,n]]),randrest[[n]]};
randmiinit[n_]:={randmag1[[1,n]] E^(I randphase[[1,n]]),randmag1[[2,n]] E^(I randphase[[2,n]])};
randdoinit[n_]:={randrest[[n]],randmag2[[n]] E^(I randphase[[1,n]])};


(*randinits={randupinit[1],randmiinit[2],randdoinit[3],randupinit[4],randmiinit[5],randdoinit[6]};*)


randinits=Table[randupinit[n],{n,1,updownmiddle[[1]]}]~Join~Table[randdoinit[n],{n,updownmiddle[[1]]+1,updownmiddle[[1]]+updownmiddle[[2]]}]~Join~Table[randmiinit[n],{n,updownmiddle[[1]]+updownmiddle[[2]]+1,updownmiddle[[1]]+updownmiddle[[2]]+updownmiddle[[3]]}];


ResNorm=Norm2^(updownmiddle[[1]]+updownmiddle[[2]]) Norm1^(2updownmiddle[[3]]);


(*ProgressIndicator[Dynamic[j],{0,(su2Runs/mSIM-1)}]*)


spindyn=Table[0,{Nt},{Sites},{3}];
su2Runsdone=0;
Timing[
Do[
TWAres=ParallelTable[
Product[metric1[[n,m,mSIM j+ii]],{n,1,2},{m,updownmiddle[[1]]+updownmiddle[[2]]+1,updownmiddle[[1]]+updownmiddle[[2]]+updownmiddle[[3]]}]Product[metric2[[m,mSIM j+ii]],{m,1,updownmiddle[[1]]+updownmiddle[[2]]}]vs/.NDSolve[(eqnss~Join~initialss)/.{U->Uvalue,\[Mu]->\[Mu]value,J->Jvalue,navg->navgvalue}~Join~Flatten[Table[Subscript[s, 0][m,n]->(svarsingreek[[n]]/.{\[Alpha]->randinits[[m,1,mSIM j+ii]],\[Beta]->randinits[[m,2,mSIM j+ii]]}),{m,1,Sites},{n,1,3}]],Flatten[vs],{t,0,tmax},MaxSteps->\[Infinity]
],
{ii,mSIM}
];
spindyn=spindyn+Total[Table[TWAres[[All,1,All]],{t,ts}]\[Transpose]];
su2Runsdone=su2Runsdone+1;
,{j,0,(su2Runs/mSIM-1)}
]
]
avgspins2=ResNorm Re[spindyn]/su2Runs;


Save[su2outfile,avgspins2]
