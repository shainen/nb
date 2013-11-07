(* ::Package:: *)

(* ::Section::Closed:: *)
(*Constants*)


(* ::Input:: *)
(*Get["constants.m"]*)


(* ::Input:: *)
(*Runs=100;*)
(*mSIM=10;*)


(* ::Input:: *)
(*su2outfile="data/su2S"<>ToString[Sites]<>"t"<>ToString[tmax]<>"j"<>ToString[Jvalue]<>"r"<>ToString[Runs]<>".lst";*)


(* ::Section::Closed:: *)
(*Dynamic Equations*)


(* ::Input:: *)
(*vs=Table[Table[s[m,n][t],{n,1,3}],{m,1,Sites}];*)
(*ds=Table[Table[s[m,n]'[t],{n,1,3}],{m,1,Sites}];*)
(*initialss:=Flatten[Table[Table[s[m,n][0]==Subscript[s, 0][m,n],{n,1,3}],{m,1,Sites}]];*)
(*\[CapitalDelta]s[m_,ow_]:=Table[D[ow,s[m,n][t]],{n,1,3}];*)


(* ::Input:: *)
(*sw[m_,ow_]:=vs[[m]]  ow-I/2 vs[[m]] \[Cross]\[CapitalDelta]s[m,ow]-1/8 (\[CapitalDelta]s[m,ow]+Table[vs[[m]] .\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]],{n,1,3}]-1/2 vs[[m]]  Sum[\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]][[n]],{n,1,3}]);*)
(*swb[m_,ow_]:=vs[[m]]  ow+I/2 vs[[m]] \[Cross]\[CapitalDelta]s[m,ow]-1/8 (\[CapitalDelta]s[m,ow]+Table[vs[[m]] .\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]],{n,1,3}]-1/2 vs[[m]]  Sum[\[CapitalDelta]s[m,\[CapitalDelta]s[m,ow][[n]]][[n]],{n,1,3}]);*)


(* ::Input:: *)
(*Hs[m_,ow_]:=U/2 sw[m,sw[m,ow][[3]]][[3]]-J navg (sw[m,ow][[1]](Sum[sw[n,1][[1]],{n,1,m-1}]+Sum[sw[n,1][[1]],{n,m+1,Sites}])+sw[m,ow][[2]](Sum[sw[n,1][[2]],{n,1,m-1}]+Sum[sw[n,1][[2]],{n,m+1,Sites}]))-\[Mu] sw[m,ow][[3]];*)
(*Hsb[m_,ow_]:=U/2 swb[m,swb[m,ow][[3]]][[3]]-J navg (swb[m,ow][[1]](Sum[swb[n,1][[1]],{n,1,m-1}]+Sum[swb[n,1][[1]],{n,m+1,Sites}])+swb[m,ow][[2]](Sum[swb[n,1][[2]],{n,1,m-1}]+Sum[swb[n,1][[2]],{n,m+1,Sites}]))-\[Mu] swb[m,ow][[3]];*)


(* ::Input:: *)
(*eqnss1=Table[ds[[1,n]]==Simplify[I (Hs[1,vs[[1,n]]]-Hsb[1,vs[[1,n]]])],{n,1,3}]*)


(* ::Input:: *)
(*eqnss:=Flatten[Table[eqnss1/.Flatten[Table[{s[1,n]'[t]->s[m,n]'[t],s[1,n][t]->s[m,n][t],s[m,n][t]->s[1,n][t]},{n,1,3}]],{m,1,Sites}]];*)


(* ::Input:: *)
(*MatrixForm[eqnss]*)


(* ::Section:: *)
(*Wigner with neg prob*)


(* ::Input:: *)
(*svarsingreek={1/2 (\[Beta] Conjugate[\[Alpha]]+\[Alpha] Conjugate[\[Beta]]),1/2 (-I \[Beta] Conjugate[\[Alpha]]+I \[Alpha] Conjugate[\[Beta]]),1/2 (\[Alpha] Conjugate[\[Alpha]]-\[Beta] Conjugate[\[Beta]])};*)


(* ::Input:: *)
(*Norm1=(4-Sqrt[E])/Sqrt[E];*)


(* ::Input:: *)
(*Norm2=E^(-1-1/Sqrt[2]) (4 (2+Sqrt[2])+4 (-2+Sqrt[2]) E^Sqrt[2]+E^(1+1/Sqrt[2]));*)


(* ::Input:: *)
(*Fock1Mag=ProbabilityDistribution[4 a E^(-2 a^2) Abs[(4a^2-1)]/Norm1,{a,0,\[Infinity]}];*)


(* ::Input:: *)
(*Fock2Mag=ProbabilityDistribution[4 a E^(-2 a^2) Abs[(1-8 a^2+8 a^4)]/Norm2,{a,0,\[Infinity]}];*)


(* ::Input:: *)
(*randmag1=RandomVariate[Fock1Mag,{2,Sites,Runs}];*)


(* ::Input:: *)
(*randmag2=RandomVariate[Fock2Mag,{Sites,Runs}];*)


(* ::Input:: *)
(*metric1=1-2UnitStep[1/2-randmag1];*)


(* ::Input:: *)
(*metric2=1-2UnitStep[randmag2-Sqrt[2-Sqrt[2]]/2]*UnitStep[Sqrt[2+Sqrt[2]]/2-randmag2];*)


(* ::Input:: *)
(*randphase=RandomReal[{0,2\[Pi]},{2,Sites,Runs}];*)


(* ::Input:: *)
(*randrest=Table[RandomVariate[NormalDistribution[0,1/2]]+I RandomVariate[NormalDistribution[0,1/2]],{Sites},{Runs}];*)


(* ::Input:: *)
(*randupinit[n_]:={randmag2[[n]] E^(I randphase[[1,n]]),randrest[[n]]};*)
(*randmiinit[n_]:={randmag1[[1,n]] E^(I randphase[[1,n]]),randmag1[[2,n]] E^(I randphase[[2,n]])};*)
(*randdoinit[n_]:={randrest[[n]],randmag2[[n]] E^(I randphase[[1,n]])};*)


(* ::Input:: *)
(*(*randinits={randupinit[1],randmiinit[2],randdoinit[3],randupinit[4],randmiinit[5],randdoinit[6]};*)*)


(* ::Input:: *)
(*randinits={randupinit[1],randmiinit[2],randdoinit[3]};*)


(* ::Input:: *)
(*Dimensions[randinits]*)


(* ::Input:: *)
(*ResNorm=Norm2^4 Norm1^4;*)


(* ::Input:: *)
(*ProgressIndicator[Dynamic[j],{0,(Runs/mSIM-1)}]*)


(* ::Input:: *)
(*spindyn=Table[0,{Nt},{Sites},{3}];*)
(*runsdone=0;*)
(*Timing[*)
(*Do[*)
(*TWAres=ParallelTable[*)
(*Product[metric1[[n,m,mSIM j+ii]],{n,1,2},{m,{2}}]Product[metric2[[m,mSIM j+ii]],{m,{1,3}}]vs/.NDSolve[(eqnss~Join~initialss)/.{U->Uvalue,\[Mu]->\[Mu]value,J->Jvalue,navg->navgvalue}~Join~Flatten[Table[Subscript[s, 0][m,n]->(svarsingreek[[n]]/.{\[Alpha]->randinits[[m,1,mSIM j+ii]],\[Beta]->randinits[[m,2,mSIM j+ii]]}),{m,1,Sites},{n,1,3}]],Flatten[vs],{t,0,tmax},MaxSteps->\[Infinity]*)
(*],*)
(*{ii,mSIM}*)
(*];*)
(*spindyn=spindyn+Total[Table[TWAres[[All,1,All]],{t,ts}]\[Transpose]];*)
(*runsdone=runsdone+1;*)
(*,{j,0,(Runs/mSIM-1)}*)
(*]*)
(*]*)
(*avgspins=ResNorm Re[spindyn]/Runs;*)


(* ::Input:: *)
(*Save[su2outfile,avgspins]*)


(* ::Input:: *)
(*Dimensions[avgspins]*)


(* ::Input:: *)
(*ListPlot[{{times,avgspins[[All,1,3]]}\[Transpose],{times,avgspins[[All,1,3]]}\[Transpose]},Joined->True,PlotRange->All]*)


(* ::Input:: *)
(*ListPlot[{{times,avgspins[[All,2,3]]}\[Transpose],{times,avgspins[[All,2,3]]}\[Transpose]},Joined->True,PlotRange->All]*)


(* ::Input:: *)
(*ListPlot[{{times,avgspins[[All,3,3]]}\[Transpose],{times,avgspins[[All,3,3]]}\[Transpose]},Joined->True,PlotRange->All]*)
