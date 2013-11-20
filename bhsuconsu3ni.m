(* ::Package:: *)

(* ::Section::Closed:: *)
(*Kernels*)

LaunchKernels[]


Needs["SubKernels`RemoteKernels`"]


LaunchKernels[RemoteMachine["server",7]]


LaunchKernels[RemoteMachine["node04",8]]


(* ::Section::Closed:: *)
(*Init*)


(*rotation*)
R={{1/Sqrt[2],0,0,0,0,1/Sqrt[2],0,0},{0,1/Sqrt[2],0,0,0,0,1/Sqrt[2],0},{0,0,1/2,0,0,0,0,Sqrt[3]/2},{0,0,0,1,0,0,0,0},{0,0,0,0,1,0,0,0},{-1/Sqrt[2],0,0,0,0,1/Sqrt[2],0,0},{0,-1/Sqrt[2],0,0,0,0,1/Sqrt[2],0},{0,0,-Sqrt[3]/2,0,0,0,0,1/2}};


(*Definition of the f-tensor*)
f[i_,j_,k_]:=0;

f[1,2,3]=f[2,3,1]=f[3,1,2]=1;
 
f[1,3,2]=f[2,1,3]=f[3,2,1]=-1;

f[1,4,7]= f[4,7,1]=f[7,1,4]=
f[1,6,5]=f[6,5,1]=f[5,1,6]=
 f[2,4,6]= f[4,6,2]= f[6,2,4]=
f[2,5,7]=f[5,7,2]=f[7,2,5]=
 f[3,4,5]= f[4,5,3]= f[5,3,4]=
 f[3,7,6]=  f[7,6,3]= f[6,3,7]=1/2;

f[1,7,4]=f[7,4,1]=f[4,1,7]=
f[1,5,6]=f[5,6,1]=f[6,1,5]=
 f[2,6,4]= f[6,4,2]= f[4,2,6]=
f[2,7,5]=f[7,5,2]=f[5,2,7]=
 f[3,5,4]= f[5,4,3]= f[4,3,5]=
 f[3,6,7]= f[6,7,3]= f[7,3,6]=-1/2;


f[4,5,8]=f[5,8,4]=f[8,4,5]=
 f[6,7,8]= f[7,8,6]= f[8,6,7]=Sqrt[3]/2; 

f[4,8,5]=f[8,5,4]=f[5,4,8]=
 f[6,8,7]= f[8,7,6]= f[7,6,8]=-Sqrt[3]/2; 

(*Definition of the d-tensor*)

d[i_,j_,k_]=0;
d[1,1,8]=d[1,8,1]=d[8,1,1]=
d[2,2,8]=d[2,8,2]=d[8,2,2]=
d[3,3,8]=d[3,8,3]=d[8,3,3]=1/Sqrt[3];
d[8,8,8]=-(1/Sqrt[3]);
d[1,4,6]=d[1,6,4]=d[4,1,6]=d[4,6,1]=d[6,1,4]=d[6,4,1]=
d[1,5,7]=d[1,7,5]=d[5,1,7]=d[5,7,1]=d[7,1,5]=d[7,5,1]=
d[2,5,6]=d[2,6,5]=d[5,2,6]=d[5,6,2]=d[6,2,5]=d[6,5,2]=
d[3,4,4]=d[4,3,4]=d[4,4,3]=
d[3,5,5]=d[5,3,5]=d[5,5,3]=1/2;
d[2,4,7]=d[2,7,4]=d[4,2,7]=d[4,7,2]=d[7,2,4]=d[7,4,2]=
d[3,6,6]=d[6,3,6]=d[6,6,3]=
d[3,7,7]=d[7,3,7]=d[7,7,3]=-(1/2);
d[4,4,8]=d[4,8,4]=d[8,4,4]=
d[5,5,8]=d[5,8,5]=d[8,5,5]=
d[6,6,8]=d[6,8,6]=d[8,6,6]=
d[7,7,8]=d[7,8,7]=d[8,7,7]=-(1/(2Sqrt[3]));


(* ::Section::Closed:: *)
(*Constants*)


Get["constants.m"]


(* ::Section:: *)
(*Dynamic Equations*)


v\[Nu]=Table[Table[\[Nu][m,n][t],{n,1,8}],{m,1,Sites}];
v\[Nu]cor=Table[Table[\[Nu][m,n][t]\[Nu][l,n][t],{n,1,8}],{m,1,Sites},{l,1,Sites}];
d\[Nu]=Table[Table[\[Nu][m,n]'[t],{n,1,8}],{m,1,Sites}];
initials\[Nu]=Flatten[Table[Table[\[Nu][m,n][0]==Subscript[\[Nu], 0][m,n],{n,1,8}],{m,1,Sites}]];
\[CapitalDelta]\[Nu][m_,ow_]:=Table[D[ow,\[Nu][m,n][t]],{n,1,8}];


vq=Table[v\[Nu][[m]].R,{m,1,Sites}];
dq=Table[d\[Nu][[m]].R,{m,1,Sites}];
\[CapitalDelta]q[m_,ow_]:=\[CapitalDelta]\[Nu][m,ow].R;


\[Nu]w[m_,ow_]:=v\[Nu][[m]] ow+I/2 Table[Sum[R[[n,a]]f[a,b,c]vq[[m,c]]\[CapitalDelta]q[m,ow][[b]],{b,1,8},{c,1,8},{a,1,8}],{n,1,8}];
\[Nu]wb[m_,ow_]:=v\[Nu][[m]] ow-I/2 Table[Sum[R[[n,a]]f[a,b,c]vq[[m,c]]\[CapitalDelta]q[m,ow][[b]],{b,1,8},{c,1,8},{a,1,8}],{n,1,8}];


sx\[Nu][m_,ow_]:= 2\[Nu]w[m,ow][[1]];
sy\[Nu][m_,ow_]:= 2\[Nu]w[m,ow][[2]];
sz\[Nu][m_,ow_]:= 2\[Nu]w[m,ow][[3]];
szs\[Nu][m_,ow_]:=2(-1/Sqrt[3])\[Nu]w[m,ow][[8]];
sx\[Nu]b[m_,ow_]:= 2\[Nu]wb[m,ow][[1]];
sy\[Nu]b[m_,ow_]:= 2\[Nu]wb[m,ow][[2]];
sz\[Nu]b[m_,ow_]:= 2\[Nu]wb[m,ow][[3]];
szs\[Nu]b[m_,ow_]:=2(-1/Sqrt[3])\[Nu]wb[m,ow][[8]];


(*H\[Nu][m_,ow_]:=U/2szs\[Nu][m,ow]-Jx  sx\[Nu][m,ow](Sum[sx\[Nu][n,1],{n,1,m-1}]+Sum[sx\[Nu][n,1],{n,m+1,Sites}])-Jy sy\[Nu][m,ow](Sum[sy\[Nu][n,1],{n,1,m-1}]+Sum[sy\[Nu][n,1],{n,m+1,Sites}])-\[Mu] sz\[Nu][m,ow];
H\[Nu]b[m_,ow_]:=U/2szs\[Nu]b[m,ow]-Jx sx\[Nu]b[m,ow](Sum[sx\[Nu]b[n,1],{n,1,m-1}]+Sum[sx\[Nu]b[n,1],{n,m+1,Sites}])-Jy sy\[Nu]b[m,ow](Sum[sy\[Nu]b[n,1],{n,1,m-1}]+Sum[sy\[Nu]b[n,1],{n,m+1,Sites}])-\[Mu] sz\[Nu]b[m,ow];*)


(*eqns\[Nu]1=Table[d\[Nu][[1,n]]\[Equal]Simplify[\[ImaginaryI] (H\[Nu][1,v\[Nu][[1,n]]]-H\[Nu]b[1,v\[Nu][[1,n]]])],{n,1,8}]*)


eqns\[Nu]1={Derivative[1][\[Nu][1,1]][t]==\[Mu] \[Nu][1,2][t]+1/2 U \[Nu][1,7][t]-2 Jy \[Nu][1,3][t] \[Nu][2,2][t],Derivative[1][\[Nu][1,2]][t]==-\[Mu] \[Nu][1,1][t]-1/2 U \[Nu][1,6][t]+2 Jx \[Nu][1,3][t] \[Nu][2,1][t],Derivative[1][\[Nu][1,3]][t]==-2 Jx \[Nu][1,2][t] \[Nu][2,1][t]+2 Jy \[Nu][1,1][t] \[Nu][2,2][t],Derivative[1][\[Nu][1,4]][t]==2 (\[Mu] \[Nu][1,5][t]+Jx \[Nu][1,7][t] \[Nu][2,1][t]+Jy \[Nu][1,6][t] \[Nu][2,2][t]),Derivative[1][\[Nu][1,5]][t]==-2 (\[Mu] \[Nu][1,4][t]+Jx \[Nu][1,6][t] \[Nu][2,1][t]-Jy \[Nu][1,7][t] \[Nu][2,2][t]),Derivative[1][\[Nu][1,6]][t]==1/2 U \[Nu][1,2][t]+\[Mu] \[Nu][1,7][t]+2 Jx \[Nu][1,5][t] \[Nu][2,1][t]-2 Jy \[Nu][1,4][t] \[Nu][2,2][t]-2 Sqrt[3] Jy \[Nu][1,8][t] \[Nu][2,2][t],Derivative[1][\[Nu][1,7]][t]==-(1/2) U \[Nu][1,1][t]-\[Mu] \[Nu][1,6][t]-2 Jx \[Nu][1,4][t] \[Nu][2,1][t]+2 Sqrt[3] Jx \[Nu][1,8][t] \[Nu][2,1][t]-2 Jy \[Nu][1,5][t] \[Nu][2,2][t],Derivative[1][\[Nu][1,8]][t]==2 Sqrt[3] (-Jx \[Nu][1,7][t] \[Nu][2,1][t]+Jy \[Nu][1,6][t] \[Nu][2,2][t])};


eqns\[Nu]=Flatten[Table[eqns\[Nu]1/.Flatten[Table[{\[Nu][1,n]'[t]->\[Nu][m,n]'[t],\[Nu][1,n][t]->\[Nu][m,n][t],\[Nu][2,n][t]->(Sum[\[Nu][l,n][t],{l,1,m-1}]+Sum[\[Nu][l,n][t],{l,m+1,Sites}])},{n,1,8}]],{m,1,Sites}]];


(*MatrixForm[eqns\[Nu]]*)


(* ::Section:: *)
(*SU3 Wigner with neg prob*)


\[Nu]varsingreek={((\[Alpha]+\[Gamma]) Conjugate[\[Beta]]+\[Beta] (Conjugate[\[Alpha]]+Conjugate[\[Gamma]]))/(2 Sqrt[2]),-((I (\[Beta] Conjugate[\[Alpha]]+(-\[Alpha]+\[Gamma]) Conjugate[\[Beta]]-\[Beta] Conjugate[\[Gamma]]))/(2 Sqrt[2])),1/2 (Abs[\[Alpha]]^2-\[Gamma] Conjugate[\[Gamma]]),1/2 (\[Gamma] Conjugate[\[Alpha]]+\[Alpha] Conjugate[\[Gamma]]),-(1/2) I (\[Gamma] Conjugate[\[Alpha]]-\[Alpha] Conjugate[\[Gamma]]),(-\[Beta] Conjugate[\[Alpha]]+(-\[Alpha]+\[Gamma]) Conjugate[\[Beta]]+\[Beta] Conjugate[\[Gamma]])/(2 Sqrt[2]),(I (-(\[Alpha]+\[Gamma]) Conjugate[\[Beta]]+\[Beta] (Conjugate[\[Alpha]]+Conjugate[\[Gamma]])))/(2 Sqrt[2]),-((Abs[\[Alpha]]^2+Abs[\[Gamma]]^2-2 \[Beta] Conjugate[\[Beta]])/(2 Sqrt[3]))};


Norm1=(4-Sqrt[E])/Sqrt[E];


Fock1Mag=ProbabilityDistribution[4 a E^(-2 a^2) Abs[(4a^2-1)]/Norm1,{a,0,\[Infinity]}];


randmag=RandomVariate[Fock1Mag,{Sites,su3Runs}];


metric=1-2UnitStep[1/2-randmag];


randphase=RandomReal[{0,2\[Pi]},{Sites,su3Runs}];


randrest=Table[RandomVariate[NormalDistribution[0,1/2]]+I RandomVariate[NormalDistribution[0,1/2]],{2},{Sites},{su3Runs}];


randupinit[n_]:={randmag[[n]] E^(I randphase[[n]]),randrest[[1,n]],randrest[[2,n]]};
randmiinit[n_]:={randrest[[1,n]],randmag[[n]] E^(I randphase[[n]]),randrest[[2,n]]};
randdoinit[n_]:={randrest[[1,n]],randrest[[2,n]],randmag[[n]] E^(I randphase[[n]])};


randinits=Table[randupinit[n],{n,1,updownmiddle[[1]]}]~Join~Table[randdoinit[n],{n,updownmiddle[[1]]+1,updownmiddle[[1]]+updownmiddle[[2]]}]~Join~Table[randmiinit[n],{n,updownmiddle[[1]]+updownmiddle[[2]]+1,updownmiddle[[1]]+updownmiddle[[2]]+updownmiddle[[3]]}];


spindyn=Table[0,{Nt},{1},{2}];
SetSharedVariable[spindyn];
spincordyn=Table[0,{Nt},{1},{4}];
SetSharedVariable[spincordyn];
su3Runsdone=0;
SetSharedVariable[su3Runsdone];
Timing[
ParallelDo[
sol=NDSolve[(eqns\[Nu]~Join~initials\[Nu])/.{U->Uvalue,\[Mu]->\[Mu]value,J->Jvalue,navg->navgvalue}~Join~Flatten[Table[Subscript[\[Nu], 0][m,n]->(\[Nu]varsingreek[[n]]/.{\[Alpha]->randinits[[m,1,j]],\[Beta]->randinits[[m,2,j]],\[Gamma]->randinits[[m,3,j]]}),{m,1,Sites},{n,1,8}]],Flatten[v\[Nu]],{t,0,tmax},MaxSteps->\[Infinity]
];
metprod=Product[metric[[n,j]],{n,1,Sites}];
TWAresspin=metprod {\[Nu][1,3][t],\[Nu][2,3][t]}/.sol;
TWArescor=metprod{\[Nu][1,8][t],\[Nu][2,8][t],\[Nu][1,8][t]\[Nu][1,8][t],\[Nu][2,8][t]\[Nu][2,8][t]}/.sol;
spindyn+=Table[TWAresspin,{t,ts}];
spincordyn+=Table[TWArescor,{t,ts}];
su3Runsdone++;
,{j,1,su3Runs}
]
]
avgspins3=Norm1^Sites Re[spindyn[[All,1]]]/su3Runs;
avgcor3=Norm1^Sites Re[spincordyn[[All,1]]]/su3Runs;


Save[su3outfile<>"spn.lst",avgspins3]
Save[su3outfile<>"corn.lst",avgcor3]
