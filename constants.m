(* ::Package:: *)

Sites=10;
updownmiddle={10,0,0};

tmax=10;
Nt=128;
dt=tmax/Nt;
Length[ts=N@Range[0,tmax-dt,dt]];
times=Table[t,{t,0,tmax-dt,dt}];

su2Runs=100000;
su3Runs=100000;

Uvalue=1;
\[Mu]value=1;
Jvalue=0.01;
navgvalue=1;
putvalues:={U->Uvalue,J->Jvalue,navg->navgvalue,\[Mu]->\[Mu]value};

Jx=0.1;
Jy=0.2;

su2outfile="data/su2awS"<>ToString[Sites]<>"t"<>ToString[tmax]<>"j"<>ToString[Jvalue]<>"r"<>ToString[su2Runs];
su3outfile="data/su3awS"<>ToString[Sites]<>"t"<>ToString[tmax]<>"j"<>ToString[Jvalue]<>"r"<>ToString[su3Runs];
