(* ::Package:: *)

Sites=6;
updownmiddle={1,1,4};

tmax=200;
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

su2outfile="data/su2awS"<>ToString[Sites]<>"t"<>ToString[tmax]<>"j"<>ToString[Jvalue]<>"r"<>ToString[su2Runs]<>".lst";
su3outfile="data/su3awS"<>ToString[Sites]<>"t"<>ToString[tmax]<>"j"<>ToString[Jvalue]<>"r"<>ToString[su3Runs]<>".lst";
