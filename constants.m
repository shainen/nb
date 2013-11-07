(* ::Package:: *)

Sites=3;
tmax=20;
Nt=128;
dt=tmax/Nt;
Length[ts=N@Range[0,tmax-dt,dt]];
times=Table[t,{t,0,tmax-dt,dt}];
Runs=100;
mSIM=10;
Uvalue=1;
\[Mu]value=1;
Jvalue=0.01;
navgvalue=1;
putvalues:={U->Uvalue,J->Jvalue,navg->navgvalue,\[Mu]->\[Mu]value};
