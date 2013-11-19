%function bhsuQM

clear

tmax=20;

Nt = 128;
dt = tmax/Nt;

t=[0:dt:tmax-dt];

S = 3;
U = 1;
mu = 1;
J1 = 1;
J2 = 0;


Jx=[0,1,0;1,0,1;0,1,0]/sqrt(2);
Jy=i*[0,-1,0;1,0,-1;0,1,0]/sqrt(2);
Jz=[1,0,0;0,0,0;0,0,-1];


JxS=cell(1,S);
JyS=cell(1,S);
JzS=cell(1,S);
for j=1:S,
    JxS{j}=sparse(kron(kron(eye(3^(j-1)),Jx),eye(3^(S-j))));
    JyS{j}=sparse(kron(kron(eye(3^(j-1)),Jy),eye(3^(S-j))));
    JzS{j}=sparse(kron(kron(eye(3^(j-1)),Jz),eye(3^(S-j))));
end

'make ham'
time=cputime;

HQM=sparse(3^S,3^S);
for j=1:S
    HQM=HQM+(JzS{j}*JzS{j})/2-JzS{j};
    for k=j+1:S
        HQM=HQM-J1*(JxS{j}*JxS{k})-J2*(JyS{j}*JyS{k});
    end
end
cputime-time

HQMf=full(HQM);

HQMf

'diag'
time=cputime;
[Vn,Em]=eig(HQMf);
cputime-time

En=diag(Em);

init=zeros(3^S,1);
init(1,1)=1;

cn=Vn'*init;
m1=Vn'*(JzS{1}*Vn);
m2=Vn'*(JzS{2}*Vn);
%m3=Vn'*(JzS{3}*Vn);
% m1=Vn'*(JxS{1}*Vn);
% m2=Vn'*(JxS{2}*Vn);
% m3=Vn'*(JxS{3}*Vn);

%m7=Vn'*(JzS{4}*Vn);
%m8=Vn'*(JzS{5}*Vn);
%m9=Vn'*(JzS{6}*Vn);

% m4=Vn'*(JzS{1}*(JzS{1}*Vn));
% m5=Vn'*(JzS{2}*(JzS{2}*Vn));
% m6=Vn'*(JzS{3}*(JzS{3}*Vn));
m4=Vn'*(JxS{1}*(JxS{1}*Vn));
m5=Vn'*(JxS{2}*(JxS{2}*Vn));
%m6=Vn'*(JxS{3}*(JxS{3}*Vn));
m12=Vn'*((JzS{1}+JzS{2}+JzS{3})*Vn);

'plot'
time=cputime
for j=1:Nt
    vec=exp(-i*t(j)*En).*cn;
    AvgSz1(j)=vec'*(m1*vec);
    AvgSz2(j)=vec'*(m2*vec);
%    AvgSz3(j)=vec'*(m3*vec);
    AvgAll(j)=real(vec'*(m12*vec));
%    AvgSz4(j)=vec'*(m7*vec);
%    AvgSz5(j)=vec'*(m8*vec);
%    AvgSz6(j)=vec'*(m9*vec);
%     SqSz1(j)=vec'*(m4*vec);
%     SqSz2(j)=vec'*(m5*vec);
%     SqSz3(j)=vec'*(m6*vec);
end
cputime-time

% widSz1=SqSz1-AvgSz1.*AvgSz1;
% widSz2=SqSz2-AvgSz2.*AvgSz2;
% widSz3=SqSz3-AvgSz3.*AvgSz3;

%save('QMAvgSz3','AvgSz1','AvgSz2','AvgSz3','-ascii');
%save('QMWidSz3','widSz1','widSz2','widSz3','-ascii');
%save('QMAvgSz2','AvgSz1','AvgSz2','-ascii');
%save('QMWidSz2','widSz1','widSz2','-ascii');



%plot the results
 figure
 hold on

 plot(t,AvgAll,'b');
% plot(t,AvgSz2,'r');
 %plot(t,AvgSz3,'y');

 % figure
 %hold on

% plot(t,AvgSz4,'b');
% plot(t,AvgSz5,'r');
% plot(t,AvgSz6,'y');
% figure
%  hold on
% % %plot(t,X(:,1),'r');
%  plot(t,widSz1,'b');
%  plot(t,widSz2,'r');
%  plot(t,widSz3,'y');
%legend('n for p=0');