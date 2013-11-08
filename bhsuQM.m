function bhsuQM

tmax=200;

Nt = 200;
dt = tmax/Nt;

t=[0:dt:tmax-dt];

S = 3;
U = 1;
mu = 1;
J = 0.01;

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
    HQM=HQM+JzS{j}*JzS{j}-JzS{j};
    for k=j+1:S
        HQM=HQM-J*(JxS{j}*JxS{k}+JyS{j}*JyS{k});
    end
end
cputime-time

HQMf=full(HQM);

'diag'
time=cputime;
[Vn,Em]=eig(HQMf);
cputime-time

En=diag(Em);

init=zeros(3^S,1);
init(8,1)=1;

cn=Vn'*init;
m1=Vn'*(JzS{1}*Vn);
m2=Vn'*(JzS{2}*Vn);
m3=Vn'*(JzS{3}*Vn);

AvgSz1=zeros(Nt,1);
AvgSz2=zeros(Nt,1);
AvgSz3=zeros(Nt,1);

'plot'
time=cputime
for j=1:Nt
    vec=exp(-i*t(j)*En).*cn;
    AvgSz1(j)=vec'*(m1*vec);
    AvgSz2(j)=vec'*(m2*vec);
    AvgSz3(j)=vec'*(m3*vec);
end
cputime-time

%plot the results
figure
hold on
%plot(t,X(:,1),'r');
plot(t,AvgSz1,'b');
plot(t,AvgSz2,'r');
plot(t,AvgSz3,'y');
%legend('n for p=0');