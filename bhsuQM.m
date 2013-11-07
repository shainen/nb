function bhsuQM

tmax=20;

Nt = 200;
dt = tmax/Nt;

t=[0:dt:tmax-dt];

S = 6;
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

HQM=zeros(3^S,3^S);
for j=1:S
    HQM=HQM+JzS{j}*JzS{j}-JzS{j};
    for k=j+1:S
        HQM=HQM-J*(JxS{j}*JxS{k}+JyS{j}*JyS{k});
    end
end

[Vn,En]=eig(HQM);

init=zeros(3^S,1);
init(1,1)=1;

cn=Vn'*init;  

AvgSz1=zeros(Nt);
for j=1:Nt
    AvgSz1(j)=cn'*expm(i*t(j)*En)*Vn'*JzS{1}*Vn*expm(-i*t(j)*En)*cn;
end

%plot the results

figure
hold on
%plot(t,X(:,1),'r');
plot(t,AvgSz1);
%legend('n for p=0');