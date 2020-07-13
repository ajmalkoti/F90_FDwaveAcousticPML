A=ones(10,5);
A(1:3,:)=2000;
A(3:6,:)=2200;
A(6:10,:)=2400;
dlmwrite('vp.txt',A,'delimiter','\t')
dlmwrite('vs.txt',0.92*A,'delimiter','\t')
dlmwrite('rho.txt',1800*ones(10,5),'delimiter','\t')