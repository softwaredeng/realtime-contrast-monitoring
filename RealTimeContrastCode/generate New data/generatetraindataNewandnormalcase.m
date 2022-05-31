clc
clear
kk=100;% time for the replicate

temp1 =what;
dir0=temp1.path;
dir1=[dir0 '\' 'dataset'];

%-----------Generate training data-------
D=100
a0=zeros(1,D);b=eye(D,D);
a1=a0;a1(1)=2;
a2=a0;a2(1:D/10)=1;

noshiftNum=100;shiftNum0=2000;shiftNum1=500;
train2=[];shift00=[];shift10=[];shift11=[];
for i=1:kk;
train2=mvnrnd(a0,b,2000);
outputDir= [dir1 '\' 'rep' num2str(i) '_traindata.txt'];writeToFile(outputDir,train2);
%%%%%%%%%%%%%%%%%%
tempshift00=[mvnrnd(a0,b,noshiftNum);mvnrnd(a0,b,shiftNum0)];
outputDir= [dir1 '\' 'rep' num2str(i) '_shift0.txt'];writeToFile(outputDir,tempshift00);
%%%%%%%%%%%%
tempshift10=[mvnrnd(a0,b,noshiftNum);mvnrnd(a1,b,shiftNum1)];
outputDir= [dir1 '\' 'rep' num2str(i) '_shift1.txt'];writeToFile(outputDir,tempshift10);
%%%%%%%%%%%%%%%%
tempshift11=[mvnrnd(a0,b,noshiftNum);mvnrnd(a2,b,shiftNum1)];
outputDir= [dir1 '\' 'rep' num2str(i) '_shift2.txt'];writeToFile(outputDir,tempshift11);
%%%%%%%%%%%%%%%%
end;


