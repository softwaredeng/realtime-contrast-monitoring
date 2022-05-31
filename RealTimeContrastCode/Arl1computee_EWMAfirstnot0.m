clc
clear
close all
runnumber=100
ntree=500
%%----------get alr0=200 limit mean
dir='E:\Newest Window Detection\Experiment s moving window\100D_exp1__Window\';

wsz=10;
strWsz=int2str(wsz);
txtName=['w',strWsz,'_',strWsz,'_',strWsz];
noShiftNum=100;

type='p1';
outputnoshift100500=load([dir,txtName,' ',type,'00.txt']);  %s1votesum votegt05 s1votesum s1logsum
outputshift1_0_100500=load([dir,txtName,' ',type,'10.txt']);  %p110
outputshift1_1_100500=load([dir,txtName,' ',type,'11.txt']); 

outputnoshift100500=outputnoshift100500';
outputshift1_0_100500=outputshift1_0_100500';
outputshift1_1_100500=outputshift1_1_100500';

outputnoshift100500=outputnoshift100500(:,noShiftNum+1-wsz+1:end);
outputshift1_0_100500=outputshift1_0_100500(:,noShiftNum+1-wsz+1:end);
outputshift1_1_100500=outputshift1_1_100500(:,noShiftNum+1-wsz+1:end);


slidetime=1300;
matrixone=ones(runnumber,slidetime);
%predict the average run of the no shift situation-------
% has been run and got the result of : limit=3.17 so temporaryly comment
x=outputnoshift100500;
 
apx0 = 1/(ntree+1); 
apx1 = ntree/(ntree+1);

indx=find(x==1);
x(indx)=apx1;

indx2=find(x==0);
x(indx2)=apx0;

 xx=1-x;
 
xxx=log(x./xx);

%

meanincontrol=mean(mean(xxx));
meanincontrol=(mean(xxx'))';
  vector=0.8*meanincontrol+0.2*xxx(:,1);
 for nn=2:slidetime;
      temp=0.8*vector(:,nn-1)+0.2*xxx(:,nn);
      vector=[vector temp];
 end;
 
 xxx=vector;

arlgoal=200;
limit=min(xxx(:));
arlmean=0;
while(arlmean<arlgoal);
    limit=limit+0.001;    
     
  arl=[];
  for i=1:runnumber;
    b=find(xxx(i,:)>limit);

       if isempty(b)==1;
         temparl=slidetime;
       else;
         temparl=b(1);
       end;
     arl=[arl temparl];
  end
    arlmean=mean(arl);
end
arlmean=mean(arl);
arlstd =std(arl);
%----------get alr1




slidetime=300;
matrixone=ones(runnumber,slidetime);

limit= limit;
 meanincontrol=meanincontrol;
 
 x1=outputshift1_0_100500;
 indx=find(x1==1);
x1(indx)=apx1;
indx2=find(x1==0);
x1(indx2)=apx0;

  x2= outputshift1_1_100500;
  indx=find(x2==1);
x2(indx)=apx1;
indx2=find(x2==0);
x2(indx2)=apx0;


% 
% %x1=x1(1:20,:)
 xx1=1-x1;
 xxx1=log(x1./xx1);


% 
% %x2=x2(1:20,:)
 xx2=1-x2;
xxx2=log(x2./xx2);



  vector=0.8*meanincontrol+0.2*xxx1(:,1);
 for nn=2:slidetime;
      temp=0.8*vector(:,nn-1)+0.2*xxx1(:,nn);
      vector=[vector temp];
 end;
xxx1=vector;

  vector=0.8*meanincontrol+0.2*xxx2(:,1);
 for nn=2:slidetime;
      temp=0.8*vector(:,nn-1)+0.2*xxx2(:,nn);
      vector=[vector temp];
 end;
xxx2=vector;



testx1=xxx1;
testx2=xxx2;

arlmeanx1=0;
arlmeanx2=0;

arlstdx1=0
arlstdx2=0


   arl1=[]
  for i=1:runnumber;
      b=[]
    b=find(testx1(i,:)>limit);
       if isempty(b)==1;
         temparl=slidetime;
       else
         temparl=b(1)
       end
     arl1=[arl1 temparl];
     
  end
    arlmeanx1=mean(arl1);
    arlstdx1 =std(arl1);
%     
       arl2=[]
   for i=1:runnumber;
    b=find(testx2(i,:)>limit);

       if isempty(b)==1;
         temparl=slidetime;
       else
         temparl=b(1);
       end
     arl2=[arl2 temparl]
  end
    arlmeanx2=mean(arl2);
    arlstdx2 =std(arl2);
    
 

 arlmeanvector=[arlmean arlmeanx1 arlmeanx2]
% arlstdx1
% arlstdx4
 arlstdvector=[arlstd arlstdx1 arlstdx2];
 arlstdvector=arlstdvector/(sqrt(runnumber));
 limit
[ arlmeanvector ; arlstdvector]

% hold off
% plot(arl1,'rx')

 hold on
 plot(arl1,'r.')
plot(arl2,'b*')

 
% % -------------------------The following is the reference and not used in
% this program
% % % fid = fopen('C:\Program Files\R\R-2.6.1\vewma.txt','wt');
% % % for k=1:size(vewma,1)
% % % for j=1:size(vewma,2)
% % % fprintf(fid,'%0.7f',vewma(k,j))
% % % fprintf(fid,'%c',' ');
% % % end
% % % fprintf(fid,'%c\n','');
% % % end
% % % fclose(fid)
% % 
% % % st = controlchart(vewma,'chart',{'ewma' 'r'});
% % 
% % % a=importdata('C:\Program Files\R\R-2.6.1\zz.txt');
% % % a
% % %EWMA