clc
clear
%close all
runnumber=100

dir='E:\Newest Window Detection\Experiment s moving window\100D_exp1__Window\';

wsz=10;
strWsz=int2str(wsz);
txtName=['w',strWsz,'_',strWsz,'_',strWsz];
noShiftNum=100;
%votesum00 means for S_0  data

type='s1logsum';
%type='votesum';
outputnoshift100500=load([dir,txtName,' ',type,'00.txt']);  %s1votesum votegt05
outputshift1_0_100500=load([dir,txtName,' ',type,'10.txt']);  %p110
outputshift1_1_100500=load([dir,txtName,' ',type,'11.txt']); 


outputnoshift100500=outputnoshift100500';
outputshift1_0_100500=outputshift1_0_100500';
outputshift1_1_100500=outputshift1_1_100500';

outputnoshift100500=outputnoshift100500(:,noShiftNum+1-wsz+1:end);
outputshift1_0_100500=outputshift1_0_100500(:,noShiftNum+1-wsz+1:end);
outputshift1_1_100500=outputshift1_1_100500(:,noShiftNum+1-wsz+1:end);

%here want to check if any logsum is equal to 1 or 0
r1=[sum(sum((outputnoshift100500)<-100)) sum(sum((outputshift1_0_100500)<-100)) sum(sum((outputshift1_1_100500)<-100))];
r2=[sum(sum((outputnoshift100500)>100)) sum(sum((outputshift1_0_100500)>100)) sum(sum((outputshift1_1_100500)>100))];
r3=[min(min((outputnoshift100500))) min(min((outputshift1_0_100500))) min(min((outputshift1_1_100500)))];
r4=[max(max((outputnoshift100500))) max(max((outputshift1_0_100500))) max(max((outputshift1_1_100500)))];
[r1;r2;r3;r4]
debug=1; % 

x=outputnoshift100500;
xx=1-x;
xxx=log(x./xx);

x1=outputshift1_0_100500;
xx1=1-x1;
xxx1=log(x1./xx1);

x2= outputshift1_1_100500;
xx2=1-x2;
xxx2=log(x2./xx2);
    xxx=x;
    xxx1=x1;
    xxx2=x2;

%-------get control limit

slidetime=2800;

%wsz=13;
%arlgoal=200-wsz+1;
arlgoal=200
limit=-3;limit = min(xxx(:));
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


figure,plot(arl,'rx')

 arlmean
 limit
    arlmeanx0=mean(arl);
    arlstdx0 =std(arl);
%------------------------



slidetime=400;


%wsz=13
%% 
%% %x4=x4(1:20,:)
testx1=xxx1
testx2=xxx2
arlmeanx1=0
arlmeanx2=0
arlstdx1=0
arlstdx2=0

   arl1=[]
  for i=1:runnumber
    b=find(testx1(i,:)>limit);
       if isempty(b)==1;
         temparl=slidetime;
       else;
         temparl=b(1);
       end;
     arl1=[arl1 temparl];
     
  end
    arlmeanx1=mean(arl1);
    arlstdx1 =std(arl1);
%     
       arl2=[];
   for i=1:runnumber;
    b=find(testx2(i,:)>limit);

       if isempty(b)==1;
         temparl=slidetime;
       else;
         temparl=b(1);
       end
     arl2=[arl2 temparl];
  end
    arlmeanx2=mean(arl2);
    arlstdx2 =std(arl2);
    
%       arl3=[];
%   for i=1:runnumber;
%     b=find(testx3(i,:)>limit);
% 
%        if isempty(b)==1;
%          temparl=slidetime;
%        else
%          temparl=b(1)-1;
%        end
%      arl3=[arl3 temparl];
%   end
%     arlmeanx3=mean(arl3);
%     arlstdx3 =std(arl3);
%     
%          arl4=[];
%   for i=1:runnumber;
%     b=find(testx4(i,:)>limit);
% 
%        if isempty(b)==1;
%          temparl=slidetime;
%        else;
%          temparl=b(1)-1;
%        end
%      arl4=[arl4 temparl];
%   end
%     arlmeanx4=mean(arl4);
%     arlstdx4 =std(arl4);
    
% arlmeanx1
% arlmeanx2
% arlmeanx3
% % arlmeanx4
 arlmeanvector=[arlmeanx0;arlmeanx1;arlmeanx2];
% arlstdx1
% arlstdx4
 arlstdvector=[arlstdx0;arlstdx1;arlstdx2];
  arlstdvector=arlstdvector/(sqrt(runnumber));


  limit=limit';
  limit
[ arlmeanvector'; arlstdvector']
 
% hold off
% plot(arl1,'rx')

 hold on
 plot(arl1,'r.')
plot(arl2,'b*')
%plot(arl3,'rs')
%plot(arl4,'k*')
% sum(arl>95)
% sum(arl1>95)
% sum(arl2>95)
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