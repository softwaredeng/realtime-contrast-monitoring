#---------------------------
rm(list=ls(all=TRUE))
library(randomForest)
#R code for the paper: for the random forest
ptm <- proc.time() #whole time used


require(doSMP)
workers <- startWorkers(2) # My computer has 2 cores
registerDoSMP(workers)




trainNum=2000;
noShiftNum=100;n1=noShiftNum;
shiftNum=2000;n2=shiftNum;
totalNum=noShiftNum+shiftNum;n3=totalNum
jtime=10#


temptrainfactor=matrix(c("n"),trainNum:1)
tempnormalfactor=matrix(c("a"),100:1)
tempabnormalfactor=matrix(c("a"),n2:1)

oobvectornormal=NULL;oobvectorabnormal=NULL;votevector=NULL;voteoobvector=NULL;oobvector=NULL;
votegt01=NULL;votegt02=NULL;votegt03=NULL;votegt04=NULL;votegt05=NULL;votegt06=NULL;votegt07=NULL
votegt08=NULL;votegt09=NULL;votesum=NULL;s1votesum=NULL;s1votegt01=NULL;s1votegt02=NULL;s1votegt03=NULL
s1votegt04=NULL;s1votegt05=NULL;s1votegt06=NULL;s1votegt07=NULL;s1votegt08=NULL;s1votegt09=NULL
p1=NULL;logsum=NULL # sum{log(p_1/1-p_1)}
s1logsum=NULL


#system.time(x <- foreach(j=1:times ) %dopar% check(j))  #  2.56 seconds  (notice that the first run would be slower, because of R's lazy loading)
#system.time(for(j in 1:times ) x <- check(j))  #  4.82 seconds


#foreach (j =1:jtime) %dopar% {#-----------------------------------------{3}
for (j in 1:jtime) {#-----------------------------------------{3}
print(j)
timeused=proc.time() - ptm
print(timeused)

bwsz=10
a=paste("w",bwsz,"_",bwsz,"_",bwsz,sep="");
start= (noShiftNum+1) -bwsz + 1
type=temptrainfactor
x1=trainNum

pathData = paste(getwd(), "/", "generate New data 2","/dataset/rep",j,"_traindata.txt",sep="");
traindatax<- read.table(pathData)
pathData = paste(getwd(), "/", "generate New data 2","/dataset/rep",j,"_shift0.txt",sep="");
testnormalandabnormaldata<- read.table(pathData)

traindataxy=cbind(traindatax , type) # The incontrol data

ktime=1500#the time of the window slide***************************************************
knumber=ktime-1

foreach (k = 0:knumber) %dopar% {#-----------------------------------------{3}
#for (k in 0:knumber){#------------------------------------{2}
temp2=testnormalandabnormaldata[(1+k):(bwsz+k),]
tempa=temp2
d2=NULL
for (dd in 0:(bwsz-1))
{
d2=rbind(d2,temp2[bwsz-dd,])
}

number=bwsz
#tempa=rbind(tempa,d2)
type=matrix(c("a"),(number):1)
final=cbind(d2,type)
newtraindata=rbind(traindataxy,final)
#my.rf <- randomForest(type ~., replace=FALSE,data=newtraindata,sampsize=c(n=50,a=12)) 
my.rf <- randomForest(type ~.,sampsize=c(n=bwsz,a=bwsz), data=newtraindata) 
#classwt=c(0.5,0.5),
#print(my.rf)
ooblist=my.rf[5]
oobcolumn=matrix(c(ooblist[[1]]),2:3)
ooberrornormal= (oobcolumn[,3])[1]
ooberrorabnormal=(oobcolumn[,3])[2]
temp=my.rf[6]

votenumber=x1+number
temp2=matrix( c(temp[[1]]),votenumber,2)
p1vote=mean(temp2[(x1+1),2])# since the recent data we are putting the first postion

gt09=sum(temp2[(1:x1),1]>0.9)/x1
gt08=sum(temp2[(1:x1),1]>0.8)/x1
gt07=sum(temp2[(1:x1),1]>0.7)/x1
gt06=sum(temp2[(1:x1),1]>0.6)/x1
gt05=sum(temp2[(1:x1),1]>0.5)/x1
gt04=sum(temp2[(1:x1),1]>0.4)/x1
gt03=sum(temp2[(1:x1),1]>0.3)/x1
gt02=sum(temp2[(1:x1),1]>0.2)/x1
gt01=sum(temp2[(1:x1),1]>0.1)/x1


#previous one does not consider the special case when 0 is involved
#logsum0=sum( log(   temp2[(1:x1),1]/ (1- temp2[(1:x1),1] )  )     )
#s1logsum0=sum( log(   temp2[((x1+1):votenumber),2]/ (1- temp2[((x1+1):votenumber),2] )  )     )
#---
apx1=500/(500+1); apx0 = 1/(500+1);
temp21 = temp2[(1:x1),1]
temp21[which(temp21 == 1)]=apx1
temp21[which(temp21 == 0)]=apx0
logsum0=sum( log(  temp21/ (1- temp21 )  )     )

temp21 = temp2[((x1+1):votenumber),2]
temp21[which(temp21 == 1)]=apx1
temp21[which(temp21 == 0)]=apx0
s1logsum0=sum( log(   temp21/ (1- temp21 )  )     )
#---------------


gtsum=sum(temp2[(1:x1),1])/x1

s1gtsum=sum(temp2[((x1+1):votenumber),2])/number


s1gt09=sum(temp2[((x1+1):votenumber),2]>0.9)/number
s1gt08=sum(temp2[((x1+1):votenumber),2]>0.8)/number
s1gt07=sum(temp2[((x1+1):votenumber),2]>0.7)/number
s1gt06=sum(temp2[((x1+1):votenumber),2]>0.6)/number
s1gt05=sum(temp2[((x1+1):votenumber),2]>0.5)/number
s1gt04=sum(temp2[((x1+1):votenumber),2]>0.4)/number
s1gt03=sum(temp2[((x1+1):votenumber),2]>0.3)/number
s1gt02=sum(temp2[((x1+1):votenumber),2]>0.2)/number
s1gt01=sum(temp2[((x1+1):votenumber),2]>0.1)/number


oobvectornormal=rbind(oobvectornormal,ooberrornormal)
oobvectorabnormal=rbind(oobvectorabnormal,ooberrorabnormal)
voteoobvector=rbind(voteoobvector,p1vote)
votegt01=rbind(votegt01,gt01)
votegt02=rbind(votegt02,gt02)
votegt03=rbind(votegt03,gt03)
votegt04=rbind(votegt04,gt04)
votegt05=rbind(votegt05,gt05)
votegt06=rbind(votegt06,gt06)
votegt07=rbind(votegt07,gt07)
votegt08=rbind(votegt08,gt08)
votegt09=rbind(votegt09,gt09)

votesum=rbind(votesum,gtsum)
s1votesum=rbind(s1votesum,s1gtsum)

logsum=rbind(logsum,logsum0)
s1logsum=rbind(s1logsum,s1logsum0)

s1votegt01=rbind(s1votegt01,s1gt01)
s1votegt02=rbind(s1votegt02,s1gt02)
s1votegt03=rbind(s1votegt03,s1gt03)
s1votegt04=rbind(s1votegt04,s1gt04)
s1votegt05=rbind(s1votegt05,s1gt05)
s1votegt06=rbind(s1votegt06,s1gt06)
s1votegt07=rbind(s1votegt07,s1gt07)
s1votegt08=rbind(s1votegt08,s1gt08)
s1votegt09=rbind(s1votegt09,s1gt09)

p1=rbind(p1,p1vote)

}#----------------------------------{2}
}#----------------------------------{3}



# stop workers
stopWorkers(workers)

#--save image
timeStamp=gsub(" ", "_", Sys.time())   #(or can use Sys.Date())
timeStamp=gsub(":", "-", timeStamp)
tempName=paste(dataName, collapse = "");tempName=gsub(".txt","", tempName)

timeStamp=paste(timeStamp,tempName,a,".RData",sep="");
save.image(timeStamp)



oobnormalmatrix=matrix(c(oobvectornormal),ktime:jtime)
oobnormalmatrix=t(oobnormalmatrix)
oobabnormalmatrix=matrix(c(oobvectorabnormal),ktime:jtime)
oobabnormalmatrix=t(oobabnormalmatrix)
voteoobmatrix=matrix(c(voteoobvector),ktime:jtime)
voteoobmatrix=t(voteoobmatrix)


logsum=t(logsum)
logsum=matrix(c(logsum),ktime:jtime)

s1logsum=t(s1logsum)
s1logsum=matrix(c(s1logsum),ktime:jtime)

votegt01=t(votegt01)
votegt01=matrix(c(votegt01),ktime:jtime)
votegt02=t(votegt02)
votegt02=matrix(c(votegt02),ktime:jtime)
votegt03=t(votegt03)
votegt03=matrix(c(votegt03),ktime:jtime)
votegt04=t(votegt04)
votegt04=matrix(c(votegt04),ktime:jtime)
votegt05=t(votegt05)
votegt05=matrix(c(votegt05),ktime:jtime)
votegt06=t(votegt06)
votegt06=matrix(c(votegt06),ktime:jtime)
votegt07=t(votegt07)
votegt07=matrix(c(votegt07),ktime:jtime)
votegt08=t(votegt08)
votegt08=matrix(c(votegt08),ktime:jtime)
votegt09=t(votegt09)
votegt09=matrix(c(votegt09),ktime:jtime)

votesum=t(votesum)
votesum=matrix(c(votesum),ktime:jtime)

s1votesum=t(s1votesum)
s1votesum=matrix(c(s1votesum),ktime:jtime)

s1votegt01=t(s1votegt01)
s1votegt01=matrix(c(s1votegt01),ktime:jtime)
s1votegt02=t(s1votegt02)
s1votegt02=matrix(c(s1votegt02),ktime:jtime)
s1votegt03=t(s1votegt03)
s1votegt03=matrix(c(s1votegt03),ktime:jtime)
s1votegt04=t(s1votegt04)
s1votegt04=matrix(c(s1votegt04),ktime:jtime)
s1votegt05=t(s1votegt05)
s1votegt05=matrix(c(s1votegt05),ktime:jtime)
s1votegt06=t(s1votegt06)
s1votegt06=matrix(c(s1votegt06),ktime:jtime)
s1votegt07=t(s1votegt07)
s1votegt07=matrix(c(s1votegt07),ktime:jtime)
s1votegt08=t(s1votegt08)
s1votegt08=matrix(c(s1votegt08),ktime:jtime)
s1votegt09=t(s1votegt09)
s1votegt09=matrix(c(s1votegt09),ktime:jtime)

p1=t(p1)
p1=matrix(c(p1),ktime:jtime)



#write.table(voteoobmatrix, file = "outputshift0-0-100500replicatep1voteoob.txt",quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(oobnormalmatrix, file = "oobnormal00.txt",quote= FALSE,row.names=FALSE,col.names=FALSE)
#write.table(oobabnormalmatrix, file = "outputshift0-0-100500oob12wsz20replicateoobabnormal.txt",quote= FALSE,row.names=FALSE,col.names=FALSE)
timeused=proc.time() - ptm
print(timeused)
#------------------


write.table(votesum, file = paste(a,"votesum00.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votesum, file = paste(a,"s1votesum00.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt01, file = paste(a,"votegt0100.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt02, file = paste(a,"votegt0200.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt03, file = paste(a,"votegt0300.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt04, file = paste(a,"votegt0400.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt05, file = paste(a,"votegt0500.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt06, file = paste(a,"votegt0600.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt07, file = paste(a,"votegt0700.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt08, file = paste(a,"votegt0800.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(votegt09, file = paste(a,"votegt0900.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(p1, file = paste(a,"p100.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt01, file = paste(a,"s1votegt0100.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt02, file = paste(a,"s1votegt0200.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt03, file = paste(a,"s1votegt0300.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt04, file = paste(a,"s1votegt0400.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt05, file = paste(a,"s1votegt0500.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt06, file = paste(a,"s1votegt0600.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt07, file = paste(a,"s1votegt0700.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt08, file = paste(a,"s1votegt0800.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1votegt09, file = paste(a,"s1votegt0900.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(logsum, file = paste(a,"logsum00.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)
write.table(s1logsum, file = paste(a,"s1logsum00.txt"),quote= FALSE,row.names=FALSE,col.names=FALSE)


timeStamp=gsub(" ", "_", Sys.time())   #(or can use Sys.Date())
timeStamp=gsub(":", "-", timeStamp)
timeStamp=paste(a,timeStamp,".RData",sep="");
save.image(timeStamp)
#