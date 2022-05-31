# create a function to run in each itteration of the loop
oneIter <-function(winsize,noShiftNum,shiftStr,ktime) {

bwsz=winsize
a=paste("w",bwsz,"_",bwsz,"_",bwsz,sep="");
start= (noShiftNum+1) -bwsz + 1
type=temptrainfactor
x1=trainNum

pathData = paste(getwd(), "/", "generate New data 2","/dataset/rep",j,"_traindata.txt",sep="");
traindatax<- read.table(pathData)
pathData = paste(getwd(), "/", "generate New data 2","/dataset/rep",j,"_",shiftStr,".txt",sep="");
testnormalandabnormaldata<- read.table(pathData)

traindataxy=cbind(traindatax , type) # The incontrol data

#ktime=10#1500#the time of the window slide***************************************************
knumber=ktime-1

oobvectornormal=NULL;oobvectorabnormal=NULL;votevector=NULL;voteoobvector=NULL;oobvector=NULL;
votegt05=NULL;votesum=NULL;s1votesum=NULL;s1votegt05=NULL;
p1=NULL;logsum=NULL;s1logsum=NULL

for (k in 0:knumber){#------------------------------------{2}
temp2=testnormalandabnormaldata[(1+k):(bwsz+k),]
tempa=temp2
d2=NULL
for (dd in 0:(bwsz-1))
{
d2=rbind(d2,temp2[bwsz-dd,])
}
number=bwsz
type=matrix(c("a"),(number):1)
final=cbind(d2,type)
newtraindata=rbind(traindataxy,final)
my.rf <- randomForest(type ~.,sampsize=c(n=bwsz,a=bwsz), data=newtraindata) 
#print(my.rf)
ooblist=my.rf[5]
oobcolumn=matrix(c(ooblist[[1]]),2:3)
ooberrornormal= (oobcolumn[,3])[1]
ooberrorabnormal=(oobcolumn[,3])[2]
temp=my.rf[6]

votenumber=x1+number
temp2=matrix( c(temp[[1]]),votenumber,2)
p1vote=mean(temp2[(x1+1),2])# since the recent data we are putting the first postion

gt05=sum(temp2[(1:x1),1]>0.5)/x1

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
s1gt05=sum(temp2[((x1+1):votenumber),2]>0.5)/number

oobvectornormal=rbind(oobvectornormal,ooberrornormal)
oobvectorabnormal=rbind(oobvectorabnormal,ooberrorabnormal)
voteoobvector=rbind(voteoobvector,p1vote)

#--combine

votegt05=rbind(votegt05,gt05)
s1votegt05=rbind(s1votegt05,s1gt05)
votesum=rbind(votesum,gtsum)
s1votesum=rbind(s1votesum,s1gtsum)

logsum=rbind(logsum,logsum0)
s1logsum=rbind(s1logsum,s1logsum0)

p1=rbind(p1,p1vote)

}#----------------------------------{2}


tempResult <- list(p1=p1,votegt05=votegt05,s1votegt05=s1votegt05,
votesum=votesum,s1votesum=s1votesum,logsum=logsum,s1logsum=s1logsum)
 
return(tempResult)
}

