library(ggplot2)
library(tikzDevice)

#Inputs
n=7
k=5

#We now create a vector housing all the points from m=1 to m=2k+2
Mvec=1:(2*k+2)

#We now wish to use the function to give the upper bound for each m
Upperbound<-vector(length=length(Mvec))


for(i in length(Upperbound):1)
{
  #Working out the starting period it is either 1 or 3 (as m even)
  if((k-(Mvec[length(Mvec)]/2)) %% 2 ==1)
  {
    addon=2
  }
  else
  {
    addon=0
  }
  PeriodIndicator=(i+addon)%%4+1
  if(PeriodIndicator==1)
  {
    Upperbound[i]=(2*Mvec[i])/(4*n+2*k+Mvec[i]-4)
  }
  if(PeriodIndicator==2)
  {
    Upperbound[i]=(2*Mvec[i])/(4*n+2*k+Mvec[i]-5)
  }
  if(PeriodIndicator==3)
  {
    Upperbound[i]=(2*Mvec[i])/(4*n+2*k+Mvec[i]-2)
  }
  if(PeriodIndicator==4)
  {
    Upperbound[i]=(2*Mvec[i])/(4*n+2*k+Mvec[i]-3)
  }
}

#we will also plot the Lower bound provided by the combinatorial improvement
LowerBound<-vector(length=length(Mvec))

for(i in length(Mvec):1)
{
 if(Mvec[i]<= 2*(n-1) && Mvec[i]<= 2*(n+k))
 {
   LowerBound[i]=(2*Mvec[i])/(2*(n+k)+Mvec[i]*(1+ (n-1)/(floor(Mvec[i]/2))))
 }
 else if(Mvec[i] > 2*(n-1) && Mvec[i]<=(n+k))
 {
   LowerBound[i]=(Mvec[i])/(n+k+Mvec[i])
 }
 else
 {
  #LowerBound[i]=(Mvec[i])/(2*(n+k)) Hamiltonian simulation
  LowerBound[i]=NaN 
 }   
}

Equal<-as.matrix(Upperbound*(Upperbound==LowerBound))
Equal[Equal==0]<-NaN
EqualDF<-as.data.frame(cbind(Mvec,Equal))

UpperDF<-as.data.frame(cbind(Mvec,Upperbound))
LowerDF<-as.data.frame(cbind(Mvec,LowerBound))


#Images will be size
w=5
h=3


#tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/Patrolling-Games-on-Graph-Collection/Images/UpperBoundPlot(n=",toString(n),",k=",toString(k),").tex",sep=""),width=w,height=h)

UpperboundPlot<-ggplot(LowerDF)+
  geom_point(aes(x=UpperDF[,1],y=UpperDF[,2]))+
  geom_line(aes(x=UpperDF[,1],y=UpperDF[,2]))+
  geom_line(aes(x=LowerDF[,1],y=LowerDF[,2]),color='red')+
  geom_point(aes(x=EqualDF[,1],y=EqualDF[,2]),color='green')+
  xlab("$m$")+ylab("Bound")

print(UpperboundPlot)
dev.off()
print(UpperboundPlot)