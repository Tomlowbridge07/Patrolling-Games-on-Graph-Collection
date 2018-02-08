library(ggplot2)
library(tikzDevice)

options(tz="UK")

n=10
k=c(5,3,2,1)
kMax=max(k)
kSum=sum(k)
extend=2

#forming region 1: i.e m>2(n+|k|)
m1=c(seq(2*(n+kSum)+1,2*(n+kSum)+extend,1))
GameValue1=c(rep(1,extend))
mRegion1=matrix(rbind(m1,GameValue1),nrow=extend,ncol=2,byrow=TRUE)

#forming region 2: 2(k_max+1)<=m<=2(n+|k|)
m2=c(seq(2*(kMax+1),2*(n+kSum),1))
GameValue2=m2*(1/(2*(n+kSum)))
mRegion2=matrix(rbind(m2,GameValue2),nrow=2*(n+kSum-kMax),ncol=2,byrow=TRUE)

mRegion=rbind(mRegion1,mRegion2)

#Forming other regions has yet to be defined

FinalRegion=mRegion

size=nrow(mRegion)
mAltRegion1=rbind(mRegion1,matrix(rep(NaN,2*(size-nrow(mRegion1))),nrow=size-nrow(mRegion1),ncol=2,byrow=TRUE))

#Plotting graph
df<-as.data.frame(mRegion)
df1<-as.data.frame(mRegion1)

tikz(file=paste("/local/pmxtol/Dropbox/R/GeneralStarRegions(S_",toString(n),"_",paste(k,collapse=','),").tex",sep=""),width=3,height=2)

valueplot<-ggplot(df,show.legend='True')+
  geom_point(aes(x = mRegion[,1], y = mRegion[,2]))+
  geom_line(aes(x = mRegion[,1], y = mRegion[,2]))+
  #geom_point(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  #geom_line(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  annotate("rect",xmin=2*(n+kSum)+0.55,xmax=2*(n+kSum)+extend+0.45,ymin=0.00,ymax=1.05,color='yellow',fill='yellow',alpha=.2)+
  annotate("rect",xmin=2*(kMax+1)-1+0.55,xmax=2*(n+kSum)+0.45,ymin=0.00,ymax=1.05,color='red',fill='red',alpha=.2)+
  #annotate("rect",xmin=8.55,xmax=9.45,ymin=0.00,ymax=1.05,color='purple',fill='purple',alpha=.2)+
  #annotate("rect",xmin=2.55,xmax=8.45,ymin=0.00,ymax=1.05,color='blue',fill='blue',alpha=.2)+
  #annotate("rect",xmin=1.55,xmax=2.45,ymin=0.00,ymax=1.05,color='green',fill='green',alpha=.2)+
  #annotate("rect",xmin=0.55,xmax=1.45,ymin=0.00,ymax=1.05,color='blue',fill='blue',alpha=.2)+
  xlab("Attack time, $m$")+ylab(paste("Value, $V \\left(S_{",toString(n),"}^{",paste(k,collapse=','),"} \\right)$"))
  #theme(axis.title = element_text(face="bold", size=20),axis.text= element_text(size=20))
  
print(valueplot)
dev.off()

print(valueplot)
