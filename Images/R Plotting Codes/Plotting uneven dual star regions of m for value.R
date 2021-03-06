library(ggplot2)
library(tikzDevice)

options(tz="UK")

#PLEASE NOTE n1>n2
n1=6
n2=4
extend=2

#forming region 1: i.e m>2(n1+n2+1)
m1=c(seq(2*(n1+n2+1)+1,2*(n1+n2+1)+1+extend,1))
GameValue1=c(rep(1,extend))
mRegion1=matrix(rbind(m1,GameValue1),nrow=extend,ncol=2,byrow=TRUE)

#forming region 2: 2(n2+1)<=m<=2(n1+n2+1)
m2=c(seq(2*n2+2,2*(n1+n2+1),1))
GameValue2=m2*(1/(2(n1+n2+1)))
mRegion2=matrix(rbind(m2,GameValue2),nrow=2*n1+1,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion1,mRegion2)

#NOTE: we are skipping m=2n2+1 as we don't know the answer

#forming region 3: m<=2n2
m3=c(seq(1,2*n2,1))
GameValue3=m3*(1/(2(n1+n2)))
mRegion3=matrix(rbind(m3,GameValue3),nrow=2*n2,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion,mRegion3)


FinalRegion=mRegion

size=nrow(mRegion)
mAltRegion1=rbind(mRegion1,matrix(rep(NaN,2*(size-nrow(mRegion1))),nrow=size-nrow(mRegion1),ncol=2,byrow=TRUE))

#Plotting graph
df<-as.data.frame(mRegion)
df1<-as.data.frame(mRegion1)

tikz(file=paste("/local/pmxtol/Dropbox/R/DualStarRegions(S_",toString(n1),",",toString(n2),").tex",sep=""),width=3,height=2)

valueplot<-ggplot(df,show.legend='True')+
  geom_point(aes(x = mRegion[,1], y = mRegion[,2]))+
  geom_line(aes(x = mRegion[,1], y = mRegion[,2]))+
  #geom_point(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  #geom_line(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  annotate("rect",xmin=2*(n1+n2+1)+1+0.55,xmax=2*(n1+n2+1)+2+extend+0.45,ymin=0.00,ymax=1.05,color='yellow',fill='yellow',alpha=.2)+
  annotate("rect",xmin=2*n2+1+0.55,xmax=2*(n1+n2+1)+1+0.45,ymin=0.00,ymax=1.05,color='red',fill='red',alpha=.2)+
  annotate("rect",xmin=0.55,xmax=2*n2+0.45,ymin=0.00,ymax=1.05,color='purple',fill='blue',alpha=.2)+
  #annotate("rect",xmin=2.55,xmax=8.45,ymin=0.00,ymax=1.05,color='blue',fill='blue',alpha=.2)+
  #annotate("rect",xmin=1.55,xmax=2.45,ymin=0.00,ymax=1.05,color='green',fill='green',alpha=.2)+
  #annotate("rect",xmin=0.55,xmax=1.45,ymin=0.00,ymax=1.05,color='blue',fill='blue',alpha=.2)+
  xlab("Attack time, $m$")+ylab(paste("Value, $V \\left(S_{",toString(n1),"} \\bigodot S_{",toString(n2),"} \\right)$"))
  #theme(axis.title = element_text(face="bold", size=20),axis.text= element_text(size=20))
  
print(valueplot)
dev.off()

print(valueplot)
