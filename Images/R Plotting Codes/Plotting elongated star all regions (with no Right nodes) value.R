library(ggplot2)
library(tikzDevice)

options(tz="UK")

n=7
k=5
extend=2

#forming region 1: i.e m>2(n+k)
m1=c(seq(2*(n+k)+1,2*(n+k)+extend,1))
GameValue1=c(rep(1,extend))
mRegion1=matrix(rbind(m1,GameValue1),nrow=extend,ncol=2,byrow=TRUE)

#forming region 2: 2(k+1)<=m<=2(n+k)
m2=c(seq(2*(k+1),2*(n+k),1))
GameValue2=m2*(1/(2*(n+k)))
mRegion2=matrix(rbind(m2,GameValue2),nrow=2*n-1,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion1,mRegion2)

#forming region 3: m=2k+1 and m=2k
m3=c(2*k,2*k+1)
GameValue3=m3/(m3+2*(n-1))
mRegion3=matrix(rbind(m3,GameValue3),nrow=2,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion,mRegion3)

#forming region 4: 3<=m<=2k-1
m4=c(seq(3,2*k-1,1))
GameValue4=(2*m4)/(2*(n+k)+m4+2*(n-1))
mRegion4=matrix(rbind(m4,GameValue4),nrow=2*k-3,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion,mRegion4)

#forming region 5: m=2 and m=1
m5=c(1,2)
GameValue5=c(1/(n+k+2),1/(n-1+floor(k/2)))
mRegion5=matrix(rbind(m5,GameValue5),nrow=2,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion,mRegion5)

#Forming other regions has yet to be defined

FinalRegion=mRegion

size=nrow(mRegion)
mAltRegion1=rbind(mRegion1,matrix(rep(NaN,2*(size-nrow(mRegion1))),nrow=size-nrow(mRegion1),ncol=2,byrow=TRUE))

#Plotting graph
df<-as.data.frame(mRegion)
df1<-as.data.frame(mRegion1)

#Setting width and height of output
w=5
h=3

tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/Patrolling-Games-on-Graph-Collection/Images/LargenElongatedStarRegions(S_",toString(n),"_",toString(k),").tex",sep=""),width=w,height=h)

valueplot<-ggplot(df,show.legend='True')+
  geom_point(aes(x = mRegion[,1], y = mRegion[,2]))+
  geom_line(aes(x = mRegion[,1], y = mRegion[,2]))+
  #geom_point(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  #geom_line(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  annotate("rect",xmin=2*(n+k)+0.55,xmax=2*(n+k)+extend+0.45,ymin=0.00,ymax=1.05,color='yellow',fill='yellow',alpha=.2)+
  annotate("rect",xmin=2*(k+1)-1+0.55,xmax=2*(n+k)+0.45,ymin=0.00,ymax=1.05,color='red',fill='red',alpha=.2)+
  annotate("rect",xmin=2*k-1+0.55,xmax=2*k+1+0.45,ymin=0.00,ymax=1.05,color='purple',fill='purple',alpha=.2)+
  annotate("rect",xmin=2+0.55,xmax=2*k-1+0.45,ymin=0.00,ymax=1.05,color='blue',fill='blue',alpha=.2)+
  annotate("rect",xmin=1+0.55,xmax=2+0.45,ymin=0.00,ymax=1.05,color='green',fill='green',alpha=.2)+
  annotate("rect",xmin=0.55,xmax=1.45,ymin=0.00,ymax=1.05,color='orange',fill='orange',alpha=.2)+
  xlab("Attack time, $m$")+ylab(paste("Value, $V(S_{",toString(n),"}^{",toString(k),"})$"))
  #theme(axis.title = element_text(face="bold", size=20),axis.text= element_text(size=20))
  
print(valueplot)
dev.off()
print(valueplot)
  
#ggsave(filename = "LineRegions(L_10).png",path="/local/pmxtol/Dropbox/R",plot=valueplot,dpi=600)
