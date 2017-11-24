#load libraries
library(ggplot2)
library(tikzDevice)

A=data.frame(nrow=8,ncol = 2)
A[1,1]="1"
A[2,1]="2"
A[3,1]="3"
A[4,1]="4"
A[5,1]="5"
A[6,1]="6"
A[7,1]="c"
A[8,1]="*"

A[1,2]=4/18
A[2,2]=6/18
A[3,2]=8/18
A[4,2]=8/18
A[5,2]=8/18
A[6,2]=8/18
A[7,2]=10/18
A[8,2]=4/18

B=A
B[1,2]=(9/17)*A[1,2]+(2/17)
B[2,2]=(9/17)*A[2,2]+(2/17)
B[3,2]=(9/17)*A[3,2]+(2/17)
B[4,2]=(9/17)*A[4,2]
B[5,2]=(9/17)*A[5,2]
B[6,2]=(9/17)*A[6,2]+(6/17)
B[7,2]=(9/17)*A[7,2]+(6/17)
B[8,2]=(9/17)*A[8,2]+(2/17)

C=A
C[1,2]=(18/30)*A[1,2]+(4/30)
C[2,2]=(18/30)*A[2,2]+(4/30)
C[3,2]=(18/30)*A[3,2]+(4/30)
C[4,2]=(18/30)*A[4,2]
C[5,2]=(18/30)*A[1,2]
C[5,2]=(18/30)*A[5,2]+(4/30)
C[5,2]=(18/30)*A[5,2]
C[6,2]=(18/30)*A[6,2]+(4/30)
C[7,2]=(18/30)*A[7,2]+(8/30)
C[8,2]=(18/30)*A[8,2]+(4/30)

X=vector(length=8)
X=1:8

df<-data.frame(A,B,C)

#Images created are of size
w=5
h=3  

#Creating Base Plot and saving
tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/Patrolling-Games-on-Graph-Collection/Images/BaseInterceptionOnS_4^5(m=4).tex",sep=""),width=w,height=h)

BasePlot<-ggplot(df,show.legend='True') + geom_point(aes(x = A[,1], y = A[,2])) +
  geom_line(aes(x = X, y = A[,2]))+ylab('Probability')+xlab('Node')+
  theme(axis.title = element_text(face="bold", size=30),axis.text= element_text(size=30))+
  scale_x_discrete(limits=c("1","2","3","4","5","6","c","*"))
print(BasePlot)
dev.off()

print(BasePlot)

#Creatingh Navie Improvement Plot and saving
tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/Patrolling-Games-on-Graph-Collection/Images/NavieInterceptionOnS_4^5(m=4).tex",sep=""),width=w,height=h)
NaviePlot<-ggplot(df,show.legend='True') + geom_point(aes(x = A[,1], y = A[,2])) +
  geom_line(aes(x = X, y = A[,2]))+geom_point(aes(x = B[,1], y = B[,2]),colour='red')+
  geom_line(aes(x = X, y = B[,2]),colour='red')+ylab('Probability')+xlab('Node')+
  theme(axis.title = element_text(face="bold", size=30),axis.text= element_text(size=30))+
  scale_x_discrete(limits=c("1","2","3","4","5","6","c","*"))
print(NaviePlot)
dev.off()
print(NaviePlot)

#Creating Combinatorial Plot and saving
tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/Patrolling-Games-on-Graph-Collection/Images/CombinatorialInterceptionOnS_4^5(m=4).tex",sep=""),width=w,height=h)
CombinatorialPlot<-ggplot(df,show.legend='True') + geom_point(aes(x = A[,1], y = A[,2])) +
geom_line(aes(x = X, y = A[,2]))+geom_point(aes(x = B[,1], y = B[,2]),colour='red')+
  geom_line(aes(x = X, y = B[,2]),colour='red')+geom_point(aes(x = C[,1], y = C[,2]),colour='blue')+
  geom_line(aes(x = X, y = C[,2]),colour='blue')+ylab('Probability')+xlab('Node')+
  theme(axis.title = element_text(face="bold", size=30),axis.text= element_text(size=30))+
  scale_x_discrete(limits=c("1","2","3","4","5","6","c","*"))
print(CombinatorialPlot)
dev.off()
print(CombinatorialPlot)
