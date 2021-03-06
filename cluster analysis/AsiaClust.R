df<-read.table("D:\\AsiaClust.csv",header=T,sep=";",dec=",")
library(ggplot2)
library(gridExtra)

��������� ���������� �� ��������� ���������:
 1) ������������ ���������� ������ ���������� �������� ������������� ��������� ������
 2) ���������� ����������� �������� ( ��� ��� �� ������ �������� ��������� ������� ����� �������� � ��������� �� ��������
                                      ������� ��� ��������, �� ��� ������ ������������ �������, �������� ����������� �� �����������
                                     �� ��������� ������, ���� ����� �� ������ ����������)
��������� ��� �� ���� ��������� � ������� ���� ����������� ������� � ����������

df1<-df
df1$GDP.Pop<-df1$GDP/df1$Population
names(df1)[7]<-"GDP/Pop"
df1<-df1[,-6]

����� ������������ ��������
df2<-df1
df2[,2:6]<-scale(df2[,2:6])

����� �������, � ����� ������ ������ ��� ����������� �������� � ��� ���������� �����������

������ ����� ������������ ������������� �������������

hclustWard <- hclust(dist(df2[,2:6]), method="ward.D")
hclustComplete<- hclust(dist(df2[,2:6]), method="complete")
hclustSingle<- hclust(dist(df2[,2:6]), method="single")

df2.dist=dist(df2[,2:6])
df2.mds=cmdscale(df2.dist)
qplot(df2.mds[,1],df2.mds[,2])
dfplot<-as.data.frame(df2.mds)

set.seed(1234)
kmean7<-kmeans(df2[,2:6], 7, iter.max = 100,nstart = 50)


dfplot$cl<-as.factor(kmean7$cluster)
xc<-rep(0,7)
for (i in 1:7){
  xc[i]<-sum(dfplot[which(dfplot[,3]==i),1])/length(dfplot[which(dfplot[,3]==i),1])
}
yc<-rep(0,7)
for (i in 1:7){
  yc[i]<-sum(dfplot[which(dfplot[,3]==i),2])/length(dfplot[which(dfplot[,3]==i),2])
}
centers<-data.frame(xc=xc,yc=yc)
ggplot(dfplot,aes(x=V1,y=V2))+geom_point(aes(col=cl),size=3)+geom_point(data=centers,aes(x=xc,y=yc),col="#f9e526", size=52, alpha=.3)+
  ggtitle("7 ���������")+theme(plot.title = element_text(lineheight=1, face="bold"))



plot(df2$`GDP/Pop`,df2$Jobless.rate)
plot(df$Population,df$Jobless.rate)

plot(hclustWard, labels=df2[ ,1],hang=-1,main="Ward")
plot(hclustComplete,labels=df2[ ,1],hang=-1,main="Complete")
plot(hclustSingle,labels=df2[ ,1],hang=-1,main="Single")

��� ����� ����� Single �� �������� � ������ ������ ��� �������������
������ ���������� ������� �� ��������� ��������

df2.dist<-dist(df2[,2:6])
df2.mds<-cmdscale(df2.dist)
plot(df2.mds)

plot(df2$`GDP/Pop`,df2$Jobless.rate) � �.�.

���������� Ward � Complete �����, ������� ��������� �� ��� ���������
#***************************
#�������� ������� ���������� �����

qplot(1:(length(df2[ ,1])-1),hclustWard$height,geom=c("line","point"))
qplot(1:(length(df2[ ,1])-1),hclustComplete$height,geom=c("line","point"))


plot(1:(length(df2[ ,1])-1),hclustWard$height,type='o')
# ���� �� ����������������� ���������� ���������, ������� �� ����, ��� ������ �� ����� ����������
# � ������. �� ����� ������� ��� 3, ��� � 5, � ���� 9
#��������� ��� ��� ��������
plot(hclustWard, labels=df2[ ,1])
rect.hclust(hclustWard, k=3, border="red")
rect.hclust(hclustWard, k=5, border="blue")
rect.hclust(hclustWard, k=9, border="green")
# ���� ��� �������� �������� �������������


plot(1:(length(df2[ ,1])-1),hclustComplete$height,type='o')
# ������ �� ����� �������, �� ��� ����� ������������� �������� 6 ���������
plot(hclustComplete, labels=df2[ ,1],hang = -1)
rect.hclust(hclustComplete, k=6, border="red")
# �����, ����� ������������ �� ���� ���������, ���� ����� ��������, ��� ���� ����������
# ���� �� ���������� �����, �� �� ������, ��� ������ ����� ���� ����� ������� �� 9 �����
# ������� ��������� ��������� ��������� �� 6 ���������, ����� ������� 9 ��������� ��� ������ ��������
# ������� ���������� ����� ����������
groupsC6 <- cutree(hclustComplete, k=6) 
groupsC6

#�������������� ��������� ������� � ������� ��� ���� �����������, ����� ���� ����� ����������������
#�������� ������ ���������
means<-apply(df1[,2:6],2,mean)
medians<-apply(df1[,2:6],2,median)
#�� ����������� ���� �������� ����� ��������������� �� ������� ��� � �������� ���������� ����������
q1<-qplot(df1$GDP,main="GDP",bins=50,xlab="")
q2<-qplot(df1$GDP.YoY,main="GDP.YOY",bins=50,xlab="")
q3<-qplot(df1$Inflation.rate,main="Inflation rate",bins=50,xlab="")
q4<-qplot(df1$Jobless.rate,main="Jobless rate",bins=50,xlab="")
q5<-qplot(df1$`GDP/Pop`,main="GDP per Population",bins=50,xlab="")

grid.arrange(q1,q2,q3,q4,q5,ncol=3)



#������� 1
colMeans(df1[groupsC6==1, 2:6])
barplot(colMeans(df2[groupsC6==1, 2:6]),main="������� 1")
df1[groupsC6==1,1]
#� ������ ������ ������ ����� ��-�� ������ ��������� ��� � ��������� ������ ����������� 

#������� 2
colMeans(df1[groupsC6==2, 2:6])
barplot(colMeans(df2[groupsC6==2, 2:6]),main="������� 2")
df1[groupsC6==2,1]
#� ������ ������ ������ ������ � �������� ����������, ���������� ������� ����� ������� � ���������
#����������� �� �������
medians
#������� 3
colMeans(df1[groupsC6==3, 2:6])
barplot(colMeans(df2[groupsC6==3, 2:6]),main="������� 3")
df1[groupsC6==3,1]
medians
# ����� �������������� �������, � ������� ���������� ��� �� ���� ��������� ������ � �������
# ���� ��������� �����. ��� �� �������� � ��������� �����������,����� ���, ������� �� ������ 
# ������ ��������, �� � � 2.7 ��� ������ ����������.
# ������� 4
colMeans(df1[groupsC6==4, 2:6])
barplot(colMeans(df2[groupsC6==4, 2:6]),main="������� 4")
df1[groupsC6==4,1]
medians
# � ���� ������� ������ ������ ����� �� ���� ����� ������� ��������, ������� ������� ���������
# �� ����������. ����� ������ ��������, ��� ��������� ���������� ������ �� ��� ��������
#������� 5
colMeans(df1[groupsC6==5, 2:6])
barplot(colMeans(df2[groupsC6==5, 2:6]),main="������� 5")
df1[groupsC6==5,1]
medians
#� ��� ������ ������ ����� ���� ������. �����. ��� ������ ����������, ����� �������
#�������� ��� � �������, �������� ���������� �������������� ���� � ��� ����. ����� � ������
# ����� �������, �� ��������� � ��������� �������� ������� �����������. ����� � ��� �������
# ����� �� ����� ������ ����� � ������� � ����� �� ����� ��������� ������� ��� �� ���� ���������
# �� ��-�� ����, ��� ������� ����� ��������� ���� ������ ������������ ��������������� ������
# �� ��-�� ������� ��� �� ����� � ������������� ������� ����� � ������, ��� ��������� ������

#������� 6
colMeans(df1[groupsC6==6, 2:6])
barplot(colMeans(df2[groupsC6==6, 2:6]),main="������� 6")
df1[groupsC6==6,1]
medians
#� ��� ������ ����� ���������� � ���������, ������������ ����� ������� ������� �����������, � ������� ���
# ������ ����� ��������, ��� ������� ��� � ���� ������� ������, ��� � ������� �� �������,
# � ������ ���������� ������

#���������, ��� ���������, ���� ������� ������ �� 9 ���������
groupsC9 <- cutree(hclustComplete, k=9) 
groupsW9 <- cutree(hclustWard, k=9) 


#������� 1
df1[groupsC9==1,1]
df1[groupsW9==1,1]
# � ���, � ��� �����
#������� 2
df1[groupsC9==2,1]
df1[groupsW9==2,1]
#������������ �������� ������ ���� ���������
#������� 3
df1[groupsC9==3,1]
df1[groupsW9==3,1]
colMeans(df1[groupsC9==3, 2:6])
colMeans(df1[groupsW9==3, 2:6])
barplot(colMeans(df2[groupsC9==3, 2:6]),main="������� C3")
barplot(colMeans(df2[groupsW9==3, 2:6]),main="������� W3")
medians
colMeans(df1[groupsC6==3, 2:6])
#��� ������ ����� ����� ���� �� ����� ������������ ����������
#��� ���� � ����� ������ ������� �� ������� ������� ������ � ������� �������������, ���� ������
#�� ��� � ��� ��������, �� ������� ��� �� ���� ���������
#������� 4
df1[groupsC9==4,1]
df1[groupsW9==5,1]
colMeans(df1[groupsC9==4, 2:6])
colMeans(df1[groupsW9==5, 2:6])
barplot(colMeans(df2[groupsC9==5, 2:6]),main="������� C4")
barplot(colMeans(df2[groupsW9==5, 2:6]),main="������� W4")
medians
colMeans(df1[groupsC6==3, 2:6])
#�������� ������� � ���������� �� ������� 3 �������� ����� 4, ���� ������ ������
# ��������� ������� ������������
#������� 5
df1[groupsC9==5,1]
df1[groupsW9==6,1]
colMeans(df1[groupsC9==5, 2:6])
colMeans(df1[groupsW9==6, 2:6])
barplot(colMeans(df2[groupsC9==5, 2:6]),main="������� C5")
barplot(colMeans(df2[groupsW9==6, 2:6]),main="������� W5")
medians
df1[groupsC6==2,]
#�������� ��� ������ � ����� ������� ��� �� ���� ���������, ������ ��������� � �������
#�����������
#������� 6
df1[groupsC9==6,1]
df1[groupsW9==7,1]
#�����
#������� 7
df1[groupsC9==7,1]
df1[groupsW9==4,1]
colMeans(df1[groupsC9==7, 2:6])
colMeans(df1[groupsW9==4, 2:6])
barplot(colMeans(df2[groupsC9==7, 2:6]),main="������� C7")
barplot(colMeans(df2[groupsW9==4, 2:6]),main="������� W4")
medians
colMeans(df1[groupsC6==3,2:6])
#���� ������� ���������� �� ���� ����� ������ ������� ��� � ��������
#������� ������� ���/�� ���� ���������, ������ ����� ��-�� ������� �����
#������� 8
df1[groupsC9==8,1]
df1[groupsW9==8,1]
#������� �����
df1[groupsC9==9,1]
df1[groupsW9==9,1]
colMeans(df1[groupsC9==9, 2:6])
colMeans(df1[groupsW9==9, 2:6])
barplot(colMeans(df2[groupsC9==9, 2:6]),main="������� C9")
barplot(colMeans(df2[groupsW9==9, 2:6]),main="������� W9")
medians
colMeans(df1[groupsC6==6,2:6])
#�������� �������

for (i in 1:9){
  print(df1[groupsW9==i,1])
}
for (i in 1:9){
  print(df1[groupsC9==i,1])
}
# ����� �������, ����� �������������� ����������� ������� ��������� ������������
# ���� ��������� �� ����� ��������� ��������� �� ��������, �� 
# ����� �������� 6 ��������� ������� ��������� ����� ���������� ��������� Complete
# ��� ����� ���������� ������������ ����� ����� 9 ��������� � ������������ ����� �����
# �������� ��� ��������� ������������
# ��������� ����� ���������� ���� knn
library(dendextend)
library(dplyr)
row.names(df2)<-df2[,1]
dendh6<-as.dendrogram(hclust(dist(df2[,2:6]), method="complete"))
dendh6%>%set("labels_colors",k=6)%>%set("branches_k_col",k=6)%>%set("branches_lwd",2)%>%plot(main="����������� ��� 6 ���������")

dendh9<-as.dendrogram(hclust(dist(df2[,2:6]), method="ward.D"))
dendh9%>%set("labels_colors",k=9)%>%set("branches_k_col",k=9)%>%set("branches_lwd",2)%>%plot(main="����������� ��� 9 ���������")


dendh99<-as.dendrogram(hclust(dist(df2[,2:6]), method="complete"))
dendh99%>%set("labels_colors",k=9)%>%set("branches_k_col",k=9)%>%set("branches_lwd",2)%>%plot(main="����������� ��� 9 ���������")

df2.dist=dist(df2[,2:6])
df2.mds=cmdscale(df2.dist)
qplot(df2.mds[,1],df2.mds[,2])


#kmeans
TW<-rep(0,15)
for (i in 1:15)
{
  TW[i]<-kmeans(df2[,2:6],centers=i)$tot.withinss
}
qplot(1:15, TW, xlab="Number of Clusters", ylab="Within groups sum of squares",geom=c("point","line"))
# ���� �� ����� ������� ����� ����������� �������� 5 � 7 ���������

kmean5<-kmeans(df2[,2:6], 5, iter.max = 100,nstart = 50)
kmean5$centers
#��������� �� ��������
for (i in 1:5){print(kmean5$cluster[kmean5$cluster==i])}
# ���������� ����� � ����, ��� ���� ������� ��� ������������� ������������� 
# ������� � ���, ��� � ������������� ������������� � ������ �������� �������� ����� � ����������
# � ����������. � kmeans ���������� � ����� ��������� � ����� ��������, � ��������� � ������ ���� ����� �������� ����� 
# ��������� �� ��� ������ ������������
df4<-df1[,2:6]
row.names(df4)<-df[,1]
df4[c("Afghanistan","Palestine","Yemen"),]
#�� ������� ���������� ������ �������, ����� ������������� ��������� �����, �� ��� �� 
#������ ���������� ��� ������� ����� ������ ��������
kmean7<-kmeans(df2[,2:6], 7, iter.max = 100,nstart = 50)
kmean7$centers
for (i in 1:7){print(kmean7$cluster[kmean7$cluster==i])}
