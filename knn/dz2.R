olive=read.table("D:\\olive_data.csv", header=F,sep=";")
names(olive)=c("area","region","palmitic","palmitoleic","stearic", "oleic","linoleic","linolenic","arachidic",
               "eicosenoic")
olive2<-olive[,-c(2)]

s<-sample(1:nrow(olive2),400,replace=F)
train<-as.data.frame(scale(olive2[s,-1]))
train_labels<-as.factor(olive2[s,1])
test<-as.data.frame(scale(olive2[-s,-1]))
test_labels<-as.factor(olive2[-s,1])

library(class)


set.seed(1234)
a<-rep(0,15)
for( i in 1:15){
  classifier<-knn(train,test,train_labels,k=i)
  a[i]<-sum(classifier!=test_labels)
}

##### CV
#10 фолдов
#перемешаем выборку
scv<-sample(1:nrow(olive2),nrow(olive2),replace=F)
olive3<-olive2[scv,]

cv_break<-round(seq(1,572,length.out = 11))
set.seed(1234)
b<-rep(1:15)
for(j in 1:15){
a<-rep(0,10)
for (i in 1:10){
  classifier<-knn(as.data.frame(scale(olive3[-c(cv_break[i]:cv_break[i+1]),-1])),
                                as.data.frame(scale(olive3[cv_break[i]:cv_break[i+1],-1])),
                                as.factor(olive3[-c(cv_break[i]:cv_break[i+1]),1]),
                                k=j)
   a[i]<-sum(classifier!=as.factor(olive3[c(cv_break[i]:cv_break[i+1]),1]))
}
b[j]<-mean(a)
}
which.min(b)

k=3
classifier<-knn(train,test,train_labels,k=3)
sum(classifier!=test_labels)
### 6 ошибок
table(classifier,test_labels)

olive_pred<-scale(rbind(olive2[,-1],c(1011,  52,  260,	7924,	678,	151,	70,	44)))
knn(olive_pred[1:572,],olive_pred[573,],as.factor(olive2[,1]),k=3,prob=T)

ol.dist=dist(olive_pred[,c(1:8)])
ol.mds=cmdscale(ol.dist)

library(ggplot2)
ol.mds2<-as.data.frame(cbind(ol.mds[1:572,],olive$area))
ol.mds3<-as.data.frame(ol.mds)
qplot(ol.mds2$V1,ol.mds2$V2,colour=as.factor(ol.mds2$V3))+geom_point(aes(x=ol.mds3[573,1],y=ol.mds3[573,2]),size=5,shape=18)
