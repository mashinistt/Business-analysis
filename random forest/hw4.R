df<-read.table("C:\\Users\\Vladimir\\Downloads\\olive_data.csv",sep = ";",header=F)
names(df)=c("area","region","palmitic","palmitoleic","stearic", "oleic","linoleic","linolenic","arachidic",
               "eicosenoic")
olive<-read.table("D:\\olive_data.csv", header=F,sep=";")
names(olive)=c("area","region","palmitic","palmitoleic","stearic", "oleic","linoleic","linolenic","arachidic",
               "eicosenoic")
olive2<-olive[,-c(2)]

set.seed(42)
s<-sample(1:nrow(olive2),400,replace=F)
train<-olive2[s,-1]
train_labels<-as.factor(olive2[s,1])
test<-olive2[-s,-1]
test_labels<-as.factor(olive2[-s,1])

install.packages("randomForest")
library(randomForest)

barplot(table(train_labels))
barplot(table(test_labels))

set.seed(42)
model1<-randomForest(train,train_labels,ntree=500,mtry=3,sampsize=253,nodesize = 1,replace=T,importance = T,
                     localImp = F,proximity = F,norm.votes = T,do.trace = 25,keep.forest = T,
                     corr.bias = F,keep.inbag = F)
set.seed(1234)
model1_opt<-randomForest(train,train_labels,ntree=100,mtry=3,sampsize=253,nodesize = 1,replace=T,importance = T,
                         localImp = F, proximity = F,norm.votes = T,do.trace =10,keep.forest = T,
                         corr.bias = F,keep.inbag = F)
# 100 более чем достаточно
p1_train<-predict(model1_opt,train)
sum(diag(table(p1_train,train_labels)))/sum(table(p1_train,train_labels))
#1
p1<-predict(model1_opt,test)
sum(diag(table(p1,test_labels)))/sum(table(p1,test_labels))
#0.9534884
varImpPlot(model1_opt)
#–езультаты немного отличаютс€, но в целом переменные сильно вли€ющие на точность сильно вли€ют и на 
#индекс ƒжини
using_variables<-varUsed(model1_opt, by.tree=F,count=T)
names(using_variables)<-c("palmitic","palmitoleic","stearic", "oleic","linoleic","linolenic","arachidic",
"eicosenoic")
barplot(using_variables)
#как видим частота использовани€ представл€ет что-то среднее
#между двум€ предыдущими графиками
set.seed(42)
model2<-randomForest(train,train_labels,ntree=100,mtry=3,sampsize=253,nodesize = 5,replace=T,importance = T,
                     localImp = F,proximity = F,norm.votes = T,do.trace = 15,keep.forest = T,
                     corr.bias = F,keep.inbag = F)
#
p2_train<-predict(model2,train)
sum(diag(table(p2_train,train_labels)))/sum(table(p2_train,train_labels))
#0.995
p2<-predict(model2,test)
sum(diag(table(p2,test_labels)))/sum(table(p2,test_labels))
#0.9534884

set.seed(42)
model3<-randomForest(train,train_labels,ntree=100,mtry=3,sampsize=253,nodesize = 15,replace=T,importance = T,
                     localImp = F,proximity = F,norm.votes = T,do.trace = 15,keep.forest = T,
                     corr.bias = F,keep.inbag = F)
#
p3_train<-predict(model3,train)
sum(diag(table(p3_train,train_labels)))/sum(table(p3_train,train_labels))
#0.995
p3<-predict(model3,test)
sum(diag(table(p3,test_labels)))/sum(table(p3,test_labels))


barplot(table(train_labels))
table(p1,test_labels)
table(train_labels)

df_cs<-olive2[which(olive2$area=="Coast-Sardinia"),]
df_cs<-df_cs[1:20,]

df_napl<-olive2[which(olive2$area=="North-Apulia"),]
df_napl<-df_napl[1:25,]

df_sc<-olive2[which(olive2$area=="Sicily"),]
df_sc<-df_sc[1:25,]

train_labels<-c(train_labels,df_cs$area,df_napl$area,df_sc$area)
df_cs<-df_cs[,-1]
df_napl<-df_napl[,-1]
df_sc<-df_sc[,-1]

df_train1<-rbind(df_cs,df_napl,df_sc)
df_train<-rbind(df_train,df_train1)
df_train<-rbind(train,df_train1)

train_labels<-as.factor(train_labels)
set.seed(2016)
model1_new<-randomForest(df_train,train_labels,ntree=70,mtry=3,sampsize=292,nodesize = 1,replace=T,importance = T,
                         localImp = F, proximity = F,norm.votes = T,do.trace =10,keep.forest = T,
                         corr.bias = F,keep.inbag = F)

p3_train<-predict(model1_new,df_train)
sum(diag(table(p3_train,train_labels)))/sum(table(p3_train,train_labels))
#0.995
p3<-predict(model1_new,test)
sum(diag(table(p3,test_labels)))/sum(table(p3,test_labels))
