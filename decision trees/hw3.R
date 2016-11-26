dftrain<-read.table("D:\\hw3\\DataSetAdult.txt",sep=",")

dftest<-read.table("D:\\hw3\\AdultTest.txt",sep=",")

#добавим имена столбцам
names(dftrain)<-c('age',
                  'workclass',
                  'fnlwgt',
                  'education',
                  'education-num',
                  'marital-status',
                  'occupation',
                  'relationship',
                  'race',
                  'sex',
                  'capital-gain',
                  'capital-loss',
                  'hours-per-week',
                  'native-country',
                  'target')

names(dftest)<-c('age',
                  'workclass',
                  'fnlwgt',
                  'education',
                  'education-num',
                  'marital-status',
                  'occupation',
                  'relationship',
                  'race',
                  'sex',
                  'capital-gain',
                  'capital-loss',
                  'hours-per-week',
                  'native-country',
                  'target')
#уберем столбец с весами
dftrain<-dftrain[,-3]
dftest<-dftest[,-3]

#загрузим библиотеки
library(rpart)
library(ggplot2)
install.packages("rpart.plot")
library(rpart.plot)

#Посмотрим, как отличается доход для разных категорий граждан
ggplot(dftrain,aes(race))+geom_bar(aes(fill =target ), position = "dodge")
ggplot(dftrain,aes(workclass))+geom_bar(aes(fill =target ), position = "dodge")
ggplot(dftrain,aes(as.factor(dftrain$'education-num')))+geom_bar(aes(fill =target ), position = "dodge")
ggplot(dftrain,aes(dftrain$'marital-status'))+geom_bar(aes(fill =target ), position = "dodge")
ggplot(dftrain,aes(occupation))+geom_bar(aes(fill =target ), position = "dodge")
ggplot(dftrain,aes(relationship))+geom_bar(aes(fill =target ), position = "dodge")
ggplot(dftrain,aes(sex))+geom_bar(aes(fill =target ), position = "dodge")

#посмотрим, есть ли смысл логарифмировать
cor(log(dftrain[which(dftrain$`capital-gain`!=0),10]),as.numeric(dftrain[which(dftrain$`capital-gain`!=0),14]))

ggplot(dftrain,aes(as.factor(dftrain$`hours-per-week`)))+geom_bar(aes(fill =target ), position = "dodge")
ggplot(dftrain,aes(dftrain$`native-country`))+geom_bar(aes(fill =target ), position = "dodge")

#построим деревья решений
model1<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket = 5,cp=0.01,maxdepth = 8))
printcp(model1)
plotcp(model1)
summary(model1)
barplot(model1$variable.importance)


rpart.plot(model1,type=2,extra=4)

p1<-predict(model1,dftest,type="class")
table(dftest$target,p1)
sum(diag(table(dftest$target,p1)))/16281
#0.8445427
p2<-predict(model1,dftrain,type="class")
table(dftrain$target,p2)
sum(diag(table(dftrain$target,p2)))/24279
#0.8442687

model2<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket = 5,cp=0.001,maxdepth = 8,cv=10))
printcp(model2)
plotcp(model2)
rpart.plot(model2,type=2,extra=4)

p3<-predict(model2,dftest,type="class")
table(dftest$target,p3)
sum(diag(table(dftest$target,p3)))/16281
#0.8607579
p4<-predict(model2,dftrain,type="class")
table(dftrain$target,p4)
sum(diag(table(dftrain$target,p4)))/24279
#0.8651098

model3<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket = 5,cp=0.001,maxdepth = 7,cv=10))

p5<-predict(model3,dftest,type="class")
table(dftest$target,p5)
sum(diag(table(dftest$target,p5)))/16281
#0.8576869
p6<-predict(model3,dftrain,type="class")
table(dftrain$target,p6)
sum(diag(table(dftrain$target,p6)))/24279
#0.8625149

model4<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket = 5,cp=0.001,maxdepth = 9,cv=10))
p7<-predict(model4,dftest,type="class")
table(dftest$target,p7)
sum(diag(table(dftest$target,p7)))/16281
#0.8606351
p8<-predict(model4,dftrain,type="class")
table(dftrain$target,p8)
sum(diag(table(dftrain$target,p8)))/24279
#0.8653981

model5<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket = 5,cp=0.001,maxdepth =10))
p9<-predict(model5,dftest,type="class")
table(dftest$target,p9)
sum(diag(table(dftest$target,p9)))/16281
#0.8616793
p10<-predict(model5,dftrain,type="class")
table(dftrain$target,p10)
sum(diag(table(dftrain$target,p10)))/24279
#0.8677458
#Точность увеличилась совсем на чуть-чуть, поэтому оставим модель номер 4

#Добавим фичи
dftrain$race_other<-as.factor(ifelse(dftrain$race==" Other",1,0))
dftest$race_other<-as.factor(ifelse(dftest$race==" Other",1,0))
dftrain$`education-num_more_8`<-as.factor(ifelse(dftrain$`education-num`>8,1,0))
dftest$`education-num_more_8`<-as.factor(ifelse(dftest$`education-num`>8,1,0))
dftrain$pre_four<-as.factor(ifelse((dftrain$education==" 1st-4th")|(dftrain$education==" Preschool"),1,0))
dftest$pre_four<-as.factor(ifelse((dftest$education==" 1st-4th")|(dftest$education==" Preschool"),1,0))
dftrain$doc_prof<-as.factor(ifelse((dftrain$education==" Prof-school")|(dftrain$education==" Doctorate"),1,0))
dftest$doc_prof<-as.factor(ifelse((dftest$education==" Prof-school")|(dftest$education==" Doctorate"),1,0))
dftrain$bachelors<-as.factor(ifelse(dftrain$education==" Bachelors",1,0))
dftest$bachelors<-as.factor(ifelse(dftest$education==" Bachelors",1,0))
dftrain$mar_civ<-as.factor(ifelse(dftrain$`marital-status`==" Married-civ-spouse",1,0))
dftest$mar_civ<-as.factor(ifelse(dftest$`marital-status`==" Married-civ-spouse",1,0))
dftrain$prec_work<-as.factor(ifelse((dftrain$occupation==" Exec-managerial")|(dftrain$occupation==" Prof-specialty"),1,0))
dftest$prec_work<-as.factor(ifelse((dftest$occupation==" Exec-managerial")|(dftest$occupation==" Prof-specialty"),1,0))
dftrain$serv_house<-as.factor(ifelse(dftrain$occupation==" Priv-house-serv",1,0))
dftest$serv_house<-as.factor(ifelse(dftest$occupation==" Priv-house-serv",1,0))
dftrain$husband_wife<-as.factor(ifelse((dftrain$relationship==" Husband")|(dftrain$relationship==" Wife"),1,0))
dftest$husband_wife<-as.factor(ifelse((dftest$relationship==" Husband")|(dftest$relationship==" Wife"),1,0))
dftrain$log_capt_gain_not_null<-ifelse(dftrain$`capital-gain`>0,log(dftrain$`capital-gain`),0)
dftest$log_capt_gain_not_null<-ifelse(dftest$`capital-gain`>0,log(dftest$`capital-gain`),0)
dftrain$log_capt_loss_not_null<-ifelse(dftrain$`capital-loss`>0,log(dftrain$`capital-loss`),0)
dftest$log_capt_loss_not_null<-ifelse(dftest$`capital-loss`>0,log(dftest$`capital-loss`),0)
dftrain$hours_per_week_more_45<-as.factor(ifelse(dftrain$`hours-per-week`>45,1,0))
dftest$hours_per_week_more_45<-as.factor(ifelse(dftest$`hours-per-week`>45,1,0))
dftrain$without_work<-as.factor(ifelse((dftrain$workclass==" Never-worked")|(dftrain$workclass==" Without-pay"),1,0))
dftest$without_work<-as.factor(ifelse((dftest$workclass==" Never-worked")|(dftest$workclass==" Without-pay"),1,0))

model6<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket = 5,cp=0.001,maxdepth = 9,cv=10))
p11<-predict(model6,dftest,type="class")
table(dftest$target,p11)
sum(diag(table(dftest$target,p11)))/16281
#0.8606351
p12<-predict(model6,dftrain,type="class")
table(dftrain$target,p12)
sum(diag(table(dftrain$target,p12)))/24279
#0.8653981

summary(model6)
barplot(model6$variable.importance)
#Хотя некоторые добавленные переменные вошли в значимые, модель не улучшилась
model7<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket=5,cp=0,0005,maxdepth = 9))

p13<-predict(model7,dftest,type="class")
table(dftest$target,p13)
sum(diag(table(dftest$target,p13)))/16281
#0.8606351
p14<-predict(model7,dftrain,type="class")
table(dftrain$target,p14)
sum(diag(table(dftrain$target,p14)))/24279
#0.8653981
# модель переобучена

# заменим неизвестные переменные на моду
dftrain2<-dftrain[,1:14]
dftrain2$workclass<-ifelse(dftrain2$workclass==" ?"," Private",dftrain2$workclass)
dftrain2$education<-ifelse(dftrain2$education==" ?"," HS-grad",dftrain2$education)
dftrain2$`marital-status`<-ifelse(dftrain2$`marital-status`==" ?"," Married-civ-spouse",dftrain2$`marital-status`)
dftrain2$occupation<-ifelse(dftrain2$occupation==" ?"," Prof-specialty",dftrain2$occupation)
dftrain2$relationship<-ifelse(dftrain2$relationship==" ?"," Husband",dftrain2$relationship)

dftest2<-dftest[,1:14]
dftest2$workclass<-ifelse(dftest2$workclass==" ?"," Private",dftest2$workclass)
dftest2$education<-ifelse(dftest2$education==" ?"," HS-grad",dftest2$education)
dftest2$`marital-status`<-ifelse(dftest2$`marital-status`==" ?"," Married-civ-spouse",dftest2$`marital-status`)
dftest2$occupation<-ifelse(dftest2$occupation==" ?"," Prof-specialty",dftest2$occupation)
dftest2$relationship<-ifelse(dftest2$relationship==" ?"," Husband",dftest2$relationship)

model8<-rpart(target~.,data=dftrain2,method="class",control=rpart.control(minsplit=15,minbucket=5,cp=0,001,maxdepth = 9))
p14<-predict(model8,dftrain2,type="class")
table(dftrain2$target,p14)
sum(diag(table(dftrain2$target,p14)))/24279
#0.8728943
p13<-predict(model8,dftest2,type="class")
table(dftest2$target,p13)
sum(diag(table(dftest2$target,p13)))/16281
#0.8595295
#модель переобучена

# Итого лучшая модель для дерева решений - модель номер 4
#model4<-rpart(target~.,data=dftrain,method="class",control=rpart.control(minsplit=15,minbucket = 5,cp=0.001,maxdepth = 9,cv=10))
#точность на тестовой выборке 0.8606351
#точность на тренировочной выборки 0.8653981

#knn
dftrain3<-read.table("D:\\hw3\\DataSetAdult.txt",sep=",")

dftest3<-read.table("D:\\hw3\\AdultTest.txt",sep=",")

dftrain3<-dftrain[,c(1,4,12,15,16,17,18,19,20,21,22,23,24,25,26,27)]
dftrain3$age<-scale(dftrain3$age)
dftrain3$`education-num`<-scale(dftrain3$`education-num`)
dftrain3$`hours-per-week`<-scale(dftrain3$`hours-per-week`)
dftrain3$log_capt_gain_not_null<-scale(dftrain3$log_capt_gain_not_null)
dftrain3$log_capt_loss_not_null<-scale(dftrain3$log_capt_loss_not_null)
dftrain3$`education-num_more_8`<-as.numeric(dftrain3$`education-num_more_8`)
dftrain3$race_other<-as.numeric(dftrain3$race_other)
dftrain3$pre_four<-as.numeric(dftrain3$pre_four)
dftrain3$doc_prof<-as.numeric(dftrain3$doc_prof)
dftrain3$bachelors<-as.numeric(dftrain3$bachelors)
dftrain3$mar_civ<-as.numeric(dftrain3$mar_civ)
dftrain3$prec_work<-as.numeric(dftrain3$prec_work)
dftrain3$serv_house<-as.numeric(dftrain3$serv_house)
dftrain3$husband_wife<-as.numeric(dftrain3$husband_wife)
dftrain3$hours_per_week_more_45<-as.numeric(dftrain3$hours_per_week_more_45)
dftrain3$without_work<-as.numeric(dftrain3$without_work)



sample_knn<-sample(1:24279,17500,replace=F)
dftrain_knn<-dftrain3[sample_knn,]
target<-as.factor(dftrain[sample_knn,14])
target_test<-as.factor(dftrain[-sample_knn,14])
dftrain_test_knn<-dftrain3[-sample_knn,]


library(class)
set.seed(42)
a<-rep(0,15)
for( i in 1:15){
  classifier<-knn(dftrain_knn,dftrain_test_knn,target,k=i)
  a[i]<-sum(classifier!=target_test)
}
a
which.min(a)
#k=9
 
classifier<-knn(dftrain_knn,dftrain_test_knn,target,k=9)
(6779-sum(classifier!=target_test))/6779
#0.8384718

dftest3<-dftest[,c(1,4,12,15,16,17,18,19,20,21,22,23,24,25,26,27)]
dftest3$age<-scale(dftest3$age)
dftest3$`education-num`<-scale(dftest3$`education-num`)
dftest3$`hours-per-week`<-scale(dftest3$`hours-per-week`)
dftest3$log_capt_gain_not_null<-scale(dftest3$log_capt_gain_not_null)
dftest3$log_capt_loss_not_null<-scale(dftest3$log_capt_loss_not_null)
dftest3$`education-num_more_8`<-as.numeric(dftest3$`education-num_more_8`)
dftest3$race_other<-as.numeric(dftest3$race_other)
dftest3$pre_four<-as.numeric(dftest3$pre_four)
dftest3$doc_prof<-as.numeric(dftest3$doc_prof)
dftest3$bachelors<-as.numeric(dftest3$bachelors)
dftest3$mar_civ<-as.numeric(dftest3$mar_civ)
dftest3$prec_work<-as.numeric(dftest3$prec_work)
dftest3$serv_house<-as.numeric(dftest3$serv_house)
dftest3$husband_wife<-as.numeric(dftest3$husband_wife)
dftest3$hours_per_week_more_45<-as.numeric(dftest3$hours_per_week_more_45)
dftest3$without_work<-as.numeric(dftest3$without_work)

target<-as.factor(dftrain[,14])
target_test<-as.factor(dftest)

classifier<-knn(dftrain3,dftest3,target,k=9)
levels(target_test)<-c(" <=50K"," >50K")
levels(classifier)
(16281-sum(classifier!=target_test))/16281
#на тестовом множестве результат оказался намного хуже 0.7620539
#Таким образом, для данной задачи дерево решений подходит лучше, чем knn.





