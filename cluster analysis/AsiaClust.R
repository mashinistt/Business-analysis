df<-read.table("D:\\AsiaClust.csv",header=T,sep=";",dec=",")
library(ggplot2)
library(gridExtra)

Параметры выбирались по следующим критериям:
 1) совокупность параметров должна адектватно отражать экономическое состояние страны
 2) отсутствие пропущенных значений ( так как во многих столбцах пропущено слишком много значений и адекватно их заменить
                                      средним или медианой, на мой взгляд неправильное рещение, логичнее попробовать их предсказать
                                     по имеющимся данным, либо взять из других источников)
Расчитаем ВВП на душу населения и заменим этим показателем столбец с населением

df1<-df
df1$GDP.Pop<-df1$GDP/df1$Population
names(df1)[7]<-"GDP/Pop"
df1<-df1[,-6]

далее пронормируем признаки
df2<-df1
df2[,2:6]<-scale(df2[,2:6])

Таким образом, в нашем наборе данных нет пропущенных значений и все показатели нормированы

Первой будем использовать иерархическую кластеризацию

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
  ggtitle("7 кластеров")+theme(plot.title = element_text(lineheight=1, face="bold"))



plot(df2$`GDP/Pop`,df2$Jobless.rate)
plot(df$Population,df$Jobless.rate)

plot(hclustWard, labels=df2[ ,1],hang=-1,main="Ward")
plot(hclustComplete,labels=df2[ ,1],hang=-1,main="Complete")
plot(hclustSingle,labels=df2[ ,1],hang=-1,main="Single")

Как видно метод Single не подходит в данном случае для кластеризации
Почему становится понятно из следующих графиков

df2.dist<-dist(df2[,2:6])
df2.mds<-cmdscale(df2.dist)
plot(df2.mds)

plot(df2$`GDP/Pop`,df2$Jobless.rate) и т.п.

Результаты Ward и Complete схожи, поэтому посмотрим на них детальней
#***************************
#Построим графики каменистая осыпь

qplot(1:(length(df2[ ,1])-1),hclustWard$height,geom=c("line","point"))
qplot(1:(length(df2[ ,1])-1),hclustComplete$height,geom=c("line","point"))


plot(1:(length(df2[ ,1])-1),hclustWard$height,type='o')
# Судя по графикуКоличество выделяемых кластеров, зависит от того, как сильно мы хотим углубиться
# в анализ. Мы можем выбрать как 3, так и 5, и даже 9
#Посмотрим как это выглядит
plot(hclustWard, labels=df2[ ,1])
rect.hclust(hclustWard, k=3, border="red")
rect.hclust(hclustWard, k=5, border="blue")
rect.hclust(hclustWard, k=9, border="green")
# Пока все варианты выглядят жизнеспособно


plot(1:(length(df2[ ,1])-1),hclustComplete$height,type='o')
# Исходя из этого графика, на мой вгляд целесообразно выделить 6 кластеров
plot(hclustComplete, labels=df2[ ,1],hang = -1)
rect.hclust(hclustComplete, k=6, border="red")
# Думаю, можно остановиться на этих кластерах, хотя стоит отметить, что если спуститься
# ниже по каменистой осыпи, то мы увидим, что страны также есть смысл разбить на 9 групп
# Сначала посмотрим результат разбиения на 6 кластеров, затем сравним 9 кластеров для разных способов
# расчета расстояния между кластерами
groupsC6 <- cutree(hclustComplete, k=6) 
groupsC6

#Предварительно расчитаем средние и медианы для всех показателей, чтобы было проще интерпретировать
#значения внутри кластеров
means<-apply(df1[,2:6],2,mean)
medians<-apply(df1[,2:6],2,median)
#По приведенным ниже графикам будем ориентироваться со средним или с медианой сравнивать показатели
q1<-qplot(df1$GDP,main="GDP",bins=50,xlab="")
q2<-qplot(df1$GDP.YoY,main="GDP.YOY",bins=50,xlab="")
q3<-qplot(df1$Inflation.rate,main="Inflation rate",bins=50,xlab="")
q4<-qplot(df1$Jobless.rate,main="Jobless rate",bins=50,xlab="")
q5<-qplot(df1$`GDP/Pop`,main="GDP per Population",bins=50,xlab="")

grid.arrange(q1,q2,q3,q4,q5,ncol=3)



#Кластер 1
colMeans(df1[groupsC6==1, 2:6])
barplot(colMeans(df2[groupsC6==1, 2:6]),main="Кластер 1")
df1[groupsC6==1,1]
#В данную группу входит Китай из-за своего огромного ВВП и невысоких прочих показателях 

#Кластер 2
colMeans(df1[groupsC6==2, 2:6])
barplot(colMeans(df2[groupsC6==2, 2:6]),main="Кластер 2")
df1[groupsC6==2,1]
#В данную группу попали страны с развитой экономикой, показатели которых лучше средних и медианных
#Показателей по региону
medians
#Кластер 3
colMeans(df1[groupsC6==3, 2:6])
barplot(colMeans(df2[groupsC6==3, 2:6]),main="Кластер 3")
df1[groupsC6==3,1]
medians
# Самый многочисленный кластер, в котором показатель ВВП на душу населения близок к медиане
# всех азиатских стран. Это же касается и остальных показателей,кроме ВВП, который не только 
# меньше среднего, но и в 2.7 раз меньше мелианного.
# Кластер 4
colMeans(df1[groupsC6==4, 2:6])
barplot(colMeans(df2[groupsC6==4, 2:6]),main="Кластер 4")
df1[groupsC6==4,1]
medians
# В этот кластер попала только Сирия за счет очень большой инфляции, причины которой объяснять
# не приходится. Стоит однако заметить, что остальные показатели далеко не так критичны
#Кластер 5
colMeans(df1[groupsC6==5, 2:6])
barplot(colMeans(df2[groupsC6==5, 2:6]),main="Кластер 5")
df1[groupsC6==5,1]
medians
#В эту группу попала также одна страна. Йенен. Эта страна отличилась, самым большим
#падением ВВП в регионе, опередив ближайшего преследователя Оман в два раза. Также в стране
# очень большой, по сравнению с остальным регионом уровень безработицы. Йемен и так являлся
# одной из самых бедных стран в регионе с одним из самых маленьких уровней ВВП на душу населения
# но из-за того, что бОльшую часть экономики этой страны представляет нефтедобывающий сектор
# то из-за падения цен на нефть и сокращающихся запасов нефти в стране, там наметился кризис

#Кластер 6
colMeans(df1[groupsC6==6, 2:6])
barplot(colMeans(df2[groupsC6==6, 2:6]),main="Кластер 6")
df1[groupsC6==6,1]
medians
#В эту группу вошли Афганистан и Палестина, отличающиеся очень большим уровнем безработицы, и низкими ВВП
# Однако стоит отметить, что прирост ВВП в этих странах больше, чем в среднем по региону,
# и больше медианного уровня

#Посмотрим, что получится, если разбить страны на 9 кластеров
groupsC9 <- cutree(hclustComplete, k=9) 
groupsW9 <- cutree(hclustWard, k=9) 


#Кластер 1
df1[groupsC9==1,1]
df1[groupsW9==1,1]
# И там, и там Китай
#Кластер 2
df1[groupsC9==2,1]
df1[groupsW9==2,1]
#Экономически развитые страны тоже совпадают
#Кластер 3
df1[groupsC9==3,1]
df1[groupsW9==3,1]
colMeans(df1[groupsC9==3, 2:6])
colMeans(df1[groupsW9==3, 2:6])
barplot(colMeans(df2[groupsC9==3, 2:6]),main="Кластер C3")
barplot(colMeans(df2[groupsW9==3, 2:6]),main="Кластер W3")
medians
colMeans(df1[groupsC6==3, 2:6])
#Два метода между собой дают не очень отличающиеся результаты
#При этом в новый третий кластер из старого выбраны страны с большим производством, если судить
#по ВВП и его приросту, но меньшим ВВП на душу населения
#Кластер 4
df1[groupsC9==4,1]
df1[groupsW9==5,1]
colMeans(df1[groupsC9==4, 2:6])
colMeans(df1[groupsW9==5, 2:6])
barplot(colMeans(df2[groupsC9==5, 2:6]),main="Кластер C4")
barplot(colMeans(df2[groupsW9==5, 2:6]),main="Кластер W4")
medians
colMeans(df1[groupsC6==3, 2:6])
#Кластеры совпали и образовали из старого 3 кластера новый 4, взяв оттуда страны
# невысоким уровнем производства
#Кластер 5
df1[groupsC9==5,1]
df1[groupsW9==6,1]
colMeans(df1[groupsC9==5, 2:6])
colMeans(df1[groupsW9==6, 2:6])
barplot(colMeans(df2[groupsC9==5, 2:6]),main="Кластер C5")
barplot(colMeans(df2[groupsW9==6, 2:6]),main="Кластер W5")
medians
df1[groupsC6==2,]
#Получили две страны с самым высоким ввп на душу населения, низкой инфляцией и уровнем
#безработицы
#Кластер 6
df1[groupsC9==6,1]
df1[groupsW9==7,1]
#Сирия
#Кластер 7
df1[groupsC9==7,1]
df1[groupsW9==4,1]
colMeans(df1[groupsC9==7, 2:6])
colMeans(df1[groupsW9==4, 2:6])
barplot(colMeans(df2[groupsC9==7, 2:6]),main="Кластер C7")
barplot(colMeans(df2[groupsW9==4, 2:6]),main="Кластер W4")
medians
colMeans(df1[groupsC6==3,2:6])
#Оман немного выбивается из этих стран низким уровнем ВВП и наоборот
#Высоким уровнем ВВП/на душу населения, скорее всего из-за запасов нефти
#Кластер 8
df1[groupsC9==8,1]
df1[groupsW9==8,1]
#Остался Йемен
df1[groupsC9==9,1]
df1[groupsW9==9,1]
colMeans(df1[groupsC9==9, 2:6])
colMeans(df1[groupsW9==9, 2:6])
barplot(colMeans(df2[groupsC9==9, 2:6]),main="Кластер C9")
barplot(colMeans(df2[groupsW9==9, 2:6]),main="Кластер W9")
medians
colMeans(df1[groupsC6==6,2:6])
#Остались прежние

for (i in 1:9){
  print(df1[groupsW9==i,1])
}
for (i in 1:9){
  print(df1[groupsC9==i,1])
}
# Таким образом, после иерархического кластерного анализа следующие рекомендации
# Если требуется не очень детальное разбиение на кластеры, то 
# Можно выделить 6 кластеров измеряя растояние между кластерами используя Complete
# Для более детального рассмотрения можно взять 9 кластеров и использовать метод Варда
# Построим две выбранные дендрограммы
# Посмотрим какие результаты даст knn
library(dendextend)
library(dplyr)
row.names(df2)<-df2[,1]
dendh6<-as.dendrogram(hclust(dist(df2[,2:6]), method="complete"))
dendh6%>%set("labels_colors",k=6)%>%set("branches_k_col",k=6)%>%set("branches_lwd",2)%>%plot(main="Дедрограмма для 6 кластеров")

dendh9<-as.dendrogram(hclust(dist(df2[,2:6]), method="ward.D"))
dendh9%>%set("labels_colors",k=9)%>%set("branches_k_col",k=9)%>%set("branches_lwd",2)%>%plot(main="Дедрограмма для 9 кластеров")


dendh99<-as.dendrogram(hclust(dist(df2[,2:6]), method="complete"))
dendh99%>%set("labels_colors",k=9)%>%set("branches_k_col",k=9)%>%set("branches_lwd",2)%>%plot(main="Дедрограмма для 9 кластеров")

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
# Судя по этому графику стоит попробовать выделить 5 и 7 кластеров

kmean5<-kmeans(df2[,2:6], 5, iter.max = 100,nstart = 50)
kmean5$centers
#Посмотрим на кластеры
for (i in 1:5){print(kmean5$cluster[kmean5$cluster==i])}
# Результаты схожи с теми, что были получен при иерархической кластеризации 
# Разница в том, что в иерархической кластеризации в разные кластеры выделены Йемен и Афганистан
# с Палестиной. В kmeans Афганистан и Йемен находятся в одном кластере, а Палестина в группе чуть более развитых стран 
# Посмотрим на эти страны внимательнее
df4<-df1[,2:6]
row.names(df4)<-df[,1]
df4[c("Afghanistan","Palestine","Yemen"),]
#Из таблицы однозначно нельзя сказать, какая кластеризация сработала лучше, но все же 
#резкое сокращение ВВП видится более важным фактором
kmean7<-kmeans(df2[,2:6], 7, iter.max = 100,nstart = 50)
kmean7$centers
for (i in 1:7){print(kmean7$cluster[kmean7$cluster==i])}
