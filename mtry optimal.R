resample=read.table("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',',header=TRUE)
test=read.table("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',',header=TRUE)
attach(resample)
Class2=ifelse(Class==0,"0:No fraud","1:Fraud")
resample2=data.frame(resample,Class2)
resample2=resample2[,-31]
Class2=ifelse(test$Class==0,"0:No fraud", "1:Fraud")
test2=data.frame(test,Class2)
test2=test2[,-31]
library(randomForest)

rssm=vector()
for (j in c(1000,2000,3000,4000,5000)){
  for (i in 1:18){
    set.seed(2501)
    rf.rus=randomForest(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2,mtry=1,ntree=j)
    rf.rus.pred=predict(rf.rus,newdata=test2,type="class")
    rssmt=mean(Class2==rf.rus.pred)
    rssm=rbind(rssm,rssmt)
  }
}
rssm

#Le nombre de prédicteur optimal pour la fôret aléatoire est de 1.
#nombre optimal compris entre 2000 et 4000

rssm=vector()
for (j in c(2000,2500,3000,3500,4000)){
    set.seed(2501)
    rf.rus=randomForest(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2,mtry=1,ntree=j)
    rf.rus.pred=predict(rf.rus,newdata=test2,type="class")
    rssmt=mean(Class2==rf.rus.pred)
    rssm=rbind(rssm,rssmt)
}
rssm
#Nombre d'arbre optimal compris entre 2500 et 3000

rssm=vector()
for (j in c(2500,2600,2700,2800,2900,3000)){
    set.seed(2501)
    rf.rus=randomForest(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2,mtry=1,ntree=j)
    rf.rus.pred=predict(rf.rus,newdata=test2,type="class")
    rssmt=mean(Class2==rf.rus.pred)
    rssm=rbind(rssm,rssmt)
}
rssm
#Nombre d'arbre optimal compris entre 2500 et 2700

rssm=vector()
for (j in c(2500,2550,2600,2650,2700)){
    set.seed(2501)
    rf.rus=randomForest(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2,mtry=1,ntree=j)
    rf.rus.pred=predict(rf.rus,newdata=test2,type="class")
    rssmt=mean(Class2==rf.rus.pred)
    rssm=rbind(rssm,rssmt)
}
rssm
#Nombre d'arbre optimal compris entre 2550 et 2650

rssm=vector()
for (j in c(2550,2560,2570,2580,2590,2600,2610,2620,2630,2640,2650)){
    set.seed(2501)
    rf.rus=randomForest(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2,mtry=1,ntree=j)
    rf.rus.pred=predict(rf.rus,newdata=test2,type="class")
    rssmt=mean(Class2==rf.rus.pred)
    rssm=rbind(rssm,rssmt)
}
rssm
#Nombre d'arbre optimal compris entre 2550 et 2559

rssm=vector()
for (j in c(2550,2551,2552,2553,2554,2555,2556,2557,2558,2559)){
    set.seed(2501)
    rf.rus=randomForest(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2,mtry=1,ntree=j)
    rf.rus.pred=predict(rf.rus,newdata=test2,type="class")
    rssmt=mean(Class2==rf.rus.pred)
    rssm=rbind(rssm,rssmt)
}
rssm
#Le nombre d'arbre optimal est donc de 2551
#Dans l'application, pour la forêt, on utilisera mtry=1 et ntree=2551



