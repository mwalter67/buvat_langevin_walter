library(data.table)
library(corrplot)
library(caret)
library(e1071)
library(ggplot2)

#Importation de la table de données

data=fread("C:/Users/mikew/OneDrive/Documents/MASTER 2 ESA/S1/SVM/Projet SVM/creditcard.csv",sep=',')
head(data)
summary(data)
attach(data)
names(data)

#Création de la table pour le RUS

#Partition 70% pour l'échantillon d'apprentissage et 30% pour l'échantillon Test

set.seed(9876)
inTrain<-createDataPartition(data$Class, p = 0.7,list=FALSE)
train <-data[inTrain]  #377 Total Occurrences of Fraud
table(train$Class)
test  <-data[-inTrain] 


nonfraud<-which(train$Class==0)
fraud<-which(train$Class==1)

train.sub<-list()

for(i in 1:10){
  train.sub[[i]]<-train[c(fraud,sample(nonfraud,length(fraud)*(2.33)))]
}

rus_data=train.sub[[1]]

table(rus_data$Class)

write.csv(rus_data,"C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",row.names = FALSE)


