## Importation des données

data=read.csv("C:/Users/mikew/OneDrive/Bureau/MASTER 2 ESA/S1/SVM/Projet SVM/creditcard.csv", header=T )
head(data)
attach(data)
names(data)

##Création des échantillons d'apprentissage et de test

seed=1
train=sample(1:nrow(data),200000)
data_train=data[train,]
data_test=data[-train,]

table(data$Class)

fraud=data$Class==1



#Il y a 492 transactions frauduleuses dans notre base de données.

## Test SVM

#Installation et chargement du package nécessaire

install.packages("e1071")
library(e1071)

#Essai sur un petit échantillon

essai=sample(1:nrow(data),10000)
data_essai=data[essai,]
class_essai=Class[essai]

svm_essai=svm(Class~. , data=data_essai, kernel="linear")
svm_essai

pred_essai=predict(svm_essai, data_essai)

predessai=rep(0,10000)
predessai[pred_essai<0.5]=0
predessai[pred_essai>=0.5]=1

table(predessai,class_essai)

err=mean(data_essai$Class!=predessai)
err

#Test sur l'échantillon train

#Kernel linéaire
## Pas encore testé

svm_train=svm(Class~. , data=data_train, kernel="linear")
