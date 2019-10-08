library(shiny)
library(ggplot2)
library(e1071)
library(data.table)
library(pROC)
library(rmarkdown)
library(caret)
library(tree)
library(DT)
library(corrplot)
library(rgl)
library(class)

## Importation des données

resample=fread("creditcard_rus.csv",sep=',')
test=fread("creditcard_test.csv",sep=',')
#resample=fread("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
#test=fread("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',')
#resample=fread("C:/Users/Julien/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
#test=fread("C:/Users/Julien/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',')
#resample=fread("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
#test=fread("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',')
attach(resample)
names(resample)

### Fonction du SVM

choix_svm=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21 , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  pred_essai=predict(svm_resample, resample)
  print(svm_resample) 
  
}

### Fonction matrice de confusion échantillon d'apprentissage

confusion=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21 , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  
  pred_essai=predict(svm_resample, resample)
  Class.f=as.factor(Class)
  conf=confusionMatrix(data=pred_essai,reference=Class.f)
  plot.confusion(conf,"Matrice de confusion sur son échantillon d'apprentissage")
}

### Fonction matrice de confusion échantillon test

confusion2=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21 , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  
  pred_essai=predict(svm_resample, newdata=test)
  Class.f=as.factor(test$Class)
  conf=confusionMatrix(data=pred_essai,reference=Class.f)
  plot.confusion(conf,"Matrice de confusion du SVM (test)")
}

### Création fonction de la matrice de confusion

plot.confusion=function(cm,titre) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(titre, cex.main=2)
  
  # Création matrice
  
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Pas de Fraude', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Fraude', cex=1.2)
  text(125, 370, 'Prédite', cex=1.3, srt=90, font=2)
  text(245, 450, 'Observée', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Pas de fraude', cex=1.2, srt=90)
  text(140, 335, 'Fraude', cex=1.2, srt=90)
  
  # Ajout des résultats 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # Ajout de sensitivité spécificté et taux d'erreur
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Statistiques importantes", xaxt='n', yaxt='n')
  text(25, 60, "Sensitivité", cex=1.5, font=2)
  text(25, 40, round(as.numeric(cm$byClass[1]), 3), cex=1.4)
  text(50, 60, "Spécificité", cex=1.5, font=2)
  text(50, 40, round(as.numeric(cm$byClass[2]), 3), cex=1.4)
  text(75, 60, "Taux d'erreur", cex=1.5, font=2)
  text(75, 40, 1-round(as.numeric(cm$overall[1]), 3), cex=1.4)
  
}


## Début du Server Shiny

shinyServer(function(input, output) {
  
  
  #### Output du SVM 
  
  
  output$table1 <- renderPrint({
    choix_svm(input$kernel,input$cout, input$deg)
  })
  
  #### Output matrice de confusion échantillon validation et test
  
  output$confusion <- renderPlot({
    confusion(input$kernel,input$cout, input$deg)
  })
  
  output$confusion2 <- renderPlot({
    confusion2(input$kernel,input$cout, input$deg)
  })
  
  ### Output Courbe ROC
  
  output$roc <- renderPlot({
    set.seed(5)
    svm_resample=svm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21 , data=resample, kernel=input$kernel, type="C-classification", cost=input$cout, degree=input$deg )
    pred2=predict(svm_resample,data=resample,newdata = test, probability = FALSE)
    summary(pred2)
    tab2=table(pred2,test$Class)
    err2=mean(test$Class!=pred2)*100
    prednum2=as.numeric(pred2)
    roc2=roc(test$Class,prednum2)
    plot.roc(roc2, print.auc=T, col="blue", main= "Courbe ROC du SVM")
    
  })
  
  ### Output tableau résultats des différents SVM
  
  output$bestsvm <- DT::renderDataTable({
    bestsvm=fread("bestsvm.csv")
    #bestsvm=fread("C:/Users/mikew/OneDrive/Documents/MASTER 2 ESA/S1/SVM/Projet SVM/bestsvm.csv")
    #bestsvm=fread("C:/Users/Julien/Documents/GitHub/buvat_langevin_walter/bestsvm.csv")
    #bestsvm=fread("C:/Users/util/Documents/GitHub/buvat_langevin_walter/bestsvm.csv")
    bestsvm},
    options=list(pageLength=12,
                 dom='tp')
  )
  
  #### Output des matrices de confusions du SVM pour les comparaisons
  
  output$meilleursvm <-renderPlot({
    confusion2(input$kernel,input$cout, input$deg)
  })
  
  output$meilleursvm2 <-renderPlot({
    confusion2(input$kernel,input$cout, input$deg)
  })
  
  output$meilleursvm3 <-renderPlot({
    confusion2(input$kernel,input$cout, input$deg)
  })
  
  #### Création des graphiques pour comparaison SVM et regression logistique
  
  output$concurrent<-renderPlot({
    glm_rus=glm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,family="binomial",data=resample)
    glm_pred_prob=predict(glm_rus,newdata=test,type="response")
    
    glm_pred=rep(0,nrow(test))
    glm_pred[glm_pred_prob<=.5]=0
    glm_pred[glm_pred_prob>.5]=1
    
    Class.glm=as.factor(test$Class)
    glm_pred=as.factor(glm_pred)
    
    conf2=confusionMatrix(data=glm_pred,reference=Class.glm)
    plot.confusion(conf2,"Matrice de confusion de la régression logistique")
    
  }) 
  
  
  output$roccomp <- renderPlot({
    
    
    set.seed(5)
    svm_resample=svm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21 , data=resample, kernel=input$kernel, type="C-classification", cost=input$cout, degree=input$deg )
    pred_essai=predict(svm_resample, newdata=test)
    
    glm_rus=glm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,family="binomial",data=resample)
    glm_pred_prob=predict(glm_rus,newdata=test,type="response")
    glm_pred=rep(0,nrow(test))
    glm_pred[glm_pred_prob<=.5]=0
    glm_pred[glm_pred_prob>.5]=1
    
    plot.roc(test$Class,as.numeric(pred_essai),main="Comparaison des courbes ROC", percent=TRUE, col="#1c61b6", print.auc=T,  print.auc.y=40)
    plot.roc(test$Class,glm_pred, percent=TRUE, col="#008600",add=T, print.auc=T)
    legend("bottomright", legend=c("SVM", "Régression logistique"), col=c("#1c61b6", "#008600"), lwd=2)  
    
    
    
    
  })
  
  #### Création des graphiques pour comparaison SVM et arbre de classification
  
  output$concurrent2 <-renderPlot({
    Class2=ifelse(Class==0,"0:No fraud","1:Fraud")
    resample2=data.frame(resample,Class2)
    resample2=resample2[,-31]
    
    Class2=ifelse(test$Class==0,"0:No fraud", "1:Fraud")
    test2=data.frame(test,Class2)
    test2=test2[,-31]
    
    
    tree.rus=tree(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2)
    
    set.seed(2501)
    
    tree.pred=predict(tree.rus,test2,type="class")
    table(tree.pred,Class2)
    
    Class4=test2$Class2
    conf.tree=confusionMatrix(data=tree.pred,reference=Class4)
    plot.confusion(conf.tree,"Matrice de confusion de l'arbre de classification")
  })
  
  output$roccomp2 <-renderPlot({
    set.seed(5)
    svm_resample=svm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21 , data=resample, kernel=input$kernel, type="C-classification", cost=input$cout, degree=input$deg )
    pred_essai=predict(svm_resample, newdata=test)
    
    Class2=ifelse(Class==0,"0:No fraud","1:Fraud")
    resample2=data.frame(resample,Class2)
    resample2=resample2[,-31]
    Class2=ifelse(test$Class==0,"0:No fraud", "1:Fraud")
    test2=data.frame(test,Class2)
    test2=test2[,-31]
    tree.rus=tree(Class2~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21,resample2)
    set.seed(2501)
    tree.pred=predict(tree.rus,test2,type="class")
    
    plot.roc(test$Class,as.numeric(pred_essai),main="Comparaison des courbes ROC", percent=TRUE, col="#1c61b6", print.auc=T,  print.auc.y=40)
    plot.roc(test2$Class2,as.numeric(tree.pred), percent=TRUE, col="#008600",add=T, print.auc=T)
    legend("bottomright", legend=c("SVM", "Arbre de classification"), col=c("#1c61b6", "#008600"), lwd=2)
  })
  
  #### Création des graphiques pour comparaison SVM et KNN
  
  output$concurrent3 <-renderPlot({
    x.resample=resample[,-31]
    y.resample=resample$Class
    
    x.test=test[,-31]
    y.test=test$Class
    
    k=input$k
    k=as.numeric(k)
    
    knn.pred=knn(train = x.resample, 
                 test  = x.test, 
                 cl    = y.resample, 
                 k     = k)
    
    
    
    
    Class.knn=as.factor(test$Class)
    knn.pred=as.factor(knn.pred)
    
    conf3=confusionMatrix(data=knn.pred,reference=Class.knn)
    plot.confusion(conf3,paste("Matrice de confusion de la méthode KNN avec",k, "voisins"))
  })
  
  output$roccomp3 <- renderPlot({
    
    
    set.seed(5)
    svm_resample=svm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21 , data=resample, kernel=input$kernel, type="C-classification", cost=input$cout, degree=input$deg )
    pred_essai=predict(svm_resample, newdata=test)
    
    x.resample=resample[,-31]
    y.resample=resample$Class
    
    x.test=test[,-31]
    y.test=test$Class
    
    k=input$k
    k=as.numeric(k)
    
    knn.pred=knn(train = x.resample, 
                 test  = x.test, 
                 cl    = y.resample, 
                 k     = k)
    
    plot.roc(test$Class,as.numeric(pred_essai),main="Comparaison des courbes ROC", percent=TRUE, col="#1c61b6", print.auc=T,  print.auc.y=40)
    plot.roc(test$Class,as.numeric(knn.pred), percent=TRUE, col="#008600",add=T, print.auc=T)
    legend("bottomright", legend=c("SVM", "KNN"), col=c("#1c61b6", "#008600"), lwd=2)  
    
    
    
    
  })
  
  #### Création du bouton de téléchargement de la notice  
  
  output$notice <- downloadHandler(
    filename = "notice.pdf",
    content = function(file) {
      tempReport <- file.path("texte/notice.Rmd")
      file.copy("notice.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #### Création du graphique 3D de l'onglet 2 
  
  output$troisd <-renderRglwidget({
    train <- iris
    train$y <-ifelse(train[,5]=="setosa", 1, -1)
    sv <- svm(y~Petal.Length+Petal.Width+Sepal.Length, data=train, kernel="linear", scale=FALSE, type="C-classification")
    W <- rowSums(sapply(1:length(sv$coefs), function(i) sv$coefs[i]*sv$SV[i,]))
    plot3d(train$Petal.Length, train$Petal.Width, train$Sepal.Length, ylab="", xlab="", zlab="", col= ifelse(train$y==-1,"red","blue"), size = 2, type='s', alpha = .6, main="Un exemple dans un espace a 3 dimensions")
    rgl.bg(color = "white")
    rgl.planes(a = W[1], b=W[2], c=W[3], d=-sv$rho, color="green", alpha=.4)
    rglwidget()
    
  })
  
  
  
  
  
  
})


