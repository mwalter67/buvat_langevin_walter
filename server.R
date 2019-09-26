library(shiny)
library(ggplot2)
library(e1071)
library(data.table)
library(pROC)
library(rmarkdown)
library(caret)

resample=fread("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
test=fread("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',')
#resample=fread("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
#test=fread("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',')
attach(resample)
names(resample)

choix_svm=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~. , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  pred_essai=predict(svm_resample, resample)
  print(svm_resample) 
  
}

confusion=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~. , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  
  pred_essai=predict(svm_resample, resample)
  Class.f=as.factor(Class)
  conf=confusionMatrix(data=pred_essai,reference=Class.f)
  plot.confusion(conf,"Matrice de confusion sur son échantillon d'apprentissage")
}

confusion2=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~. , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  
  pred_essai=predict(svm_resample, newdata=test)
  Class.f=as.factor(test$Class)
  conf=confusionMatrix(data=pred_essai,reference=Class.f)
  plot.confusion(conf,"Matrice de confusion sur son échantillon de test")
}





plot.confusion=function(cm,titre) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(titre, cex.main=2)
  
  # create the matrix 
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
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Statistiques importantes", xaxt='n', yaxt='n')
  text(25, 60, "Sensitivité", cex=1.5, font=2)
  text(25, 40, round(as.numeric(cm$byClass[1]), 3), cex=1.4)
  text(50, 60, "Spécificité", cex=1.5, font=2)
  text(50, 40, round(as.numeric(cm$byClass[2]), 3), cex=1.4)
  text(75, 60, "Taux d'erreur", cex=1.5, font=2)
  text(75, 40, 1-round(as.numeric(cm$overall[1]), 3), cex=1.4)
  
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  
  output$text1 <- renderText("Essai du texte 1")
  
  output$Plot <- renderPlot({
    
    
    
    
    # Construct sample data set - completely separated
    set.seed(10)
    x <- matrix(rnorm(20*2), ncol = 2)
    y <- c(rep(-1,10), rep(1,10))
    x[y==1,] <- x[y==1,] + 3/2
    dat <- data.frame(x=x, y=as.factor(y))
    
    # Plot data
    ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
      geom_point(size = 2) +
      scale_color_manual(values=c("#000000", "#FF0000")) +
      theme(legend.position = "none") +
      ggtitle("Exemple d'un SVM avec un échantillon séparable")+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x="X")+
      labs(y="Y")
    
    
  })
  
  
  output$table1 <- renderPrint({
    choix_svm(input$kernel,input$cout, input$deg)
  })
  
  output$confusion <- renderPlot({
    confusion(input$kernel,input$cout, input$deg)
  })
  
  output$confusion2 <- renderPlot({
    confusion2(input$kernel,input$cout, input$deg)
  })
  
  output$roc <- renderPlot({
    set.seed(5)
    svm_resample=svm(Class~. , data=resample, kernel=input$kernel, type="C-classification", cost=input$cout, degree=input$deg )
    pred2=predict(svm_resample,data=resample,newdata = test, probability = FALSE)
    summary(pred2)
    tab2=table(pred2,test$Class)
    err2=mean(test$Class!=pred2)*100
    prednum2=as.numeric(pred2)
    roc2=roc(test$Class,prednum2)
    plot.roc(roc2, print.auc=T, col="blue", main= "Courbe ROC sur l'échantillon de test")
    
  })
  
  
  
  
})