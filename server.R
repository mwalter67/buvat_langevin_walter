library(shiny)
library(ggplot2)
library(e1071)
library(data.table)
library(pROC)
library(rmarkdown)

#resample=fread("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
#test=fread("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',')
resample=fread("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
test=fread("C:/Users/util/Documents/GitHub/buvat_langevin_walter/creditcard_test.csv",sep=',')

attach(resample)
names(resample)

choix_svm=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~. , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  pred_essai=predict(svm_resample, resample)
  tab=table(pred_essai,Class)
  err=mean(resample$Class!=pred_essai)*100
  print(svm_resample)   
}

confusion=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~. , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  pred_essai=predict(svm_resample, resample)
  tab=table(pred_essai,Class)
  err=mean(resample$Class!=pred_essai)*100
  tab=data.frame(tab)
  confusion=ggplot(data =  tab, mapping = aes(x = pred_essai, y = Class)) + 
    geom_tile(aes(fill = Freq), colour = "black") +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low = "white", high = "white") +
    theme_bw() + theme(legend.position = "none") +
    ggtitle("Matrice de confusion de l'échantillon d'apprentissage") +
    xlab("Classe prédite") + ylab("Classe observée") +
    theme(plot.title = element_text(hjust = 0.5))
  print(confusion)
}

confusion2=function(kernel, cout, deg){
  set.seed(5)
  svm_resample=svm(Class~. , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
  pred2=predict(svm_resample,data=resample,newdata = test, probability = FALSE)
  summary(pred2)
  
  tab2=table(pred2,test$Class)
  tab2=as.data.frame(tab2)
  err2=mean(test$Class!=pred2)*100
  err2
  
  confusion=ggplot(data =  tab2, mapping = aes(x = pred2, y = Var2)) + 
    geom_tile(aes(fill = Freq), colour = "black") +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low = "white", high = "white") +
    theme_bw() + theme(legend.position = "none") +
    ggtitle("Matrice de confusion de l'échantillon test") +
    xlab("Classe prédite") + ylab("Classe observée") +
    theme(plot.title = element_text(hjust = 0.5))
  print(confusion)
  
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
      ggtitle("Exemple d'un SVM avec un Ã©chantillon sÃ©parable")+
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
