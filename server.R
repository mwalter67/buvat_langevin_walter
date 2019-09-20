library(data.table)
library(shiny)
library(ggplot2)
library(e1071)

resample=fread("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/creditcard_rus.csv",sep=',')
attach(resample)
names(resample)

choix_svm=function(kernel, cout, deg){
    set.seed(5)
    svm_resample=svm(Class~. , data=resample, kernel=kernel, type="C-classification", cost=cout, degree=deg )
    pred_essai=predict(svm_resample, resample)
    tab=table(pred_essai,Class)
    err=mean(resample$Class!=pred_essai)*100
    print(svm_resample)   
    print(tab)
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
    
    output$text2 <- renderText("Essai du texte 2")
    
    output$table1 <- renderPrint({
        choix_svm(input$kernel,input$cout, input$deg)
        })
    
    

    

})
