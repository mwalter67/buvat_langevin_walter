library(shiny)
library(ggplot2)
library(e1071)
library(data.table)
library(pROC)
library(rmarkdown)


shinyUI(fluidPage(title="Projet SVM",
                  tags$head(includeCSS("C:/Users/mikew/OneDrive/Documents/MASTER 2 ESA/S1/SVM/Projet SVM/TEST_BIS/www/app.css")),
                  tags$img(style="position: fixed; bottom: 0; left: 0; border: 0;width: 150px; height: 150px",
                           src="logoESA.png"
                           ),

    div(id = "header",
        div(id = "title",
            "Implémentation d'un SVM dans le cadre d'un risque à la fraude"
        ),
        div(id = "subtitle",
            "Démonstrateur crée par",
            tags$a(href = "mailto:mailto:antoinebuvat74@gmail.com", "BUVAT Antoine"),",",
            tags$a(href = "mailto:mailto:langevin.julien@gmail.com", "LANGEVIN Julien"), "et ",
            tags$a(href = "mailto:mailto:mikewalter@hotmail.fr", "WALTER Mickaël")
        )
    ),
    
   
        navlistPanel(
            tabPanel(
                title="Comment utiliser ce démonstrateur",
                mainPanel(
                    
                    textOutput("text1")
                )
            ),
            tabPanel(
                title="Qu'est ce qu'un SVM?",
                
                mainPanel(
                    plotOutput("Plot")
                )
            ),
            
            tabPanel(
                title="Présentation de nos données",
                mainPanel(
                    includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/pres_données.Rmd")
                )
            ),
            tabPanel(
                title="Intéraction intéractive avec le SVM",
                sidebarLayout(
                    sidebarPanel(
                        selectInput("kernel",
                                    "Choix du noyau",
                                    choices=c("linear","polynomial","radial","sigmoid"),
                                    multiple=FALSE,
                                    selected="linear"
                        ),
                        conditionalPanel("input.kernel =='polynomial'",
                                         sliderInput("deg",
                                                     "Choix du degré du polynÃ´me",
                                                     min=3,
                                                     max=10,
                                                     value=3,
                                                     step=1
                                                     
                                         )
                                         
                        )
                        
                    ,
                        selectInput("cout",
                                    "Coût de pénalisation",
                                    choices=c(1,3,5,10),
                                    multiple=FALSE,
                                    selected=1
                        )
                    ),
                        
                        mainPanel(
                            verbatimTextOutput("table1"),
                            plotOutput("confusion"),
                            plotOutput("confusion2"),
                            plotOutput("roc")
                            
                           
                            
                        )
                    )
                    
                    
                
            )
        )

))
