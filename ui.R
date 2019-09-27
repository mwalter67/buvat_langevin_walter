library(shiny)
library(ggplot2)
library(e1071)
library(data.table)
library(pROC)
library(rmarkdown)
library(caret)
library(tree)


shinyUI(fluidPage(title="Projet SVM",
                  tags$style('body{font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                             font-size: 14px;
                             line-height: 1.42857143;
                             color: #333;
                             background-color: #fff;
                             text-align: justify;}'),
                  tags$style('.col-sm-8{width: 100%;
                             padding-right: 200px;
                             padding-left: 200px;}'),
                  tags$head(includeCSS("C:/Users/mikew/OneDrive/Documents/MASTER 2 ESA/S1/SVM/Projet SVM/TEST_BIS/www/app.css")),
                  #tags$head(includeCSS("C:/Users/util/Documents/GitHub/buvat_langevin_walter/www/app.css")),
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
                  
                  
                  tabsetPanel(
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
                        #includeMarkdown("C:/Users/util/Documents/GitHub/buvat_langevin_walter/texte/pres_données.Rmd")
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
                                           radioButtons("deg",
                                                        "Choix du degré du polynôme",
                                                        choices = c(3,4,5),
                                                        selected = 3,
                                                        inline = F
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
                      
                      
                      
                    ),
                    
                    tabPanel(
                      title="Comparaison du SVM avec deux autres méthodes de machines learning",
                      
                        
                      navlistPanel(
                        tabPanel(
                          "Recherche du meilleur SVM",
                          mainPanel(
                            includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/Best_SVM.Rmd"),
                            tableOutput("BestSVM")
                          )
                        ),
                        tabPanel(
                          "Regression logistique",
                          mainPanel(
                            
                            plotOutput("meilleursvm"),
                            plotOutput("concurrent"),
                            plotOutput("roccomp")
                            
                          )
                        ),
                        tabPanel(
                          "Arbre de regression",
                          mainPanel(
                            
                              plotOutput("meilleursvm2"),
                              plotOutput("concurrent2"),
                              plotOutput("roccomp2")
                            
                            
                          )
                        )
                        
                      )
                    ) 
                    
                    )
                  )
                  
)
