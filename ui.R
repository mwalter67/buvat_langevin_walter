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


shinyUI(fluidPage(title="Projet SVM",
                  tags$style('body{font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                             font-size: 14px;
                             line-height: 1.42857143;
                             color: #333;
                             background-color: #fff;
                             text-align: justify;}'),
                  
                  
                  tags$style('img{vertical-align: middle;
                             display: block;
                             margin-left: auto;
                             margin-right: auto;
                             width: 80%;
                             height: auto;}'),
                  
                  tags$head(includeCSS("www/app.css")),
                  #tags$head(includeCSS("C:/Users/mikew/OneDrive/Bureau/buvat_langevin_walter/www/app.css")),
                  #tags$head(includeCSS("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/www/app.css")),
                  #tags$head(includeCSS("C:/Users/Julien/Documents/GitHub/buvat_langevin_walter/www/app.css")),
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
                  
                  
                  navlistPanel(
                    tabPanel(
                      title="Comment utiliser ce démonstrateur",
                      mainPanel(
                        includeMarkdown("texte/intro.Rmd"),
                        fluidRow(
                          column(12,align="center",
                                 downloadButton("notice", "Téléchargement de la notice"))),
                        width=12
                      )
                    ),
                    tabPanel(
                      title="Qu'est ce qu'un SVM?",
                      
                      mainPanel(
                        includeMarkdown("texte/svm_p1.Rmd"),
                        #includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/svm_p1.Rmd"),
                        fluidRow(
                          column(12,align="center",
                                 rglwidgetOutput("troisd"))),
                        includeMarkdown("texte/svm_p2.Rmd"),
                        #includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/svm_p2.Rmd"),
                        #includeMarkdown(("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/SVM.Rmd")),
                        #includeMarkdown(rmarkdown::render("C:/Users/Julien/Documents/GitHub/buvat_langevin_walter/texte/SVM.Rmd")),
                        
                        width = 12
                      )
                    ),
                    
                    tabPanel(
                      title="Présentation de nos données",
                      mainPanel(
                        includeMarkdown("texte/pres_données.Rmd"),
                        #includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/pres_données.Rmd"),
                        #includeMarkdown("C:/Users/Julien/Documents/GitHub/buvat_langevin_walter/texte/pres_données.Rmd"),
                        #includeMarkdown("C:/Users/util/Documents/GitHub/buvat_langevin_walter/texte/pres_données.Rmd"),
                        width = 12
                      )
                    ),
                    tabPanel(
                      title="Modifier le SVM à votre guise",
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
                          ),
                          width=3
                        ),
                        
                        mainPanel(
                          verbatimTextOutput("table1"),
                          plotOutput("confusion"),
                          plotOutput("confusion2"),
                          plotOutput("roc"),
                          width=9
                        )
                      )
                      
                      
                      
                    ),
                    
                    tabPanel(
                      title="Comparaison du SVM avec d'autres méthodes de machine learning",
                      
                      
                      tabsetPanel(
                        tabPanel(
                          "Recherche du meilleur SVM",
                          mainPanel(
                            includeMarkdown("texte/Best_SVM.Rmd"),
                            #includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/Best_SVM.Rmd"),
                            #includeMarkdown("C:/Users/Julien/Documents/GitHub/buvat_langevin_walter/texte/Best_SVM.Rmd"),
                            #includeMarkdown("C:/Users/util/Documents/GitHub/buvat_langevin_walter/texte/Best_SVM.Rmd"),
                            fluidRow(
                              column(8,align="center",
                                     DT::dataTableOutput("bestsvm", width = 300)
                              )),
                            includeMarkdown("texte/Best_SVM2.Rmd"),
                            #includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/Best_SVM2.Rmd"),
                            width=12
                          )
                        ),
                        tabPanel(
                          "Régression logistique",
                          mainPanel(
                            
                            plotOutput("meilleursvm"),
                            plotOutput("concurrent"),
                            plotOutput("roccomp"),
                            width=12
                            
                          )
                        ),
                        tabPanel(
                          "Arbre de classification",
                          mainPanel(
                            
                            plotOutput("meilleursvm2"),
                            plotOutput("concurrent2"),
                            plotOutput("roccomp2"),
                            width = 12
                            
                            
                          )
                        ),
                        tabPanel(
                          "Méthode KNN",
                          mainPanel(
                            
                            selectInput("k",
                                        "Choix du nombre de voisins",
                                        choices=c(1,3,5,10,20),
                                        multiple=FALSE,
                                        selected=1
                            ),
                            plotOutput("meilleursvm3"),
                            plotOutput("concurrent3"),
                            plotOutput("roccomp3"),
                            width = 12
                            
                            
                          )
                        )
                        
                        
                      )
                    ),
                    tabPanel(
                      title="Remerciements",
                      mainPanel(
                        includeMarkdown("texte/remerciement.Rmd"),
                        #includeMarkdown("C:/Users/mikew/OneDrive/Documents/GitHub/buvat_langevin_walter/texte/remerciement.Rmd"),
                        width=12
                      )
                    )
                    
                  )
)

)
