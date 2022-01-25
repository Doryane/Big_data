#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shinydashboard)
library(shiny)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

library(randomForest)
library(pROC)
library(ROCR)
library(Matrix)
library(glmnet)
library(Rcpp)
library(xgboost)
library(ggplot2)
library(pROC)
library(shinybusy)

library(randomForest)
library(pROC)
library(ROCR)
library(Matrix)
library(glmnet)
library(Rcpp)
library(xgboost)
library(ranger)
library(gbm)
library(shinyalert)
library(markdown)

set.seed(987654) 

library("readxl")
data=read.csv("kaggle.csv",header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE)

data= data[,-1]

# using colMeans()
mean_val <- colMeans(data,na.rm = TRUE)

# replacing NA with mean value of each column
for(i in colnames(data))
  data[,i][is.na(data[,i])] <- mean_val[i]

df3 <- data %>% 
  rename(
    SeriousDlqin2yrs_V2 = V2,
    RevolvingUtilizationOfUnsecuredLines_V3 = V3,
    Age_V4 = V4,
    NumberOfTime30_59DaysPastDueNotWorse_V5 = V5,
    DebtRatio_V6 = V6,
    MonthlyIncome_V7 = V7,
    NumberOfOpenCreditLinesAndLoans_V8 = V8,
    NumberOfTimes90DaysLate_V9 = V9,
    NumberRealEstateLoansOrLines_V10 = V10,
    NumberOfTimes60_89DaysPastDueNotWorse_V11 = V11,
    NumberOfDependents_V12 = V12)
attach(df3)



ui <- shinyUI(
  dashboardPage(
    
    dashboardHeader(title = span("Big Data",style = "color: White; font-size: 28px")
    ),
    
    # Forme de l'application
    dashboardSidebar(
      sidebarMenu(id="sidebar",
                  menuItem("Accueil", tabName = "home", icon = icon("th")),
                  menuItem("Introduction", tabName = "Introduction", icon = icon("bookmark")),
                  menuItem("Méthodologie", tabName = "Methodologie", icon = icon("book")),
                  menuItem("Traitement initial", tabName = "Ttt", icon = icon("cog")),
                  menuItem("Données", tabName = "Donnees", icon = icon("th")),
                  menuItem("Résultats", tabName = "Methodes", icon = icon("folder")),
                  menuItem("Comparer les méthodes", tabName = "Comparaison", icon = icon("tasks")),
                  menuItem("Contact", tabName = "propos", icon = icon("globe"))
      )
    ),
    
    dashboardBody(
      # Pour changer les couleurs
      #partie couleur
      
      tags$head(tags$style(HTML('                 
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #0ebfcb
;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #04357f;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #04357f;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #04357f;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #0468bb;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #04357f;
                                color: #FFFFFF;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #0468bb;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #0468bb;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                                
                                '))),
      
      tabItems(
        
        #Tab Content 0
        tabItem(tabName="home",
                add_busy_gif(
                  src = "https://64.media.tumblr.com/ec627f87bb0b6a55d920415a3798a3f0/tumblr_pkb6nr1CsS1w5tjdn_500.gifv",
                  position = "full-page",
                  height = "1000px",
                  width = "1000spx"
                ),
                tags$img(src="big_data2.jpg",style = 'top: 0px;left:0px;display:block;margin-left: auto;margin-right: auto;height:100%;width: 100%;position: absolute'),#height=675,width=1175),
                tags$button(id="lol","Bienvenue",style = 'position: absolute; top:65%; left:50%;height:50px;width:200px;font-size:25px;',icon("forward"),
                            class="btn action-button btn-large btn-primary",type="button", HTML('<i class="icon-star"></i>'))
                ), 
        
          
        # First tab content
        tabItem(tabName = "Introduction",
                includeMarkdown("intro.Rmd"),
                tags$button(id="lol2","Suivant",style = 'position: absolute; top:80%; left:50%;height:60px;width:200px;font-size:25px;',icon("forward"),
                            class="btn action-button btn-large btn-primary",type="button", HTML('<i class="icon-star"></i>')), 
                add_busy_spinner(spin = "fading-circle")
        ),
        
        # Second tab content
        tabItem(tabName = "Donnees",
                add_busy_spinner(spin = "fading-circle"),
                htmlOutput("exp"),
                
                DT::dataTableOutput("mytable"),
                
                  fluidRow(
                    box(title = "Description des variables",
                        solidHeader = TRUE,
                        status = "primary",
                        selectInput(
                          "var",
                          "Sélectionner une variable:",
                          choices = colnames(df3),
                          selected = "SeriousDlqin2yrs"),
                        textOutput("result"),
                        verbatimTextOutput("summary"))
                  )),
        # Third tab content
        tabItem(tabName = "Methodologie",
                add_busy_spinner(spin = "fading-circle"),
                htmlOutput("inc")),
        
        #Table en plus
        
        tabItem(tabName = "Ttt",
                add_busy_spinner(spin = "fading-circle"),
                htmlOutput("texttt")
                ),
        
        # Fourth tab content
        tabItem(tabName = "Methodes",
                add_busy_spinner(spin = "fading-circle"),
                fluidRow(
                  box(title = "Sélection de la méthode",
                      status = "primary",
                      solidHeader = TRUE,
                      selectInput("method",
                                  "Sélectionner une méthode:",
                                  choices = c("Lasso", "Ridge","Adaptive Lasso", "Elastic Net", 
                                              "Random Forest", "Gradient Boosting", "ADA Boost") ,
                                  selected = "Lasso")),
                  
                   
                    
                  box(title = "Resume",
                        status = "primary",
                        solidHeader = TRUE,
                        tableOutput("result2")),
                  
                    box(title = "Matrice de confusion",
                        status = "primary",
                        solidHeader = TRUE,
                        verbatimTextOutput("result3")), 
                
                      box(title = "ROC",
                          status = "primary",
                          solidHeader = TRUE,
                          plotOutput("result4")),
                  
                  box(title = "Générer un rapport pour la methode choisit",
                      status = "primary",
                      solidHeader = TRUE,
                      downloadButton("Resultats", "Generate report"))
                  
                  )
        ),
        
        #Table de comparaison de methode
        
        tabItem(tabName = "Comparaison",
                add_busy_spinner(spin = "fading-circle"),
                  htmlOutput("ccl"),
                h2("Tableau de comparaison"), 
                fluidRow(align='center',tags$img(src = "Tableau.JPG")), 
                tags$br(), 
                h2("Graphiques des AUC"),tags$br(), tags$br(),
                fluidRow(tags$img(src = "Roc.JPG", width = 1500))),
        
        # Table a propos
        
        tabItem(tabName = "propos",
                add_busy_spinner(spin = "fading-circle"),
                fluidRow(
                  h1(strong(" Cette application à été développée par trois étudiantes du master ESA :"), align="center",style = "font-size:30px;"),
                  tags$br(),tags$br(),
                  div(img(src = "MASTER.JPG", height = 100, width = 600),style="text-align:center;"),
                  tags$br(),tags$br(),
                  tags$div(class = "col-md-4", align='center',
                           h1("Doryane Klein"), tags$img(src="Doryane.jpg"), tags$br(), tags$a(href = "mailto:doryane.klein@etu.univ-orleans.fr", "E-mail")),
                  tags$div(class = "col-md-4", align='center',
                           
                           h1("Manon Engeammes"), tags$img(src="Manon.jpg"), tags$br(), tags$a(href = "mailto:manon.engeammes@etu.univ-orleans.fr", "E-mail")),
                  
                  tags$div(class = "col-md-4", align='center',
                           h1("Wendy Lego"), tags$img(src="Wendy.jpg", height = 140, width = 140) , tags$br(), tags$a(href = "mailto:wendy.lego@gmail.com", "E-mail")),
                  tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(), tags$br(), tags$br(),
                  
                  tags$br(),tags$br(),tags$br(),
                  h1(strong("Merci"), align="center",style = "font-size:30px;")
                  
                )
         )
       )
      )
    )
  )

