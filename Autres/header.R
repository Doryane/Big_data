library(pROC)
library(DT)
library(knitr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tibble)
library(shinycssloaders)
library(e1071)
library(precrec)
library(ggplot2)
library(dplyr)
library(class)
library(caret)
library(xgboost)
library(corrplot)
library(kernlab)
library(knitr)
library(kableExtra)
library(ggplot2)
library(xgboost)
library(corrplot)
library(pROC)
library(dplyr)
library(grid)
library(reshape2)
library(caret)
library(precrec)
library(e1071)
library(xgboost)


header <- dashboardHeader(title = "Penalised Logistic Tree Regression - PLTR",
                          titleWidth = 500,
                          tags$li(a(onclick = "openTab('intro')",
                                    href = NULL,
                                    icon("home"),
                                    title = "Homepage",
                                    style = "cursor: pointer;"),
                                  class = "dropdown",
                                  tags$script(HTML("
                                       var openTab = function(tabName){
                                       $('a', $('.sidebar')).each(function() {
                                       if(this.getAttribute('data-value') == tabName) {
                                       this.click()
                                       };
                                       });
                                       }"))))
 
                          