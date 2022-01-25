#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library("readxl")
data=read.csv("kaggle.csv",header = TRUE, sep = ",", quote = "\"",
              dec = ".", fill = TRUE)


data= data[,-1]

# using colMeans()
mean_val <- colMeans(data,na.rm = TRUE)

# replacing NA with mean value of each column
for(i in colnames(data))
  data[,i][is.na(data[,i])] <- mean_val[i]



sample=sample(1:nrow(data),nrow(data)*0.7)
train=data[sample,]
test=data[-sample,]

x <- model.matrix(V2~., train)[, -1]
y=train[,1]

x_test=as.matrix(test[,-1])

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

set.seed(987654) 

#################### CODES ET METHODES ######################

## LASSO ##

lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lasso_cv <- cv.glmnet(x, y, alpha =1 , lambda=lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(lasso_cv)
# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min
# Fit final model
model_cv_lasso <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
lasso_prob <- predict(model_cv_lasso,newx = x_test,s=lambda_cv,type="response")
lasso_pred=rep(1,nrow(x_test))
lasso_pred[lasso_prob<0.5]=0
lasso_table = table(lasso_pred,test$V2)
specificite_lasso=(lasso_table[1,1])/(lasso_table[1,1]+lasso_table[1,2])
sensibilite_lasso=(lasso_table[2,2])/(lasso_table[2,1]+lasso_table[2,2])
Tx_bon_class_lasso=(lasso_table[1,1]+lasso_table[2,2])/(lasso_table[2,1]+lasso_table[2,2]+lasso_table[1,1]+lasso_table[1,2])
Tx_mauvais_class_lasso=(lasso_table[2,1]+lasso_table[1,2])/(lasso_table[2,1]+lasso_table[2,2]+lasso_table[1,1]+lasso_table[1,2])

Resume_lasso = c(specificite_lasso, sensibilite_lasso,Tx_bon_class_lasso,Tx_mauvais_class_lasso)
Nom = c("Specificite","Sensibilite","Taux de bonne classification", "Taux de mauvaise classification")
Resume_lasso = as.data.frame(Resume_lasso)
Nom = as.data.frame(Nom)
Resume_lasso = cbind(Nom,Resume_lasso)

roc_lasso = roc(test$V2,as.vector(lasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)

## RIDGE ##

x <- model.matrix(V2~., train)[, -1]
y=train[,1]
# Perform 10-fold cross-validation to select lambda 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 0 implements ridge regression
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda=lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(ridge_cv)
# Best cross-validated lambda
lambda_cv_ridge <- ridge_cv$lambda.min
# Fit final model
model_cv_ridge <- glmnet(x, y, alpha = 0, lambda = lambda_cv_ridge, standardize = TRUE)
x_test=as.matrix(test[,-1])
ridge_prob <- predict(model_cv_ridge,newx = x_test,s=lambda_cv_ridge,type="response")
ridge_pred=rep(1,nrow(x_test))
ridge_pred[ridge_prob<0.5]=0
ridge_table = table(ridge_pred,test$V2)
specificite_ridge=(ridge_table[1,1])/(ridge_table[1,1]+ridge_table[1,2])
sensibilite_ridge=(ridge_table[2,2])/(ridge_table[2,1]+ridge_table[2,2])
Tx_bon_class_ridge=(ridge_table[1,1]+ridge_table[2,2])/(ridge_table[2,1]+ridge_table[2,2]+ridge_table[1,1]+ridge_table[1,2])
Tx_mauvais_class_ridge=(ridge_table[2,1]+ridge_table[1,2])/(ridge_table[2,1]+ridge_table[2,2]+ridge_table[1,1]+ridge_table[1,2])

Resume_ridge = c(specificite_ridge, sensibilite_ridge,Tx_bon_class_ridge,Tx_mauvais_class_ridge)
Nom = c("Specificite","Sensibilite","Taux de bonne classification", "Taux de mauvaise classification")
Resume_ridge = as.data.frame(Resume_ridge)
Nom = as.data.frame(Nom)
Resume_ridge = cbind(Nom,Resume_ridge)

roc_ridge = roc(test$V2,as.vector(ridge_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)


## ADAPTIVE LASSO ##



best_ridge_coef <- as.numeric(coef(ridge_cv, s=ridge_cv$lambda.1se))[-1]
adalasso.fit <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = TRUE, nfolds = 10, penalty.factor=1 / abs(best_ridge_coef))
lambda_cv_adpt=adalasso.fit$lambda.min
model_cv_alasso <- glmnet(x, y, alpha = 1 , lambda=adalasso.fit$lambda.min,  penalty.factor=1 / abs(best_ridge_coef), standardize = TRUE)
x_test=as.matrix(test[,-1])
alasso_prob <- predict(model_cv_alasso,newx = x_test,s=adalasso.fit$lambda.min,type="response")
alasso_pred=rep(1,nrow(x_test))
alasso_pred[alasso_prob<0.5]=0
al=table(alasso_pred,test$V2)
roc(test$V2,as.vector(alasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)

al_table = table(alasso_pred,test$V2)
specificite_al=(al_table[1,1])/(al_table[1,1]+al_table[1,2])
sensibilite_al=(al_table[2,2])/(al_table[2,1]+al_table[2,2])
Tx_bon_class_al=(al_table[1,1]+al_table[2,2])/(al_table[2,1]+al_table[2,2]+al_table[1,1]+al_table[1,2])
Tx_mauvais_class_al=(al_table[2,1]+al_table[1,2])/(al_table[2,1]+al_table[2,2]+al_table[1,1]+al_table[1,2])

Resume_al = c(specificite_al, sensibilite_al,Tx_bon_class_al,Tx_mauvais_class_al)
Nom = c("Specificite","Sensibilite","Taux de bonne classification", "Taux de mauvaise classification")
Resume_al = as.data.frame(Resume_al)
Nom = as.data.frame(Nom)
Resume_al = cbind(Nom,Resume_al)

roc_al = roc(test$V2,as.vector(alasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)





## ELASTIC-NET ##

alphalist <- seq(0,1,by=0.1)
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(x, y, alpha =a , lambda=lambdas_to_try,
            standardize = TRUE, nfolds = 10)
})
for (i in 1:11) {print(min(elasticnet[[i]]$cvm))}
elastic_net=cv.glmnet(x,y, alpha=0.8, lambda = lambdas_to_try, standardize= TRUE, nfolds=10)
# Plot cross-validation results
plot(elastic_net)
# Best cross-validated lambda
lambda_cv_elastic <- elastic_net$lambda.min
# Fit final model
model_cv_elastic <- glmnet(x, y, alpha =0.8, lambda = lambda_cv, standardize = TRUE)
elastic_prob <- predict(model_cv_elastic,newx = x_test,s=lambda_cv,type="response")
elastic_pred=rep(1,nrow(x_test))
elastic_pred[elastic_prob<0.5]=0
e=table(elastic_pred,test$V2)
roc(test$V2,as.vector(elastic_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)

el_table = table(elastic_pred,test$V2)
specificite_el=(el_table[1,1])/(el_table[1,1]+el_table[1,2])
sensibilite_el=(el_table[2,2])/(el_table[2,1]+el_table[2,2])
Tx_bon_class_el=(el_table[1,1]+el_table[2,2])/(el_table[2,1]+el_table[2,2]+el_table[1,1]+el_table[1,2])
Tx_mauvais_class_el=(el_table[2,1]+el_table[1,2])/(el_table[2,1]+el_table[2,2]+el_table[1,1]+el_table[1,2])

Resume_el = c(specificite_el, sensibilite_el,Tx_bon_class_el,Tx_mauvais_class_el)
Nom = c("Specificite","Sensibilite","Taux de bonne classification", "Taux de mauvaise classification")
Resume_el = as.data.frame(Resume_el)
Nom = as.data.frame(Nom)
Resume_el = cbind(Nom,Resume_el)

roc_el = roc(test$V2,as.vector(elastic_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)


## RANDOM FOREST ##

y=train[,1]
y <- as.factor(y)
ytest <- as.factor(test$V2)
x_test=as.matrix(test[,-1])

rf= ranger(x=train[,-1],y=y,mtry=2, probability=TRUE)
rf.probs=predict(rf,data=test)
rf_probs = predict(rf,data=test)
rf.pred=rep(1,nrow(x_test))
rf.pred[rf.probs$predictions[,2]<0.5]=0
rf_table=table(rf.pred,test$V2)
specificite_rf=(rf_table[1,1])/(rf_table[1,1]+rf_table[1,2])
sensibilite_rf=(rf_table[2,2])/(rf_table[2,1]+rf_table[2,2])
Tx_bon_class_rf=(rf_table[1,1]+rf_table[2,2])/(rf_table[2,1]+rf_table[2,2]+rf_table[1,1]+rf_table[1,2])
Tx_mauvais_class_rf=(rf_table[2,1]+rf_table[1,2])/(rf_table[2,1]+rf_table[2,2]+rf_table[1,1]+rf_table[1,2])
Resume_rf = c(specificite_rf, sensibilite_rf,Tx_bon_class_rf,Tx_mauvais_class_rf)
Nom = c("Specificite","Sensibilite","Taux de bonne classification", "Taux de mauvaise classification")
Resume_rf = as.data.frame(Resume_rf)
Nom = as.data.frame(Nom)
Resume_rf = cbind(Nom,Resume_rf)

roc_rf = roc(test$V2,as.vector(rf.probs$predictions[,2]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)


## GRADIENT BOOSTING ##

x <- model.matrix(V2~., train)[, -1]
y=train[,1]
cv=xgb.cv(data=x, label=y,nrounds = 100, nfold = 5, eta = 0.3, depth = 6)
elog <- as.data.frame(cv$evaluation_log)
nrounds <- which.min(elog$test_rmse_mean)
model <- xgboost(data=x, label=y,
                 nrounds = nrounds,                
                 eta = 0.3,                 
                 depth = 6)
x_test=as.matrix(test[,-1])
gb_prob <- predict(model, x_test)
gb_pred=rep(1,nrow(x_test))
gb_pred[gb_prob<0.5]=0
gb=table(gb_pred,test$V2)
roc(test$V2,as.vector(gb_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)

gb_table = table(gb_pred,test$V2)
specificite_gb=(gb_table[1,1])/(gb_table[1,1]+gb_table[1,2])
sensibilite_gb=(gb_table[2,2])/(gb_table[2,1]+gb_table[2,2])
Tx_bon_class_gb=(gb_table[1,1]+gb_table[2,2])/(gb_table[2,1]+gb_table[2,2]+gb_table[1,1]+gb_table[1,2])
Tx_mauvais_class_gb=(gb_table[2,1]+gb_table[1,2])/(gb_table[2,1]+gb_table[2,2]+gb_table[1,1]+gb_table[1,2])

Resume_gb = c(specificite_gb, sensibilite_gb,Tx_bon_class_gb,Tx_mauvais_class_gb)
Nom = c("Specificite","Sensibilite","Taux de bonne classification", "Taux de mauvaise classification")
Resume_gb = as.data.frame(Resume_gb)
Nom = as.data.frame(Nom)
Resume_gb = cbind(Nom,Resume_gb)

roc_gb =  roc(test$V2,as.vector(gb_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)



## ADA BOOST

gbm_algorithm <- gbm(V2 ~ ., data = train, distribution = "adaboost", n.trees = 1000)
gbm_predicted <- predict(gbm_algorithm, test, n.trees = 1000,type = 'response')
gbm_pred=rep(1,nrow(x_test))
gbm_pred[gbm_predicted<0.5]=0

gbm_table = table(gbm_pred,test$V2)
specificite_gbm=(gbm_table[1,1])/(gbm_table[1,1]+gbm_table[1,2])
sensibilite_gbm=(gbm_table[2,2])/(gbm_table[2,1]+gbm_table[2,2])
Tx_bon_class_gbm=(gbm_table[1,1]+gbm_table[2,2])/(gbm_table[2,1]+gbm_table[2,2]+gbm_table[1,1]+gbm_table[1,2])
Tx_mauvais_class_gbm=(gbm_table[2,1]+gbm_table[1,2])/(gbm_table[2,1]+gbm_table[2,2]+gbm_table[1,1]+gbm_table[1,2])

Resume_gbm = c(specificite_gbm, sensibilite_gbm,Tx_bon_class_gbm,Tx_mauvais_class_gbm)
Nom = c("Specificite","Sensibilite","Taux de bonne classification", "Taux de mauvaise classification")
Resume_gbm = as.data.frame(Resume_gbm)
Nom = as.data.frame(Nom)
Resume_gbm = cbind(Nom,Resume_gbm)

roc_ada=roc(test$V2,as.vector(gbm_predicted),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)

########################## TABLEAU DE COMPARAISON ##############################

AUC_Lasso <- auc(test$V2,as.vector(lasso_prob))
AUC_Ridge <- auc(test$V2,as.vector(ridge_prob))
AUC_Adaptative_Lasso <- auc(test$V2,as.vector(alasso_prob))
AUC_Elastic <- auc(test$V2,as.vector(elastic_prob))
AUC_GB <- auc(test$V2,as.vector(gb_prob))
AUC_RF <- auc(test$V2,as.vector(rf_probs$predictions[,2]))
AUC_gbm <- auc(test$V2,as.vector(gbm_predicted))

Indice = c("AUC","Sensibilite","Specificite","Taux de bonne classification", "Taux de mauvaise classification")
Lasso =  round(c(AUC_Lasso,sensibilite_lasso,specificite_lasso,Tx_bon_class_lasso,Tx_mauvais_class_lasso),digits = 3)
Adaptative_Lasso =  round(c(AUC_Ridge,sensibilite_al,specificite_al,Tx_bon_class_al,Tx_mauvais_class_al),digits = 3)
Ridge =  round(c(AUC_Adaptative_Lasso,sensibilite_ridge,specificite_ridge,Tx_bon_class_ridge,Tx_mauvais_class_ridge),digits = 3)
Elastic_Net =  round(c(AUC_Elastic,sensibilite_el,specificite_el,Tx_bon_class_el,Tx_mauvais_class_el),digits = 3)
Gradient_Boosting =  round(c(AUC_GB,sensibilite_gb,specificite_gb,Tx_bon_class_gb,Tx_mauvais_class_gb),digits = 3)
Random_forest = round(c(AUC_RF,sensibilite_rf,specificite_rf,Tx_bon_class_rf,Tx_mauvais_class_rf),digits = 3)
ADA_boost = round(c(AUC_gbm,sensibilite_gbm,specificite_gbm,Tx_bon_class_gbm,Tx_mauvais_class_gbm),digits = 3)

Tab_Comp = cbind(Indice,Lasso,Adaptative_Lasso,Ridge,Elastic_Net,Gradient_Boosting,Random_forest,ADA_boost)
Tab_Comp = as.data.frame(Tab_Comp)

########################## GRAPHIQUE DE COMPARAISON ############################


lasso_prob <- predict(model_cv_lasso,newx = x_test,s=lambda_cv,type="response")
ridge_prob <- predict(model_cv_ridge,newx = x_test,s=lambda_cv_ridge,type="response")
alasso_prob <- predict(model_cv_alasso,newx = x_test,s=adalasso.fit$lambda.min,type="response")
elastic_prob <- predict(model_cv_elastic,newx = x_test,s=lambda_cv,type="response")
gb_prob <- predict(model, x_test)
rf.probs <- predict(rf,data=test)

roc_lasso <- roc(test$V2,as.vector(lasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="orangered",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
roc_ridge <- roc(test$V2,as.vector(ridge_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="snow2",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
roc_alasso <- roc(test$V2,as.vector(alasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="hotpink",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
roc_elastic <- roc(test$V2,as.vector(elastic_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="violetred4",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
roc_gb <- roc(test$V2,as.vector(gb_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
roc_rf <- roc(test$V2,as.vector(rf.probs$predictions[,2]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
roc_ada <- roc(test$V2,as.vector(gbm_predicted),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)

####################### Partie serveur #########################################


server <- function(input, output,session) {
  
  # Barres d'attentes
  show_spinner()
  
  
  observeEvent(input$lol, {
    updateTabItems(session,"sidebar", "Introduction")
  })
  
  
  
  # Intro
  
  
  getPage2<-function() {
    return(includeHTML("Intro.html"))
  }
  output$intro<-renderUI({getPage2()})
  
  observeEvent(input$lol2, {
    updateTabItems(session,"sidebar", "Methodologie")
  })
  
  #Explication des variables
  
  getPage3<-function() {
    return(includeHTML("Explication.html"))
  }
  output$exp<-renderUI({getPage3()})
  
  
  output$mytable = DT::renderDataTable({df3}, options = list(
    pageLength = 15, autoWidth = TRUE,
    columnDefs = list(list( targets = 2, width = '50px')),
    scrollX = TRUE
  ))
  
  output$result <- renderText({
    if (input$var == "SeriousDlqin2yrs_V2") {
      print('The person experienced 90 days past due delinquency or worse (Yes/No)')} 
    else if (input$var == "RevolvingUtilizationOfUnsecuredLines_V3") {
      print('Total balance on credit cards and personal lines of credit except real estate and no instalment
      debt such as car loans divided by the sum of credit limits')}
    else if (input$var == "Age_V4") {
      print('Age of the borrower (in years)')}
    else if (input$var == "NumberOfTime30_59DaysPastDueNotWorse_V5") {
      print('Number of times a borrower has been between 30 and 59 days past due but not worse in the
      last 2 years')}
    else if (input$var == "DebtRatio_V6") {
      print('Monthly debt payments, alimony and living costs over the monthly gross income')}
    else if (input$var == "MonthlyIncome_V7") {
      print('Monthly Income')}
    else if (input$var == "NumberOfOpenCreditLinesAndLoans_V8") {
      print('Number of open loans (like car loan or mortgage) and credit lines (credit cards)')}
    else if (input$var == "NumberOfTimes90DaysLate_V9") {
      print('Number of times a borrower has been 90 days or more past due')}
    else if (input$var == "NumberRealEstateLoansOrLines_V10") {
      print('Number of mortgage and real estate loans including home equity lines of credit')}
    else if (input$var == "NumberOfTimes60_89DaysPastDueNotWorse_V11") {
      print('Number of times a borrower has been between 60 and 89 days past due but not worse in the last 2 years')}
    else if (input$var == "NumberOfDependents_V12") {
      print('Number of dependents in family excluding themselves (spouse, children, etc.)')}
    else{print("Error")}
  })
  
  output$summary <- renderPrint({
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
    summary(df3[input$var])
  })
  
  # Methodologie
  
  getPage<-function() {
    return(includeHTML("Methodologie.html"))
  }
  output$inc<-renderUI({getPage()})
  
  # Page en plus
  
  getPage5<-function() {
    return(includeHTML("texttt.html"))
  }
  output$texttt<-renderUI({getPage5()})
  
  
  
  # Comparaison des methodes
  
  output$result2 <- renderTable({
    
    set.seed(987654)
    if (input$method == "Lasso") {Resume_lasso}
    
    else if (input$method == "Ridge") {Resume_ridge}
    
    else if (input$method == "Adaptive Lasso") {Resume_al}
    
    else if (input$method == "Elastic Net") {Resume_el}
    
    else if (input$method == "Gradient Boosting") {Resume_gb}
    
    else if (input$method == "Random Forest") {Resume_rf}
    
    else if (input$method == "ADA Boost"){Resume_gbm}
  })
  
  
  
  output$result3 <- renderPrint(
    if (input$method == "Lasso") {table(lasso_pred,test$V2)}
    
    else if (input$method == "Ridge") {table(ridge_pred,test$V2)}
    
    else if (input$method == "Adaptive Lasso") {table(alasso_pred,test$V2)}
    
    else if (input$method == "Elastic Net") {table(elastic_pred,test$V2)}
    
    else if (input$method == "Gradient Boosting") {table(gb_pred,test$V2)}
    
    else if (input$method == "Random Forest") {table(rf.pred,test$V2)}
    
    else if (input$method == "ADA Boost"){table(gbm_pred,test$V2)}
  )
  
  
  
  output$result4 <- renderPlot({
    if (input$method == "Lasso") {
      lasso_prob <- predict(model_cv_lasso,newx = x_test,s=lambda_cv,type="response")
      roc(test$V2,as.vector(lasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)}
    
    else if (input$method == "Ridge") {
      ridge_prob <- predict(model_cv_ridge,newx = x_test,s=lambda_cv_ridge,type="response")
      roc(test$V2,as.vector(ridge_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
    }
    
    else if (input$method == "Adaptive Lasso") {
      alasso_prob <- predict(model_cv_alasso,newx = x_test,s=adalasso.fit$lambda.min,type="response")
      roc(test$V2,as.vector(alasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
    }
    
    else if (input$method == "Elastic Net") {
      elastic_prob <- predict(model_cv_elastic,newx = x_test,s=lambda_cv,type="response")
      roc(test$V2,as.vector(elastic_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
    }
    
    else if (input$method == "Gradient Boosting") {
      gb_prob <- predict(model, x_test)
      roc(test$V2,as.vector(gb_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
      
    }
    
    else if (input$method == "Random Forest") {
      rf.probs=predict(rf,data=test)
      roc(test$V2,as.vector(rf.probs$predictions[,2]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
    }
    
    else if (input$method == "ADA Boost"){
      gbm_predicted <- predict(gbm_algorithm, test, n.trees = 1000,type = 'response')
      roc(test$V2,as.vector(gbm_predicted),plot=TRUE,legacy.axes=TRUE, lwd=2, col="lightseagreen",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
      
    }
    
  })
  
  output$Resultats <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Resultats.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Resultats.Rmd")
      file.copy("Resultats.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$method,
                     roc_lasso = roc_lasso,
                     roc_ridge = roc_ridge,
                     roc_al = roc_al,
                     roc_el = roc_el,
                     roc_rf = roc_rf,
                     roc_gb = roc_gb,
                     roc_ada = roc_ada, 
                     
                     lasso_table = lasso_table,
                     ridge_table = ridge_table,
                     al_table = al_table,
                     el_table = el_table,
                     rf_table = rf_table,
                     gb_table =  gb_table, 
                     gbm_table = gbm_table,
                     
                     Resume_lasso = Resume_lasso,
                     Resume_ridge = Resume_ridge,
                     Resume_al = Resume_al,
                     Resume_el = Resume_el,
                     Resume_rf = Resume_rf, 
                     Resume_gb = Resume_gb, 
                     Resume_gbm = Resume_gbm)
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  #Comparaison
  
  
  
  output$result5 = DT::renderDataTable({Tab_Comp}, options = list(
    pageLength = 15, autoWidth = TRUE,
    columnDefs = list(list( targets = 2, width = '50px')),
    scrollX = TRUE
  ))
  
  output$result6 <- renderPlot(
    ggroc(list("Roc Lasso" = roc_lasso , "Roc ridge" = roc_ridge,
               "Roc adaptive Lasso" = roc_ridge, "Roc Elastic Net" = roc_elastic,
               "Roc Gradient Boosting" = roc_gb, "Roc Random Frest" = roc_rf,
               "Roc ADA Gradient Boosting" = roc_ada))
  )
  
  getPage4<-function() {
    return(includeHTML("Conclusion.html"))
  }
  output$ccl<-renderUI({getPage4()})
  
}

