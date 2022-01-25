############################# Tout les codes des méthodes #####################################################

## Library ##

library(dplyr)
library(randomForest)
library(pROC)
library(ROCR)
library(Matrix)
library(glmnet)
library(Rcpp)

## La table ##

df2 <- read.table("kaggle.txt")
df2 <- df2[-1]
df2 <- na.omit(df2)
df2

## Division train et test ##

smp_size <- floor(0.70 * nrow(df2))
set.seed(123)
train_df2 <- sample(seq_len(nrow(df2)), size = smp_size)

train <- df2[train_df2, ]
test <- df2[-train_df2, ]

train$sep <- "train"
test$sep <- "test"

## Camembert pour expliquer la division ##

Nb_train <- count(train, vars = sep)
Nb_test <- count(test, vars = sep)

df2_sep <- rbind(Nb_train,Nb_test)

df2_sep <- df2_sep %>% 
  rename(
    Class = vars,
    N = n)

library(ggplot2)

ggplot(df2_sep, aes(x="", y=N, fill=Class)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

summary(df2$V2)
attach(df2)
summary(V2)

###############################################################################################################
#####################                   RIDGE                          ########################################
###############################################################################################################




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
lambda_cv <- ridge_cv$lambda.min

# Fit final model
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)

x_test=as.matrix(test[,-1])

ridge_prob <- predict(model_cv,newx = x_test,s=lambda_cv,type="response")

ridge_pred=rep(1,nrow(x_test))
ridge_pred[ridge_prob<0.5]=0
table(ridge_pred,test$V2)

roc(test$V2,as.vector(ridge_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)





###############################################################################################################
#####################                   LASSO                          ########################################
###############################################################################################################

# using colMeans()
mean_val <- colMeans(df2,na.rm = TRUE)

for(i in colnames(df2))
  df2[,i][is.na(df2[,i])] <- mean_val[i]

sample=sample(1:nrow(df2),nrow(df2)*0.7)
train=df2[sample,]
test=df2[-sample,]


x <- model.matrix(V2~., train)[, -1]
y=train[,1]

# Perform 10-fold cross-validation to select lambda 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# Setting alpha = 1 implements lasso regression
lasso_cv <- cv.glmnet(x, y, alpha =1 , lambda=lambdas_to_try,
                      standardize = TRUE, nfolds = 10)

# Plot cross-validation results
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min

# Fit final model
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)

x_test=as.matrix(test[,-1])

lasso_prob <- predict(model_cv,newx = x_test,s=lambda_cv,type="response")

lasso_pred=rep(1,nrow(x_test))
lasso_pred[lasso_prob<0.5]=0
table(lasso_pred,test$V2)

roc(test$V2,as.vector(lasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)




###############################################################################################################
#####################              ADAPTATIVE LASSO                ############################################
###############################################################################################################

## Ridge ##

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
lambda_cv <- ridge_cv$lambda.min

# Fit final model
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)

x_test=as.matrix(test[,-1])

ridge_prob <- predict(model_cv,newx = x_test,s=lambda_cv,type="response")

ridge_pred=rep(1,nrow(x_test))
ridge_pred[ridge_prob<0.5]=0
table(ridge_pred,test$V2)

roc(test$V2,as.vector(ridge_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)


## Adaptative Lasso ##

best_ridge_coef <- as.numeric(coef(ridge_cv, s=ridge_cv$lambda.1se))[-1]

adalasso.fit <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = TRUE, nfolds = 10, penalty.factor=1 / abs(best_ridge_coef))
plot(adalasso.fit)

adalasso.fit$lambda.min

model_cv <- glmnet(x, y, alpha = 1 , lambda=adalasso.fit$lambda.min,  penalty.factor=1 / abs(best_ridge_coef), standardize = TRUE)


x_test=as.matrix(test[,-1])

lasso_prob <- predict(model_cv,newx = x_test,s=adalasso.fit$lambda.min,type="response")

lasso_pred=rep(1,nrow(x_test))
lasso_pred[lasso_prob<0.5]=0
table(lasso_pred,test$V2)

roc(test$V2,as.vector(lasso_prob),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)



###############################################################################################################
#####################                 ELASTIC NET                      ########################################
###############################################################################################################






###############################################################################################################
#####################                RANDOM FOREST                     ########################################
###############################################################################################################



set.seed(123456)
classifier_rf <- randomForest(as.factor(V2)~.,data = train[,-12], mtry=3, importance=T)
print(classifier_rf)

set.seed(123456)
rf.pred <- predict(classifier_rf,newdf2=test[,-12])
table(train$V2,rf.pred)

rf.pred <- predict(classifier_rf,newdf2=test[,-12])
a=mean(test$V2!=rf.pred)
err="Le taux d'erreur est : "
paste(err,round(a,4))

set.seed(123456)
rf.probs=predict(classifier_rf, newdf2=test[,-12], type="prob")
roc(test$V2,as.vector(rf.probs[,2]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)




## RANDOM FOREST MANON ##

mean_val <- colMeans(df2,na.rm = TRUE)

for(i in colnames(df2))
  df2[,i][is.na(df2[,i])] <- mean_val[i]

sample=sample(1:nrow(df2),nrow(df2)*0.7)
train=df2[sample,]
test=df2[-sample,]

y=train[,1]
y <- as.factor(y)
ytest <- as.factor(test$V2)
x_test=as.matrix(test[,-1])

#Appli des paramètres opti
model <- randomForest(x=train[,-1],y=y,ntree = 600)
model
varImpPlot(model)

#Matrice de confusion
y.random <- predict(model, newdata = x_test)
df=table(y.random,ytest)
specificite=(df[1,1])/(df[1,1]+df[1,2])
sensibilite=(df[2,2])/(df[2,1]+df[2,2])
Tx_bon_class=(df[1,1]+df[2,2])/(df[2,1]+df[2,2]+df[1,1]+df[1,2])
Tx_mauvais_class=(df[2,1]+df[1,2])/(df[2,1]+df[2,2]+df[1,1]+df[1,2])
table(y.random,ytest)

#ROC
y.random <- predict(model, newdata = x_test,type="prob")
roc(test$V2,as.vector(y.random[,1]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)
## AUC a 0.848 avec 500



###############################################################################################################
#####################               GRADIENT BOOST                     ########################################
###############################################################################################################


##BOOST
library(adabag)

#algorithme boosting
train$V2 <- as.factor(train$V2)
m.boosting <- boosting(V2 ~ ., data = train, boos = FALSE, mfinal = 100, coeflearn = 'Zhu')

#prédiction
xtest=test[,-1]

#Matrice de confu
y.boosting <- predict(m.boosting, newdata =xtest)
table(y.boosting,ytest)
df=table(y.boosting,ytest)
specificite=(df[1,1])/(df[1,1]+df[1,2])
sensibilite=(df[2,2])/(df[2,1]+df[2,2])
Tx_bon_class=(df[1,1]+df[2,2])/(df[2,1]+df[2,2]+df[1,1]+df[1,2])
Tx_mauvais_class=(df[2,1]+df[1,2])/(df[2,1]+df[2,2]+df[1,1]+df[1,2])



###############################################################################################################
#####################                   ADA BOOST                      ########################################
###############################################################################################################
