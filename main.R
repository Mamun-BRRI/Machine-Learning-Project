########## Model building for individual
library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(readxl)
library(readxl)
R<-read.csv("Data.csv")
R
attach(R)

library(caret)
validation_index <- createDataPartition(R$Y, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- R[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- R[validation_index,]

##### Best model selection
control<- trainControl(method = "cv" , number = 20)
metric<- "RMSE"

#1
set.seed(7)
fit.knn<- train(Y~., data = dataset,method="knn", metric=metric,trControl=control)
#2
set.seed(7)
fit.svm<- train(Y~., data = dataset, method="svmRadial", metric=metric, trControl=control)


#3
set.seed(7)
fit.rf<- train(Y~., data = dataset, method="rf", metric=metric, trControl=control)
#4gbm
set.seed(7)
fit.gbm <- train(Y~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
#5 "cforest"
fit.cforest<- train(Y~., data = dataset, method="cforest", metric=metric, trControl=control)
#6 "ranger"
fit.ranger<- train(Y~., data = dataset, method="ranger", metric=metric, trControl=control)
ranger=fit.ranger

### selected model
bagging_results <- resamples(list(knn=fit.knn,svm=fit.svm,rf=fit.rf,
                                  gbm=fit.gbm,cforest=fit.cforest,
                                  ranger=fit.ranger))

summary(bagging_results)
dotplot(bagging_results)

#################
knn_P<- predict(fit.knn, validation)
svm_P<- predict(fit.svm, validation)
rf_P<- predict(fit.rf, validation)
gbm_P<- predict(fit.gbm, validation)
cforest_P<- predict(fit.cforest, validation)
ranger_P<- predict(fit.ranger, validation)


validation$knn<-knn_P
validation$svm<-svm_P
validation$rf<-rf_P
validation$gbm<-gbm_P
validation$cforest<-cforest_P
validation$ranger<-ranger_P

cor(validation$YSB,validation$knn)
cor(validation$YSB,validation$svm)
cor(validation$YSB,validation$rf)
cor(validation$YSB,validation$gbm)
cor(validation$YSB,validation$cforest)
cor(validation$YSB,validation$ranger)

