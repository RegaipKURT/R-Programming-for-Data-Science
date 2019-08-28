#install.packages("ISLR")
library(ISLR)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("funModeling")
library(funModeling)
#install.packages("caret")
library(caret)
#install.packages("pROC")
library(pROC)
#install.packages("class")
library(class)#knn icin
#install.packages("e1071")
library(e1071)#knn icin
#install.packages("kernlab")
library(kernlab) #svm icin
#install.packages("ROCR")
library(ROCR) #roc icin
#install.packages("neuralnet")
library(neuralnet)
#install.packages("GGally")
library(GGally)
#install.packages("nnet")
library(nnet)
#install.packages("rpart")
library(rpart)
#install.packages("cli")
library(cli)
#install.packages("tree")
library(tree)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("randomForest")
library(randomForest)
#install.packages("gbm")
library(gbm)
#install.packages("xgboost")
library(xgboost)
#install.packages("DiagrammeR")
library(DiagrammeR)
#install.packages("mlbench")
library(mlbench)

rm(list = ls())

df <- Default
head(df)
train <- df[1:8000,]
test <- df[8001:10000,]

train_y <- train["default"]
test_y <- test["default"]

train_x <- train %>% dplyr::select(-default)
test_x <- test %>% dplyr::select(-default)
#knn algoritması

#knn için bütün değişkenlerin numerik olması gerekir.
#çünkü knn uzaklık metrikleri ile çalışır ve ölçebilmesi için numerik olması gerekir.

knn_train <- train
knn_test <- test

knn_train$student <- as.numeric(knn_train$student)
knn_test$student <- as.numeric(knn_test$student)

knn_test$default <- as.numeric(knn_test$default)
knn_train$default <- as.numeric(knn_train$default)

knn_train <- knn_train %>% select(-default)
knn_test <- knn_test %>% select(-default)

knn_model <- knn(train = knn_train, test = knn_test, 
                 cl = train_y$default, k = 5)

class_error <- function(gercek, tahmin) {
  return(
    data.frame(sin_hatasi = mean(gercek != tahmin), 
               dogruluk = 1-mean(gercek != tahmin)))
}

class_err <- function(gercek, tahmin) {
  
  mean(gercek != tahmin)
  
}

summary(knn_model)
summary(test_y$default)
deger <- class_error(train_y$default, knn_model)

deger
ctrl <- trainControl(method = "cv", number = 10, search = "grid",
                     summaryFunction = twoClassSummary, 
                     classProbs = T,
                     savePredictions = TRUE)

knn_grid <- data.frame(k=c(1:20))

knn_fit <- train(knn_train, train_y$default,
                 method="knn",
                 trControl = ctrl,
                 metric="ROC",
                 preProc = c("center", "scale"),
                 tuneGrid=knn_grid)

plot(knn_fit)
















