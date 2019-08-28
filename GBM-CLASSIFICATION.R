
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
library(ISLR)

df <- Carseats
str(df)
summary(df)
hist(df$Sales)
df$Sales <- factor(ifelse(df$Sales>7.5, "H", "L"))
summary(df$Sales)


set.seed(123)
train_indeks <- createDataPartition(df$Sales, 
                                    p = .8, 
                                    list = FALSE, 
                                    times = 1)


train <- df[train_indeks,]
test  <- df[-train_indeks,]


train_x <- train %>% dplyr::select(-Sales)
train_y <- train$Sales
test_x <- test %>% dplyr::select(-Sales)
test_y <- test$Sales
#tek bir veri seti
training <- data.frame(train_x, Sales = train_y)

train$Sales <- as.factor(ifelse(as.numeric(train$Sales)==2,1,0))
train_y <- as.factor(ifelse(as.numeric(train_y)==2,1,0))

train$Sales <- as.numeric(train$Sales)
train <- transform(train, Sales = Sales - 1)

gbm_fit <- gbm(Sales~., data = train, 
               shrinkage = 0.01,
               distribution = "bernoulli",
               cv.folds = 5,
               n.trees = 3000,
               verbose = F)

gbm_fit
summary(gbm_fit)

gbm.perf(gbm_fit, method = "cv")
plot.gbm(gbm_fit,  3, gbm.perf(gbm_fit, method = "cv"))

y_pred <- predict.gbm(gbm_fit, test_x, type = "response")
y_pred <- ifelse(y_pred<0.5, "H","L")
y_pred <- as.factor(y_pred)

confusionMatrix(test_y, y_pred, positive = "H")
