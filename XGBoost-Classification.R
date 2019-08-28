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

data(PimaIndiansDiabetes) 
df <- PimaIndiansDiabetes
summary(df)
profiling_num(df)
str(df)
ggpairs(df, verbose=FALSE)

glimpse(df)
plot_num(df)
freq(df)

set.seed(123)
train_indeks <- createDataPartition(df$diabetes, p = .8, list = FALSE, times = 1)

train <- df[train_indeks,]
test  <- df[-train_indeks,]

train_x <- train %>% dplyr::select(-diabetes)
train_y <- train$diabetes
test_x <- test %>% dplyr::select(-diabetes)
test_y <- test$diabetes

#tek bir veri seti
training <- data.frame(train_x, diabetes = train_y)

train_y <- as.numeric(train_y) - 1 
d_train <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)

test_y <- as.numeric(test_y) - 1 
d_test <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)

xgb_model <- xgboost(data = d_train, max.depth=2, eta=1, ntread=2,
                     nrounds = 20, objective="binary:logistic", verbose = 1)

bst <- xgb.train(data = d_train,  max.depth=1, eta=0.34, ntread=2,
          nrounds = 31, eval.metric="error", gamma=1.7,
          eval.metric="logloss", objective = "binary:logistic")

mm <- xgb.importance(model = bst)
mm
xgb.plot.importance(mm)

xgb.dump(bst)
xgb.plot.tree(model = bst)

pred_y_xgb <- factor(ifelse(predict(bst, as.matrix(test_x))<0.5,0,1))
pred_y_xgb2 <- factor(ifelse(predict(bst, as.matrix(test_x))<0.5,0,1))

model_lm <- lm(as.numeric(diabetes)~., data = train)
pred_lm <- predict(model_lm, test_x)
pred_lm <- factor(ifelse(pred_lm>1.5, 1,0))

confusionMatrix(factor(test_y), pred_lm, positive = "1")


confusionMatrix(factor(test_y), pred_y_xgb, positive = "1")

model_rm <- randomForest(diabetes~., data = train, ntree=500)
pred_y_rf <- factor(ifelse(predict(model_rm, test_x) == "neg", 0, 1))

confusionMatrix(factor(test_y), pred_y_rf, positive = "1")

model_knn <- knn(train = train_x, test = test_x, cl = train_y, k = 15)
confusionMatrix(factor(test_y), factor(model_knn), positive = "1")


tahminler <- data.frame(xgb=pred_y_xgb, xgb2=pred_y_xgb2, 
                        rf=pred_y_rf, knn=factor(model_knn),
                        lm=pred_lm)

tahminler

tahminler$sonuc <- factor(apply(tahminler, 1, median))

summary(tahminler)


confusionMatrix(factor(test_y), tahminler$sonuc, positive = "1")


ctrl <- trainControl(method = "cv",
                    number = 10,
                    summaryFunction = twoClassSummary, 
                    classProbs = TRUE)

xgb_grid <- expand.grid(eta = seq(0.01,2,length.out = 7), 
                        nrounds = seq(5,100, by = 20),  
                        max_depth = 1:7,  
                        min_child_weight = 2,  
                        colsample_bytree = 0.5, 
                        gamma = 0, 
                        subsample = 1)

dim(xgb_grid)


xgb_tune <- train(diabetes~., data = train,
                  method = "xgbTree",
                  tuneGrid = xgb_grid,
                  trControl = ctrl,
                  metric = "ROC")
xgb_tune
xgb_tune$bestTune
xgb.plot.tree(model=xgb_tune$bestTune)

pred_y <- predict(xgb_tune, as.matrix(test_x))
pred_y <- factor(ifelse(pred_y == "neg", 0, 1))
confusionMatrix(pred_y,factor(test_y), positive = "1")
getTrainPerf(xgb_tune)
























