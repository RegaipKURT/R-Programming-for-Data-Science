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

#KULLANACAĞIMIZ VERİSETİ
# MEME KANSERİ AMLEİYATINDAN SONRA HAYATTA KALMA DURUMUNU İNCELİYORUZ.
# DOLAYISIYLA BİR SINIFLANDRIMA PROBLEMİNE ODAKLANACAĞIZ.

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases//haberman/haberman.data"

df <- read_csv(file = url, 
               col_names = c("Age", "Operation_Year", "Number_Pos_Nodes", "Survival"))

#VERİSETİNİN DÜZENLENMESİ
summary(df)

df$Survival <- ifelse(df$Survival == 1, 0, 1)

df$Survival <- factor(df$Survival)
summary(df)

ggpairs(df[,1:3])
ggpairs(df)

freq(df)

#SCALE  FONKSİYONU
scale01 <- function(x) {
  (x-min(x)) / (max(x)-min(x))
}

df <- df %>% mutate(Age = scale01(Age),
                    Number_Pos_Nodes = scale01(Number_Pos_Nodes),
                    Operation_Year = scale01(Operation_Year))

# TRAİN TEST AYRIMA İŞLEMLERİ
train_indeks <- createDataPartition(df$Survival, p = 0.8, list = F, times = 1)

train <- df[train_indeks,]
test <- df[-train_indeks,]

train_x <- train %>% select(-Survival)
train_y <- train$Survival

test_x <- test %>% select(-Survival)
test_y <- test$Survival

levels(train$Survival) <- make.names(levels(as.factor(train$Survival)))
train_y <- train$Survival
test_y <- test$Survival


# NEURAL NETWORK KURULMASI VE EĞİTİLMESİ
set.seed(800)

nn_fit <- nnet(Survival~., data = train, size=12, decay=0.0001)
summary(nn_fit)

# TAHMİN
predict(nn_fit, test_x)
pred_y <- predict(nn_fit, test_x, type="class")
pred_y <- ifelse(pred_y=="X0", 0, 1)
pred_y 
test_y
# TAHMİN SONUÇLARININ DEĞERLENDİRİLMESİ
defaultSummary(data.frame(pred=pred_y, 
                          obs=test_y))


#YSA MODEL TUNE
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)

ysa_grid <- expand.grid(size=c(2:12), decay=seq(0.0001, 0.1, length=15))

maxSize <- max(nnetGrid$size)
numWts <- 1*(maxSize * (length(ysa_train_x) + 1) + maxSize + 1)

ysa_tune <- train(
  train_x, train_y,
  method = "nnet",
  metric = "ROC",
  tuneGrid = ysa_grid,
  trace = FALSE, 
  maxit = 2000,
  MaxNWts = numWts,
  trControl = ctrl
)

plot(ysa_tune)

#YSA TUNE EDİLMİŞ MODEL İLE TAHMİN VE DEĞERLENDİRME
pred_y <- predict(ysa_tune, test_x)
pred_y <- ifelse(pred_y=="X0", 0, 1)
pred_y <- factor(pred_y)

defaultSummary(data.frame(pred=factor(pred_y), 
                          obs=test_y))

confusionMatrix(test_y, pred_y, positive = "1")


