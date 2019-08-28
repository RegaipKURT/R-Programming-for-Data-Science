
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


#model kurma
set_tree <- tree(Sales~., data = df)
summary(set_tree)
plot(set_tree)
text(set_tree)

tree_class <- rpart(Sales~., data = df, method = "class")
rpart.plot(tree_class)
summary(tree_class)

tree_class
plotcp(tree_class)

# en düşük hatayı veren cp parametresini modelin içinden alma
# biraz uzun ve karmaşık ama lazım.
min_cp <- tree_class$cptable[which.min(tree_class$cptable[,"xerror"]), "CP"]
min_cp

# min_cp ile karar ağacını budamak
tree_pruned <- prune(tree = tree_class, cp = min_cp)
rpart.plot(tree_pruned)

tree_class
tree_pruned

prp(tree_class, type = 1)

y_pred <- predict(tree_pruned, test_x, type="class")

#test için cm
confusionMatrix(test_y, y_pred, positive = "H")

pred_y_tr <- predict(tree_pruned, train_x, type="class")
confusionMatrix(train_y, pred_y_tr)



# model tuning işlemleri

# cv.tree ile model tune etme işlemi
seat_tree <- tree(Sales ~ . , data = train)

set.seed(12312153)
seat_tree_cv <- cv.tree(seat_tree, FUN = prune.misclass, K = 10)
min_tree <- which.min(seat_tree_cv$dev)
seat_tree_cv[min_tree]
par(mfrow = c(1,2))
plot(seat_tree_cv)
plot(seat_tree_cv$size, 
     seat_tree_cv$dev / nrow(train), 
     type = "b",
     xlab = "Agac Boyutu/Dugum Sayisi", ylab = "CV Yanlis Siniflandirma Orani")


# caret ile model tune işlemi
set.seed(123)
ctrl <- trainControl(method="cv",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


cart_tune <- train(
  x = train_x,
  y = train_y,
  method = "rpart",
  tuneLength = 50,
  metric = "ROC",
  trControl = ctrl)

plot(cart_tune)

confusionMatrix(predict(cart_tune, test_x), test_y, positive = "H")

#RANDOM FOREST İLE SINIFLANDIRMA

rf_fit <- randomForest(Sales~., data = train, ntree=150, mtry=10)

pred_y <- predict(rf_fit, test_x)

test_y
pred_y
#test hatası
confusionMatrix(pred_y, test_y, positive = "H")
#train hatası
confusionMatrix(predict(rf_fit, train_x), train_y, positive = "H")


# model tuning
ctrl <- trainControl(method = "cv", number = 10, search='grid')

rf_grid <- expand.grid(data.frame(mtry=c(1:10)))
rf_tune <- train(Sales~., data=train, tuneGrid = rf_grid, trControl=ctrl,
                 method="rf", metric = 'Accuracy')

plot(rf_tune)
predict(rf_tune, test_x)
confusionMatrix(predict(rf_tune, test_x), test_y, positive = "H")

































