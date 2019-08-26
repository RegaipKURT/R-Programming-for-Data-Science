rm(list = ls())

library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)
library(pls) #kismi en kucuk kareler ve pcr icin
library(elasticnet)
library(broom) #tidy model icin
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix) 
library(kernlab) #svm
library(e1071) #svm icin
library(rpart) #cart icin
library(pgmm) #olive data seti icin 
library(dslabs)
library(rpart.plot) #rpart gorsel icin
library(partykit) #karar agaci gorseli icin 
library(ipred) #bagging icin 
library(randomForest)
library(gbm)
library(nnet)
library(neuralnet)
library(GGally)
library(NeuralNetTools) #garson fonksiyonu icin
library(FNN)
library(dplyr)
library(ggpubr)

df <- Boston
head(df)

profiling_num(df)
glimpse(df)
summary(df)
ggpairs(df)
pairs(df,pch=18)

set.seed(3456)
train_indeks <- createDataPartition(df$medv, p=0.8, list = F, times = 1)
train <- df[train_indeks, ]
test <- df[-train_indeks, ]
train

train_x <- train %>% dplyr::select(-medv)
train_y <- train$medv
test_x <- test %>% dplyr::select(-medv)
test_y <- test$medv

gbm_fit <- gbm(medv ~., data = train, distribution = "gaussian", 
               n.trees = 5000,
               interaction.depth = 1,
               shrinkage = 0.01)
#shrinkage parametresi learning rate'e karşılık geliyor.
gbm_fit
summary(gbm_fit)

defaultSummary(data.frame(obs=train_y, 
                          pred=gbm_fit$fit))

# parametrelerle oynayarak yeniden model deneyelim

gbm_fit <- gbm(medv ~., data = train, distribution = "gaussian", 
               n.trees = 10000,
               interaction.depth = 1,
               shrinkage = 0.01,
               cv.folds = 5)

gbm.perf(gbm_fit, method = "cv")

defaultSummary(data.frame(obs=train_y, 
                          pred=gbm_fit$fit))


# train için RMSE = 2.45 oldu. Ama test hatamızı da hesaplayıp sonucu görelim

pred_y <- predict(gbm_fit, test_x)

#pred_y <- predict(gbm_fit, test_x, n.trees = 500)
# predict fonksiyonuyla gbm kullanılırken n.trees ile kaç ağaç kullanılacağı belirlenebilir.

defaultSummary(data.frame(obs=test_y, 
                          pred=pred_y))

plot(predict(gbm_fit, test_x, n.trees = 5000), test_y,
     xlab = "Tahmin Edilen", ylab = "Gercek",
     main = "Tahmin Edilen vs Gercek: GBM",
     col = "dodgerblue", pch = 20)

grid()

abline(0, 1, col = "darkorange", lwd = 2)


#gbm için model tuning

# 4 tane parametreyi optimize edebiliriz: 
# 1) n.trees, 2) interaction_depth (karmaşıklık katsayısı), 3)shrinkage, 4)minode

#minode eğitim seti içindeki gözlem sayısı

ctrl <- trainControl(method = "cv", number = 10, search = "grid")

gbm_grid <- expand.grid(interaction.depth = seq(1,7, by = 2),
            n.trees = seq(100,1000, 50),
            shrinkage = c(0.01, 0.1),
            n.minobsinnode = c(10:20)
            )

gbm_fit <- train(train_x, train_y, method = "gbm", 
      trControl = ctrl, tuneGrid = gbm_grid, verbose=FALSE)

plot(gbm_fit)
gbm_fit$finalModel

gbm_fit$results %>% 
  filter(n.trees == as.numeric(gbm_fit$bestTune$n.trees) &
           interaction.depth == as.numeric(gbm_fit$bestTune$interaction.depth) &
           shrinkage == as.numeric(gbm_fit$bestTune$shrinkage) &
           n.minobsinnode == as.numeric(gbm_fit$bestTune$n.minobsinnode))


defaultSummary(data.frame(obs = test_y, 
                          pred = predict(gbm_fit, test_x)))

"
shrinkage interaction.depth n.minobsinnode n.trees RMSE Rsquared MAE
1       0.1                 5             10     700  3.1      0.9 2.2

RMSESD RsquaredSD MAESD
1   0.87      0.036  0.36
"

















