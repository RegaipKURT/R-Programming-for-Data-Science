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

#default veriseti temerrüde düşmüş kredileri gösteriyor. bağımlı değişken default değişkeni.
df <- Default
head(df)
train <- df[1:8000,]
test <- df[8001:10000,]

train_y <- train["default"]
test_y <- test["default"]

train_x <- train %>% dplyr::select(-default)
test_x <- test %>% dplyr::select(-default)

# model kurma

#lineer model
model_lm <- lm(as.numeric(default)-1 ~ balance, data = train)
summary(model_lm)

summary(model_lm$fitted.values)
plot(as.numeric(default)-1 ~ balance, data = train,
     col = "turquoise4",
     pch = "I",
     ylim = c(-0.2,1.2)
     )
abline(h = 0, lty=2)
abline(h=1, lty=2)
abline(h=0.5, lty=2, col="black")
abline(model_lm, lwd=3, col="darkorange", cex=0.01)


# logistic regression modeli

model_glm <- glm(as.numeric(default)-1 ~ balance, data = train, family = "binomial")

summary(model_glm)

curve(predict(model_glm, data.frame(balance=x), type="response"), add = T,
      lwd=3, col="red")

options(scipen = 9)

model_glm_pred <- ifelse(predict(model_glm, type="response") > 0.5, "Yes", "No")
head(model_glm_pred)
summary(model_glm_pred)
summary(train)


class_error <- function(gercek, tahmin) {
  return(data.frame(sin_hatasi = mean(gercek != tahmin), dogruluk = 1-mean(gercek != tahmin)))
}

degerler <- class_error(train$default, model_glm_pred)
degerler$sin_hatasi
degerler$dogruluk

tahmin <- table(gercek=train$default, tahmin=model_glm_pred)
tahmin


confusionMatrix(tahmin, positive = "Yes")
#positive sınıfın doğru belirtilmesi son derece önemli. aksi takdirde sonuçlar patlar.

mdoel_glm <- glm(as.numeric(default)-1 ~ ., data = train, family = "binomial")
test_ol <- predict(model_glm,test_x, type = "response")

roc(test_y$default~test_ol, plot=TRUE, print.auc=TRUE)
#nesne olarak kaydedilip içinden değerler de okunabilir.

# model tuning
attributes(trainControl())
ctrl <- trainControl(method = "cv", number = 10, 
                     summaryFunction = twoClassSummary,
                     savePredictions = TRUE,
                     classProbs = TRUE
                     )
glm_tune <- caret::train(train_x, train_y$default,
             method="glm",
             trControl=ctrl)

glm_tune$pred$Yes

defaultSummary(data.frame(obs = test_y$default,
                          pred = predict(glm_tune, test_x)
                          )
               )
train_hata <- confusionMatrix(data = predict(glm_tune, train_x), 
                              reference = train_y$default,
                              positive = "Yes")


test_hata <- confusionMatrix(data = predict(glm_tune, test_x), 
                             reference = test_y$default,
                              positive = "Yes")
train_hata
test_hata

roc(glm_tune$pred$obs,
    glm_tune$pred$Yes,
    levels = rev(levels(glm_tune$pred$obs)),
    plot = T,
    print.auc = T)


