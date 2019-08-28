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


set.seed(10111)
x <- matrix(rnorm(40), 20, 2)
y <- rep(c(-1,1), c(10,10))
head(x)
head(y)
x[y==1,] <- x[y==1,] + 1 
head(x)
plot(x, col=y+3,pch=19)

df <- data.frame(x, y=as.factor(y))

#svm model

svm_model1 <- svm(y~., data = df, kernel="polynomial", cost=10, scale = F) 
summary(svm_model1)

plot(svm_model1, df)

#SVM MODELİNİN NASIL OLUŞTUĞUNU ANLAMAK
##===========================================================
#BURADA DF'NİN MİNİMUM VE MAKSİMUM DEĞERLERİ ARASINDA DEĞERLER OLUŞTURUP
# SVM YAPISNININ NASIL ORTAYA ÇIKTIĞINI ANLAMAYA ÇALIŞACAĞIZ.

a <- seq(from=apply(df,2, range)[1,1], 
         to = apply(df, 2, range)[2,1],
         length = 5)
b <- seq(from=apply(df,2, range)[1,2], 
         to = apply(df, 2, range)[2,2],
         length = 5)
expand.grid(a,b)

make_grid <- function(x, n = 75) {
  g_range = apply(x, 2, range)
  x1 = seq(from = g_range[1,1], to = g_range[2,1], length = n)
  x2 = seq(from = g_range[1,2], to = g_range[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

x_grid <- make_grid(x)
x_grid[1:10,]

y_grid <- predict(svm_model1, x_grid)

plot(x_grid, col=c("red", "blue")[as.numeric(y_grid)],
     pch=19, cex=0.2)

points(x, col=y+3, pch=19)
points(x[svm_model1$index, ], pch="I", cex=2)

beta <- drop(t(svm_model1$coefs)%*%x[svm_model1$index,])
b0 <- svm_model1$rho

curve(b0 / beta[2], -beta[1] / beta[2])
curve((b0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
curve((b0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)
#===========================================================#


#svm tahmin bölümü

pred <- predict(svm_model1)
class_error(df$y, svm_model1$fitted)

df$y <- ifelse(df$y==-1, 0,1)
svm_model1$fitted <- ifelse(svm_model1$fitted==-1, 0, 1)

tb <- table(svm_model1$fitted, df$y)
confusionMatrix(tb, positive = "1")
svm_model1$fitted

# SVM DOĞRUSAL OLMAYAN MODELLER

#İLK MODEL 
#======================================================
#load ile yüklenen data R içinden ismiyle çağrılabilir.
load(file = "ESL.mixture.rda")
df <- ESL.mixture
attach(df)
remove(x, y)
plot(x, col=y+1)
df <- data.frame(x=x, y=as.factor(y))

n_svm_fit <- svm(factor(y)~., data = df, scale=F, kernel="radial", cost=5)
svm_fit

x_grid <- expand.grid(X1 = px1, X2 = px2)
y_grid <- predict(n_svm_fit, x_grid)


plot(x_grid, 
     col = as.numeric(y_grid), 
     pch = 20, 
     cex = .2) 

points(x, col = y + 1, pch = 19)

dv <- predict(n_svm_fit, x_grid, decision.values = TRUE)

contour(px1, px2, 
        matrix(attributes(dv)$decision, length(px1), length(px2)), 
        level = 0, 
        add = TRUE)


contour(px1, px2, 
        matrix(attributes(dv)$decision, length(px1), length(px2)), 
        level = 0.5, 
        add = TRUE, 
        col = "blue", 
        lwd = 2)


# İKİNCİ MODEL VE MODEL TUNİNG
#=========================================================
data("segmentationData")
df  <- segmentationData
as.tibble(df)
glimpse(df)
# biz bu veriseti içinde class değişkenini bağımlı değişken olarak alacağız.
table(df$Class)

svm_train <- df %>% filter(Case=="Train") %>% select(-Case)
svm_test <- df %>% filter(Case=="Test") %>% select(-Case)

svm_train_x <- svm_train %>% select(-Class)
svm_test_x <- svm_test %>% select(-Class)

svm_test_y <- svm_test$Class
svm_train_y <- svm_train$Class

#model tune işlemleri için ayarlamalar.

ctrl <- trainControl(method = "cv", summaryFunction = twoClassSummary,
                     classProbs = TRUE)

svm_grid <- expand.grid(sigma=0.08,
                        C = seq(0.001, 0.2, length=10))

svm_tune <- train(svm_train_x, svm_train_y, method="svmRadial",
                   trControl = ctrl,
                   tuneGrid=svm_grid, metric="ROC") 
svm_tune
plot(svm_tune)
confusionMatrix(predict(svm_tune, test_x), svm_test_y, positive = "WS")

y_pred <- predict(svm_tune, svm_test_x, type="raw")

test_prob <- predict(svm_tune, svm_test_x, type = "prob")
test_prob$WS
roc(svm_test_y ~ test_prob$WS, plot = TRUE, print.auc = TRUE)


























