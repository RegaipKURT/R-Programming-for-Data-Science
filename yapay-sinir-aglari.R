# YAPAY SİNİR AĞLARI

### İlk önce bütün girdileri temizleyelim
rm(list = ls())

## Kütüphaneler
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

## Verisetimizi alalım

# http://archive.ics.uci.edu/ml/datasets/Yacht+Hydrodynamics


dff <- read_table(
  file = 'http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data',
  col_names = c(
    'longpos_cob',
    'prismatic_coeff',
    'len_disp_ratio',
    'beam_draut_ratio',
    'length_beam_ratio',
    'froude_num',
    'residuary_resist'
  )
) 

glimpse(dff)
summary(dff)
profiling_num(dff)
ggpairs(dff)
chart.Correlation(dff, histogram = T, pch=19)

olcekleme <- function(x) {
  (x - min(x)) / (max(x)-min(x))
}

dff <- na.omit(dff)

sapply(dff, FUN = olcekleme)


train_indeks <- createDataPartition(dff$residuary_resist, p = 0.8, times = 1)
head(train_indeks)
train <- dff[train_indeks$Resample1, ]
test <- dff[-train_indeks$Resample1, ]

train_x <- train %>% dplyr::select(-residuary_resist)
train_y <- train %>% dplyr::select(residuary_resist)
test_x <- test %>% dplyr::select(-residuary_resist)
test_y <- test %>% dplyr::select(residuary_resist)

training <- data.frame(train_x, residuary_resist = train_y)

names(training)

# neuralnet kullanacağız ve bunun için formülün açıkça girilmesi gerekiyor.
ysa_formul <- residuary_resist ~ longpos_cob + prismatic_coeff + len_disp_ratio +   
  beam_draut_ratio + length_beam_ratio + froude_num       
#bağımlı ve bağımsız değilkenlerle oluşan formülü yazdık

neuralnet(formula = ysa_formul, data = training)

plot(neuralnet(formula = ysa_formul, data = training, hidden = c(2,1), stepmax = 100), 
     rep="best")

mynn <-
  nnet(
    residuary_resist ~ longpos_cob + prismatic_coeff + len_disp_ratio + beam_draut_ratio + length_beam_ratio + froude_num,
    data = training,
    size = 2,
    decay = 1.0e-5,
    maxit = 5000
  )


ysa_formul <- residuary_resist ~ longpos_cob + prismatic_coeff + len_disp_ratio + beam_draut_ratio + length_beam_ratio + froude_num

ysa1 <- neuralnet(ysa_formul, data = training)


plot(ysa1)

ysa1$result.matrix











