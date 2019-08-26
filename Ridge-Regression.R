library(ISLR)
library(caret)
library(dplyr)
library(ggplot2)
library(funModeling)
library(PerformanceAnalytics)
library(pls)
df <- Hitters
df <- na.omit(df)
rownames(df) <- c()

train_indeks <- createDataPartition(df$Salary, p = 0.8, times = 1)
head(train_indeks)
train <- df[train_indeks$Resample1, ]
test <- df[-train_indeks$Resample1, ]

train_x <- train %>% dplyr::select(-Salary)
train_y <- train %>% dplyr::select(Salary)
test_x <- test %>% dplyr::select(-Salary)
test_y <- test %>% dplyr::select(Salary)

training <- data.frame(train_x, Salary = train_y)

plot_num(training)
summary(training)
profiling_num(training)
chart.Correlation(df %>% dplyr::select(-c("League", "NewLeague", "Division")))

#model kurulması

lm_fit <- lm(Salary~., data = training)
summary(lm_fit)

# model nesnesi içinden alabileceğimiz değerleri görmek için şunları kullanabiliriz
names(lm_fit)
attributes(lm_fit)

sonuc <- data.frame(obs=training$Salary, pred=lm_fit$fitted.values)
defaultSummary(sonuc)

pred = predict(lm_fit, test_x)
sonuc_test <- data.frame(obs=test_y$Salary , pred=pred)

defaultSummary(sonuc_test)

# model_validation

kontrol <- trainControl(method = "cv", number = 10)

#bu şekilde herhangi bir fonksiyonun bütün parametrelerini görebiliriz.
names(trainControl())
lm_val_fit <-
  train(
    x = train_x,
    y = train_y$Salary,
    method = "lm",
    trControl = kontrol
  )

lm_val_fit
summary(lm_val_fit)
names(lm_val_fit)
lm_val_fit$bestTune
lm_val_fit$finalModel


#principle component regression - PCR

pcr_fit <- pcr(Salary~., data=training, scale=T, validation="CV")
summary(pcr_fit)
validationplot(pcr_fit)
names(pcr_fit)
defaultSummary(data.frame(obs=training$Salary, 
                          pred= as.vector(pcr_fit$fitted.values)))

predict(pcr_fit, test_x)

defaultSummary(data.frame(obs = training$Salary,
                          pred = as.vector(predict(
                            pcr_fit, test_x, ncomp = 1:3
                          ))))

# öbyle bir döngü ilew kaç bileşende ne kadar etki olduğunu görebiliriz.
for (i in c(1:19)) {
  print(as.character(i))
  
  print(defaultSummary(data.frame(obs = test_y$Salary,
                                  pred = as.vector(predict(
                                    pcr_fit, test_x, ncomp = i
                                  )))))
  i = i + 1
}


#ben sürekli yukarıdaki gibi bir döngüyle uğraşmak istemediğim için
# bir fonksiyon yazıp daha sonra da gerektiğinde kullanmak istiyorum

pcr_tune <- function(model, x, y) {
  num_of_iter=length(x)
  for (i in c(1:num_of_iter)) {
    cat("COMPONENTS:", i)
    cat("\n")
    print(defaultSummary(data.frame(obs = y[,1],
                                    pred = as.vector(predict(
                                      model, x, ncomp = i
                                    )))))
    cat("\n")
    i = i + 1
    if (i=num_of_iter) {
      remove(i)
      
    }
  }
}

ayarlar <- pcr_tune(model=pcr_fit, x=test_x, y = test_y)
#yazdığmız fonksiyon düzgün çalışıyor

kontrol <- trainControl(method = "CV", number = 10)
set.seed(100)
pcr_ayar <- train(train_x, train_y$Salary, 
                  method="pcr",
                  trContol = kontrol,
                  tuneLength = 20,
                  preProc = c("center", "scale")
                  )
pcr_ayar
pcr_ayar$finalModel

plot(pcr_ayar)

# PLS - PARTIAL LEAST SQUARES
pls_fit <- plsr(Salary~., data=training)
summary(pls_fit)
validationplot(pls_fit, val.type = "MSEP")

defaultSummary(data.frame(obs=test_y$Salary, pred=as.vector(
  predict(pls_fit, test_x)
)))

kontrol <- trainControl(method = "CV", number = 10)
set.seed(100)
pls_ayar <- train(train_x, train_y$Salary, 
                  method="pls",
                  trContol = kontrol,
                  tuneLength = 20,
                  preProc = c("center", "scale")
)
pls_ayar
pls_ayar$finalModel
  
plot(pls_ayar)


# RİDGE REGRESSİON - L2 REGULARİZATİON
train_x_x <- train_x %>% dplyr::select(-c("League", "NewLeague", "Division"))

library(glmnet)
ridge_fit <- glmnet(as.matrix(train_x_x), train_y$Salary,
                    alpha = 0)
ridge_fit 

plot(ridge_fit, xvar = "lambda", label = TRUE)
min(log(ridge_fit$lambda))
#minimum lambda değerini gösterdik

ridge_cv_fit <- cv.glmnet(as.matrix(train_x_x), train_y$Salary,
                    alpha = 0)
ridge_cv_fit
plot(ridge_cv_fit)
ridge_cv_fit$lambda.min
ridge_cv_fit$lambda.1se
coef(ridge_cv_fit, "lambda.min")
coef(ridge_cv_fit) #default olarak 1s lambdasına karşılık gelen katsayıları verir
library(broom)
tidy(ridge_cv_fit)

test_x_x <- test_x %>% dplyr::select(-c("League", "NewLeague", "Division"))

defaultSummary(data.frame(obs=test_y$Salary, pred=as.vector(
  predict(ridge_cv_fit, as.matrix(test_x_x), s = "lambda.min")
)))

kontrol <- trainControl(method = "CV", number = 10)
set.seed(100)

ridge_grid <- data.frame(
  lambda = seq(1,10000, length=20)
)
help("seq")
ridge_tune <- train(train_x_x, train_y$Salary, 
                  method="ridge",
                  trContol = kontrol,
                  tuneGrid = ridge_grid,
                  preProc = c("center", "scale")
)
ridge_tune

plot(ridge_tune)

ridge_tune$results %>% filter(lambda == as.numeric(ridge_tune$bestTune))


defaultSummary(data.frame(obs=test_y$Salary, pred=as.vector(
  predict(ridge_tune, as.matrix(test_x_x))
)))

# LASSO REGRESYON - L1 REGULARİZATİON
train_x_x <- train_x %>% dplyr::select(-c("League", "NewLeague", "Division"))

lasso_fit <- glmnet(as.matrix(train_x_x), train_y$Salary,
                    alpha = 1) #ALPHA =1 LASSO or L1 - ALPHA = 0 RİDGE or L2
lasso_fit 

plot(lasso_fit, xvar = "lambda", label = TRUE)
names(lasso_fit)
tidy(lasso_fit$beta)
lasso_fit$beta

lasso_cv_fit <- cv.glmnet(as.matrix(train_x_x), train_y$Salary,
                          alpha = 1)
lasso_cv_fit
plot(lasso_cv_fit)
lasso_cv_fit$lambda.min
lasso_cv_fit$lambda.1se
coef(lasso_cv_fit, "lambda.min")
coef(lasso_cv_fit)
glance(lasso_cv_fit)

test_x_x <- test_x %>% dplyr::select(-c("League", "NewLeague", "Division"))

defaultSummary(data.frame(obs=test_y$Salary, pred=as.vector(
  predict(lasso_cv_fit, as.matrix(test_x_x), s = "lambda.min")
)))

ctrl <- trainControl(method = "cv", number = 10)
set.seed(100)

lasso_grid <- data.frame(
  fraction = seq(.05,100, length=50)
)
lasso_grid
help("train")

lasso_tune <- caret::train(train_x_x, train_y$Salary, 
                    method="lasso",
                    trContol = ctrl,
                    tuneGrid = lasso_grid,
                    preProc = c("center", "scale")
)
lasso_tune

plot(lasso_tune)

lasso_tune$results %>% filter(lambda == as.numeric(lasso_tune$bestTune))


# ELASTİKNET -ENET - LASSO VE RİDGE'İ İÇERİR.
# DEĞİŞKEN SEÇİMİNİ L2 (LASSO), CEZALANDIRMAYI L1'E (RİDGE) GÖRE YAPAR.

library(lars)
library(elasticnet)

enet_fit <- enet(x = as.matrix(train_x_x), y = train_y$Salary,
     lambda = 1, normalize = TRUE
     )
names(enet_fit)
enet_fit$L1norm
enet_fit$lambda
enet_fit$beta.pure

plot(enet_fit)

predict(enet_fit, newx = as.matrix(test_x_x), s = 1, mode = "fraction",
        type = "fit")
predict(enet_fit, newx = as.matrix(test_x_x), s = 0.1, mode = "fraction",
        type = "coefficients")

ctrl <- trainControl(method = "cv", number = 10)
set.seed(100)


enet_grid <- 
  data.frame(lambda=seq(0,0.1, length=20), 
             fraction=seq(0.05,1,length=20))


enet_grid

enet_tune <- caret::train(train_x_x, train_y$Salary, 
                           method="enet",
                           trContol = ctrl,
                           tuneGrid = enet_grid,
                           preProc = c("center", "scale")
)
enet_tune
enet_tune$bestTune
plot(enet_tune)

defaultSummary(data.frame(obs=test_y$Salary, pred=as.vector(
  predict(enet_tune, as.matrix(test_x_x))
)))













