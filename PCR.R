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






















