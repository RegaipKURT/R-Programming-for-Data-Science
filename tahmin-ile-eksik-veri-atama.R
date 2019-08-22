library(DMwR)
library(ISLR)


#KNN İLE BOŞ VERİYİ DOLDURMA

df <- Hitters

df[sample(1:nrow(df), 7), "Hits"] <- NA #hits içine 7 tane esksik veri koyduk
df[sample(1:nrow(df), 12), "Runs"] <- NA  #runs içine 12 na koyduk

anyNA(df)

knn_data <- knnImputation(df)
anyNA(knn_data)

f <- sapply(df, FUN = function(x) which(is.na(x)))
#orjinal hitters ı çağırıp içindeki değerlerle karşılaştıralım şimdi
df <- Hitters

knn_hits <-knn_data[knn_data$Hits[c(f$Hits)], ]$Hits
hitters_hits <- df[df$Hits[c(f$Hits)], ]$Hits

summary(knn_hits)
summary(hitters_hits)
#summary ile orjinaldeki ile aradaki farka bakalım

#RANDOM FORESTS İLE BOŞ VERİYİ DOLDURMA

#Gerekli kütüphaneler
install.packages("missForest")
library(missForest)

df <- Hitters

#YİNE HİTTERS İÇİNE BİLEREK EKSİK VERİ DOLDURALIM
df[sample(1:nrow(df), 7), "Hits"] <- NA #hits içine 7 tane esksik veri koyduk
df[sample(1:nrow(df), 12), "Runs"] <- NA  #runs içine 12 na koyduk


rf_data <- missForest(df, ntree = 10)

rftree_data <- rf_data$ximp

l <- sapply(df, FUN = function(x) which(is.na(x)))


#ŞİMDİ DEĞERLENDİRELİM ORJİNAL VERİ İLE KARŞILAŞTIRARAK
hits_org <- Hitters[c(l$Hits), ]$Hits
hits_rf <- rftree_data[c(l$Hits), ]$Hits

runs_org <- Hitters[c(l$Runs), ]$Runs
runs_rf <- rftree_data[c(l$Runs), ]$Runs

summary(hits_org)
summary(hits_rf)
summary(runs_org)
summary(runs_rf)
#GÖRÜLDÜĞÜ GİBİ RANDOM FOREST İLE ORJİNALE ÇOK YAKIN DEĞERLERE ULAŞTIK.












