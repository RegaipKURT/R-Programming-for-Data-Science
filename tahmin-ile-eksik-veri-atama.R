"""
###VERİSETİNDEKİ EKSİK GÖZLEMLERİ DOLDURMANIN YÖNTEMLERİ
1) DOĞRUSAL İNTERPOLASYON YÖNTEMİ: Bir doğru üzerindeki eksik bir noktayı bulmaya çalışır. 
Dolayısıyla formülü nokta formülüne benzerdir. Verisetinde doğrusallık varsa bir başka değişkendeki 
doğrusal artışın kendi değişkenimiz üzerindeki etkisinden faydalanarak eksik değerleri bulabiliriz.

2) MAKSİMUM BEKLENTİ YÖNTEMİ: İlk olarak belirlenen bir ratgele değer üzerinden 
belirlenen hassasiyete ulaşılana kadar, aritmetik ortalamanın rastgele değerden farkı 
yine aritmetik ortalamaya eklenerek devam edilir. Ortalamanın belirlenen değerden farkı 
hassasiyet değerinden küçük olana kadar devam edilir ve belirlenen değere ulaşıldığında 
bütün eksik gözlemler o değerle doldurulur.

3) JACKKNIFE YÖNTEMİ: Maksimum beklentiden farklı olarak bütün eksik değerlerin aynı sonuçla doldurulması 
yerine, her eksik değer için ayrı hesaplama yapılmasıdır. Bir kere hesaplanan değer artık bilinen değer 
olarak kabul edilip hesalamada bu değer bilinenler arsında kullanılır.

4) MODELLEME YÖNTEMİ: Belirli bir model belirlenerek bu model ile eksik değerler tahmin edilir.

5) EKSİK VERİYİ SİLME: Bu yöntemde eksik veriler silinir. Fakat veriseti küçükse bu yöntemi tercih etmek 
verisetini daha da küçülteceğinden modelin başarısını etkileyecektir.

ÖRNEK OLARAK BUNLARI SÖYLEYEBİLİRİZ AMA BİRÇOK BAŞKA YÖNTEM DE BULUNMAKTADIR.
"""
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












