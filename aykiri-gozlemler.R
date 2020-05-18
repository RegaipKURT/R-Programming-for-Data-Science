#AYKIRI GÖZLEMLER
#AYKIRI GÖZLEMLER 20 YIL ÖNCE VERİSETİNİ BOZAN DEĞERLER OLARAK GÖRÜLÜP SİLİNİRKEN,
#ARTIK VERİNİN EN DEĞERLİ KISMI OLARAK GÖRÜLÜYOR. 
#ÖZELLİKLE SİBER GÜVENLİK, HİLELİ İŞLEMLER, SAHTECİLİK GİBİ KONULARDA AYKIRI GÖZLEMLER
#ALTIN DEĞERİNDEKİ VERİYİ BARINDIRIRLAR. ÇÜNKÜ BU KONULARDA ULAŞMAK İSTEDİĞİMİZ ASIL ŞEY
#TAM DA BU AYKIRILIK İLE İLİŞKİLİDİR. 
#YANİ AYKIRI GÖZLEMLER DOLDURULMAK, TAHMİN EDİLMEK VEYA SİLİNMEK ZORUNDA DEĞİLDİR.

            ### İSTATİSTİK AYKIRI OLANI BULMAKLA İLGİLENİR. ###
       ### YOKSA ORTALAMAYI BULMANIN KİMSEYE BİR FAYDASI YOKTUR. ###

set.seed(54)
veri <- rnorm(100)
summary(veri)
sd(veri)
veri <- c(veri, c(4,5,6))
summary(veri)
sd(veri)
boxplot(veri)
boxplot.stats(veri)$out #aykırı gözlemlerin değerleri

which(veri %in% boxplot.stats(veri)$out)
#boxplotstats ile alınan değerlerin indekslerini getir dedik.


#ŞİMDİ İKİ GÖZLEMİMİZ OLSUN VE BU İKİ GÖZLEMİN KESİŞİMİNDEKİ AYKIRI GÖZLEMLERE
#ERİŞMEK İSTEDİĞİMİZİ DÜŞÜNELİM

set.seed(54)
x <- rnorm(100)
x <- c(x, c(4,5,6))

set.seed(455)
y <- rnorm(100)
y <- c(y, c(10,11,12)) #yine aynı indeslere aykırı gözlemler ekledik 

df <- data.frame(x,y)
df

a <- which(df$x %in% boxplot.stats(df$x)$out)
b <- which(df$y %in% boxplot.stats(df$y)$out)
a
b

intersect(boxplot.stats(df$x)$out, boxplot.stats(df$y)$out) #kesişen değerler var mı bakalım
#burada bizim değerlerimiz farklı (4,5,6) ve (10,11,12). O yüzden göstermedi
ortak <- intersect(a,b) # bu şekilde ise kesişen aykırı değerlerin indekslerine ulaşabiliriz.
ortak
plot(df)
#aykırı gözlemler plot fonksiyonunda da görülüyor zaten

points(df[ortak, ], col="red", pch="x", cex=2)

ortak_tum <- union(a,b) #ikisinin birleşimini gösterir.
ortak_tum
length(ortak_tum)

plot(df)
# şimdi bütün aykırı gözlermleri çizdirelim
points(df[ortak_tum, ], col="blue", pch="+", cex=2)

#AYKIRI GÖZLEMLERİ SİLME
str(df)
df_temiz <- df[-ortak_tum, ]
str(df_temiz) #gördüğümüz gibi 103 değişkenden 97 ye düştü ve 6 aykırı gözlem silindi

#AYKIRI GÖZLEMLERİ ORTALAMA İLE DOLDURMA
df
a <- which(df$x %in% boxplot.stats(df$x)$out)
b <- which(df$y %in% boxplot.stats(df$y)$out)

df[a, ]$x
df[b, ]$y

mean(df$x)
mean(df$y)

df[a, ]$x <- mean(df$x)
df[b, ]$y <- mean(df$y)

df[a, ]$x

#AYKIRI GÖZLEMLERİN BASKILANMASI
set.seed(54)
x <- rnorm(100)
x <- c(x, c(4,5,6))

set.seed(455)
y <- rnorm(100)
y <- c(y, c(10,11,12)) #yine aynı indeslere aykırı gözlemler ekledik 

df <- data.frame(x,y)
df

a <- which(df$x %in% boxplot.stats(df$x)$out)
b <- which(df$y %in% boxplot.stats(df$y)$out)

#ŞİMDİ AYKIRI GÖZLEMLERİ 3. ÇEYREK DEĞERİ İLE DOLDURUP BASKILAYALIM
summary(df) #summary fonksiyonunun 5. elemanı 3. çeyrek değeri oluyor
summary(df)[5]
#fakat summary yazıyı da (3rd Qu: 0.641) içeriyor
#o yüzden beşli istatistiği direk sayı olarak veren fivenum ı kullanacağız
df[a, ]$x <- fivenum(df$x)[4]
summary(df)
df[a, ]$x
#hepsini 3. çeyrek değeri ile doldurmuşuz
#ama ortalamadan küçük olanları 1. çeyrek, büyük olanları 3. çeyrek ile doldurmak daha doğru

# aynı işlemi y değişkeni için de yapalım ama 
# bu sefer x değişkeni tarafı dolu olduğuna göre y değişkenini tahmin ederek dolduralım

# AYKIRI GÖZLEMİ TAHMİNLE DOLDURMA

df[b, ]$y
#tahmin ile doldurmayı eksik gözlemlerle yapabiliyoruz dolayısıyla 
#bu aykırı değerleri na ile doldurup sonra tahmin edeceğiz.
df[b, ]$y <- NA
df[a, ]$x

#tahmin yapabilmek için veriyi genişletelim başka kolonlarla
df <- data.frame(df, z=rnorm(103), t=rnorm(103), k = rnorm(103))
df
#ŞİMDİ RANDOM FORESTS İLE TAHMİN EDİP DOLDURALIM
library(missForest)
rf_data <- missForest(df)
rf_data <- rf_data$ximp
rf_data[b, ]$y

df[b, ]$y

#b,r de knn ile deneyelim doldurmayı
library(DMwR)
knn_data <- knnImputation(df)
knn_data[b, ]$y

#karşılaştıalım iki algoritmayı
summary(knn_data[b, ]$y)
summary(rf_data[b, ]$y)
  
plot(df$y, col="green")
points(rf_data[b, ]$y, col="red", pch="r", cex=0.8)
points(knn_data[b, ]$y, col="blue", pch="k", cex=0.8)
#karar veremedim hangisi iyidir.


# LOCAL OUTLİER FACTOR

#BİR NOKTANIN BULUNDUĞU YERİN YOĞUNLUĞUNUN KOMŞULARIN YOĞUNLUĞU İLE KARŞILAŞTIRILMASI

df <- iris
str(df)
summary(df)

#kategporik değişkeni almayacağız, 
#çünkü kullanacağımız algoritma sadece numerik değişkenlerle çalışıyor
df <- df[,1:4]

aykiri_skor <- lofactor(df, k = 5)

plot(density(aykiri_skor))

#şimdi skoru sralayalım
order(aykiri_skor, decreasing = T)[1:5]
#aykırı skor içindeki indeskler bunlar. şimdi bu indekslerdeki değerlere erişelim

indeksler <- order(aykiri_skor, decreasing = T)[1:5]
df[indeksler, ]

#AYKIRI GÖZLEMLERİ GÖRSELLEŞTİRMEK

df <- iris[,1:4]
n <- nrow(df)
etiektler <- 1:n

aykirilar <- order(aykiri_skor, decreasing = T)[1:5]
etiektler[-aykirilar] <- "."

biplot(prcomp(df), cex=1, xlabs = etiektler)


pch <- rep(".", n) 
pch
pch[aykirilar] <- "+"
pch
col <- rep("black", n)
col
col[aykirilar] <- "red"

pairs(df, pch = pch, col = col)

#aykırı gözlemleri kümeleme yöntemiyle doldurmak

df <- iris[,1:4]
k_ort <- kmeans(df, centers = 3)
plot(df, col=k_ort$cluster)

k_ort$centers #mkümelerin merkez noktalarının değerleri
k_ort$cluster #kümelerin numarası

merkez_df <- k_ort$centers[k_ort$cluster, ]
uzakliklar <- sqrt(rowSums(df, merkez_df)^2)

aykirilar <- order(uzakliklar, decreasing = T)[1:10]
print(df[aykirilar, ])

plot(df[c("Sepal.Length", "Sepal.Width")],
     pch=".", col=k_ort$cluster,
     cex=6)

points(k_ort$centers[, c("Sepal.Length", "Sepal.Width")],
     pch=8, col=1:3,
     cex=3)
points(df[aykirilar, c("Sepal.Length", "Sepal.Width")], 
       pch="+",
       cex=3,
       col="blue"
       )
