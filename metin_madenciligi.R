library(tidyverse)

# Kullanacağımız kitap sherlock holmes
# Adresi: http://www.gutenberg.org/ebooks/1661

sherlock <- readLines("sherlock.txt")

save(sherlock, file = "sherlock.rda") #dosyayı dizine kaydetme.
rm(sherlock)
load(file = "sherlock.rda")
head(sherlock)
length(sherlock)
tail(sherlock)

sherlock <- sherlock[-(1:61)]
#bu indeks düştükten sonra geriye kalanlardan bakıp sondan gereksiz olanları çıkaralım
sherlock <- sherlock[-(12621:12992)]

head(sherlock)

sherlock <- paste(sherlock, collapse = " ")
A <- strsplit(sherlock, ">>")
A <- unlist(A)
A <- A[-1]

install.packages("tm")
library(tm)
class(A)
dokuman_vektoru <- VectorSource(A)
class(dokuman_vektoru)
attributes(dokuman_vektoru)
dokuman_derlem <- VCorpus(dokuman_vektoru)
class(dokuman_derlem)
attributes(dokuman_derlem)

#harfleri küçültme
dokuman_derlem <- tm_map(dokuman_derlem, content_transformer(tolower))
#noktalama işaretlerinin silinmesi
dokuman_derlem <- tm_map(dokuman_derlem, content_transformer(removePunctuation))
#sayıları kaldırma
dokuman_derlem <- tm_map(dokuman_derlem, content_transformer(removeNumbers))
dokuman_derlem <- tm_map(dokuman_derlem, removeWords, stopwords("english"))

# stemming (kelime köklerine inme işlemi)

install.packages("SnowballC")
library(SnowballC)
dokuman_derlem <- tm_map(dokuman_derlem, stemDocument)
dokuman_derlem <- tm_map(dokuman_derlem, stripWhitespace)

#terim belge matrisi oluşturma

tdm <- TermDocumentMatrix(dokuman_derlem)
inspect(tdm[1:4,1:12])
tdm$nrow
tdm$ncol
tdm$dimnames$Docs
#buradaki isimleri hikaye başlıkları yapalım ranlar yerine
# sınıflandırma bölümünde lazım olacak
tdm$dimnames$Docs <- c(
  "I. A Scandal in Bohemia",
  "II. The Red-headed League",
  "III. A Case of Identity",
  "IV. The Boscombe Valley Mystery",
  "V. The Five Orange Pips",
  "VI. The Man with the Twisted Lip",
  "VII. The Adventure of the Blue Carbuncle",
  "VIII. The Adventure of the Speckled Band",
  "IX. The Adventure of the Engineer's Thumb",
   "X. The Adventure of the Noble Bachelor",
  "XI. The Adventure of the Beryl Coronet",
 "XII. The Adventure of the Copper Beeches"
)
tdm$dimnames$Docs

# şimdi bu metinde en az 100 defa geçen kelimeleri bulalım
findFreqTerms(tdm, lowfreq = 100)

#kelimeleri ilişkilendirmye yarayan bir fınksiyonumuz var tm kütüphanesinde
# şimdi watsonla ilgili kelimelere bakalım, belki watson'ın karakteri hakkında bilgi verir.
findAssocs(tdm, "watson", corlimit = 0.80) 
#corlimit, korelasyon oranıdır.
#şimdi ev sahibi olan hudson ile ilgili kelimeleri bulalım
findAssocs(tdm, "hudson", corlimit = 0.85)
#birlikte arama da yapabiliriz.

findAssocs(tdm, c("love","women", "think"), corlimit = c(0.80, 0.8, 0.85))

sum(tdm[1, 1:10])
inspect(tdm[1, 1:10])


#şimdi tdm matrisinin seyreklğini ortadan kaldırarak daha sonraki hesaplamaları kolaylaştıralım
tdm_seyrek <- removeSparseTerms(tdm, 0.1)
tdm
tdm_seyrek
# seyreklik görüleceği üzere %75'den %3'e düştü
#kelimelerin ne kadar azaldığına ise şöyle bakabiliriz.
length(tdm$dimnames$Terms)
length(tdm_seyrek$dimnames$Terms)

#ama en fazla geçen kelimeler kalıyor metinde. şöyle görebiliriz.
length(findFreqTerms(tdm, lowfreq = 100))
length(findFreqTerms(tdm_seyrek, lowfreq = 100))
#gördüğümüz gibi 100 'den fazla geçen kelimelerin sayısı aynı

length(findFreqTerms(tdm, lowfreq = 10))
length(findFreqTerms(tdm_seyrek, lowfreq = 10))
#ama 10'dan fazla geçen kelimelerin sayısı 1007'den 329 a düşmüş


# METİNLERDE VERİ GÖRSELLEŞTİRME
matris <- as.matrix(tdm_seyrek)

frekanslar <- rowSums(matris)
head(frekanslar)
isimler <- rownames(matris)
head(isimler)
frekanslar["watson"]
frekanslar["sherlock"]
hist(frekanslar, col = "blue")
frekanslar2 <- frekanslar[frekanslar>100]
barplot(frekanslar2)

# kelime blutu oluşturma
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("RColorBrewer")
library(wordcloud2)
library(RColorBrewer)
library(wordcloud)
wordcloud(rownames(matris), rowSums(matris))

#daha güzel olması için
palet <- brewer.pal(9, "BuGn")[-c(1:4)]
veri <- data.frame(frekanslar = rowSums(matris))
veri <- data.frame(words=rownames(veri), frekans=veri$frekanslar)
wordcloud2(data = veri, rotateRatio = 1)

# ISI HARİTASI OLUŞTURMA
install.packages("slam")
install.packages("reshape2")
library(slam)
library(reshape2)
tdm_yogunluk <- melt(matris, value.name = "count")
library(ggplot2)


#seyreklik matris igüzel görünsün diye sayıyı azaltalım
yogunluk2 <- removeSparseTerms(tdm, 0.001)
yogunluk2 <- as.matrix(yogunluk2)
isimler <- subset(rowSums(yogunluk2), rowSums(yogunluk2)>200)
isimler <- names(isimler)
matris <- matris[c(isimler), ]
tdm_yogunluk2 <- melt(matris, value.name = "count")

ggplot(tdm_yogunluk2, aes(Docs, Terms, fill=log10(count))) +
  geom_tile(colour="white") +
  scale_fill_gradient(high = "#FF0000", low = "#FFFFFF")+
  ylab("")+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# KÜMELEME ANALİZİ UYGULANMASI
dtm <- DocumentTermMatrix(dokuman_derlem)
dtm_yogun <- removeSparseTerms(dtm, 0.1)
frekanslar <- as.matrix(dtm_yogun)
satir_toplam <- rowSums(frekanslar)
olcek <- frekanslar /satir_toplam

uzaklık <- dist(olcek)
library(cluster)
kume <- agnes(x = uzaklık, method = "ward", metric = "euclidean")
plot(kume)

library(mclust)
model_kume <- mclustICL(uzaklık)
plot(model_kume)
attributes(model_kume)
model_mclust <- Mclust(data = uzaklık)

plot(model_mclust$classification)
plot(model_mclust$BIC)
model_mclust$modelName
summary(model_mclust)
