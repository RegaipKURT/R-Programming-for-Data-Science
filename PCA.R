#HIZLICA UYGULAMAK İÇİN
#veriseti
df <- USArrests

#pca uygulanması
pca <- prcomp(df)
pca$rotation <- -pca$rotation 
pca$x <- -pca$x
#görselleştirilmesi
biplot(pca)

# PCA İÇİN GEREKLİ ADIMLAR
"
1- Veri ön işleme ve Kovaryans Matrislerinin incelenmesi
(Çünkü varyanslar PCA için önemli)

2 - Ölçeklendirme - Scale
3 - Özdeğerler ve Özdeğerler Vektörlerinin OLuşturulması
4 - Temel Bileşen Skorlarının Hesaplanması
5 - Bileşenleri Görselleştirme ve Yorumlama
6 - Bileşen Sayısına Karar vermek
7 - PCA Uygulanması
"

#Ölçeklendirme
df <- USArrests
s_df <- apply(df, 2, scale)

#Özdeğerler ve Özdeğerler Matrislerinin Hesaplanması
cov_df <- cov(s_df)
cov_df

ei_df <- eigen(cov_df)
ei_df 
str(ei_df)
ei_df$values
ei_df$vectors

p <- ei_df$vectors[,1:2]

row.names(p) <- rownames(cov_df)
colnames(p) <- c("PC1","PC2")

# Temel Bileşen Skorlarının Hesaplanması
pc_bir <- as.matrix(s_df) %*% p[,1]
pc_iki <- as.matrix(s_df) %*% p[,2]

PC <- data.frame(Eyaletler=row.names(USArrests), PC1=pc_bir, PC2=pc_iki)

# Bileşenlerin Görselleştirilmesi ve Yorumlanması

ggplot(PC, aes(PC1, PC2)) +
  modelr::geom_ref_line(h=0)+
  modelr::geom_ref_line(v=0)+
  geom_text(aes(label=Eyaletler), size=3)+
  ggtitle("İlk İki Asal Bileşen")+
  xlab("Birinci Asal Bileşen") +
  ylab("İkinci Asal Bileşen")

# Açıklanan Varyans Oranlarına Bakarak Bileşen Sayısına Karar Vermek

avo <- ei_df$values / sum(ei_df$values)
avo
round(avo,2)

#avo ya yani açıklanan varyans oranı baktığımız zaman ilk iki bileşen ile
# verisetindeki varyansın %87 sini açıklayabiliyoruz
# 1. bil = %62, 2. bil = %25

avo_plot <- qplot(c(1:4), avo) + 
  geom_line() + 
  xlab("Temel Bilesen") + 
  ylab("AVO") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

kum_avo <- qplot(c(1:4), cumsum(avo)) + 
  geom_line() + 
  xlab("Temel Bilesen Sayisi") + 
  ggtitle("Kumulatif Yamac Grafigi") +
  ylim(0,1)

library(gridExtra)
grid.arrange(avo_plot, kum_avo, ncol = 2)

#SON ADIM - PCA UYGULAMA - EN BAŞTAKİ KODLAR
pca <- prcomp(df)
pca$rotation <- -pca$rotation 
pca$x <- -pca$x
#görselleştirilmesi
biplot(pca)

