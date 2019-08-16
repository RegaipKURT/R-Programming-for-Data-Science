library(ggplot2)
library(tidyverse)

#ggplot içindeki diamond verisetini kullanalım
veri <- diamonds
glimpse(diamonds)
  
#ggplot'un katmanlı yapısını bilmek grafikleri nasıl çizdiğimizi anlamayı kolaylaştırır.

# Bar Grafikleri
ggplot(veri, aes(cut, fill = color)) +
  geom_bar(position = position_dodge2()) +
  ggtitle("Diamond Dataset") +
  xlab("Elmas Kalitesi") +
  ylab("Gözlenme Sıklığı")

#HİSTOGRAM VE YOĞUNLUK GRAFİKLERİ
#Hangi fiyat aralığında kaç elmas var acaba?
ggplot(veri, aes(price)) + geom_histogram(color = "green", binwidth = 1000)

#Peki yoğunluk grafiği istersek nasıl olacak?
colors()
ggplot(veri, aes(price)) + geom_density(color = "violetred4")

#hem yoğunluk hem histogram istersek
ggplot(veri, aes(price)) +
  geom_histogram(aes(y = ..density..), color = "green", binwidth = 1000) +
  geom_density(color = "magenta",
               fill = "blue",
               alpha = 0.2)

#biraz daha net anlaşılması için;
ggplot(veri, aes(price)) +
  geom_histogram(color = "green", binwidth = 1000) +
  #az önceki gibi fill=color (bar grafikteydi) yerine şöyle de yapabiliriz.
  facet_grid(cut ~ .)
#facet_grid ekleyince her bir kategorinin yoğunluğu ayrı ama aynı ölçekte görülebilir.



#İNTERAKTİF HİSTOGRAM
#Plotly kütüphanesi python da olduğu gibi r için de kullanılabilir.
install.packages("plotly")
library(plotly)

plot_ly(x = rnorm(500),
        type = "histogram",
        color = "red")



#plotly ile pipe kullanılarak üst üste grafikler çizilebilir!!!
# r notebook veya markdown kullanılırken plotly son derece güzel durur bence
g <-
  plot_ly(
    x = rnbinom(500, 50, 0.9),
    type = "histogram",
    name = "binom1",
    opacity = 0.5
  ) %>%
  add_trace(x = rbinom(500, 50, 0.2),
            color = "red",
            name = "binom2") %>%
  add_trace(x = c(1:25, 2),
            type = "scatter",
            name = "noktalar") %>%
  layout(barmode = "overlay")
g


#DAĞILIM-ÇOKLU FREKANS
ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(colour = cut), bindwith = 500)

#Yoğunluk grafiği ile aynı hemen hemen
ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_density(aes(colour = cut))

#BOXPLOT (DAĞILIM GRAFİĞİ)
ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
  geom_boxplot()
#bir sürekli değişkenin bir kategorik değişken ayrımına göre boxplot grafiği


#VIOLIN DAĞILIM GRAFİĞİ
ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
  geom_violin(aes(alpha(0.7)))

# KORELASYONLAR İÇİN GRAFİKLEŞTİRME

#scatterplot

df <- iris
glimpse(iris)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(
    size = 2,
    shape = 21,
    stroke = 1,
    color = "black",
    fill = "green"
  ) +
  #korelasyonu görmek için line ekliyoruz.
  geom_smooth(method = lm, color = "red", se = T)

# Türüne göre her grubu ayrı göstermek istersek:
ggplot(iris,
       aes(
         x = Sepal.Length,
         y = Sepal.Width,
         color = Species,
         shape = Species
       )) +
  geom_point(size = 2)

#farklı bir özelliğe göre şöyle de yapılabilir.
ggplot(iris,
       aes(
         x = Sepal.Length,
         y = Sepal.Width,
         color = Petal.Length,
         size = Petal.Length
       )) +
  geom_point(size = 2)

ggplot(iris,
       aes(
         x = Sepal.Length,
         y = Sepal.Width,
         color = Petal.Length,
         size = Petal.Length
       )) +
  geom_point(size = 2, alpha = 0.6)

# SCATTERPLOT - BİRİMLERİ GRAFİĞE EKLEMEK

df <- mtcars
glimpse(df)
ggplot(df, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point() +
  geom_label(
    label = row.names(df),
    nudge_x = 0.25,
    nudge_y = 0.25,
    check_overlap = T
  ) +
  geom_smooth(method = lm, se = F, color = "red")


# Yukarıda çok fazla şeyi aynı anda grafikleştirdik
#eğer daha fazla grafik üst üste eklemek istersek nasıl olacak?
# bunun için ggextra kütüphanesinden ggMarginal fonksiyonunu kullanabiliriz!
#install.packages("ggExtra")
library(ggExtra)
g <- ggplot(df, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point() +
  geom_label(
    label = row.names(df),
    nudge_x = 0.25,
    nudge_y = 0.25,
  ) +
  geom_smooth(method = lm, se = F, color = "red")

# birinci argüman ilk grafik, ikincisi ise eksenler için hangi tipte olacağı
ggMarginal(g, type = "his")
ggMarginal(g, type = "boxplot", color="yellow4")
# “density”, “histogram”, “boxplot”, “violin” olarak tip belirlenebilir.

# ISI HARİTASI VEYA HEATMAP OLUŞTURMAK
veriseti <- mtcars
veriseti <- as.matrix(veriseti)
heatmap(veriseti)
#böyle birşey anlaşılmıyor ama kolon bazlı gösterilirse daha düzgün olur.
heatmap(veriseti, scale = "column")
#dendogramları devredışı bırakmak için
heatmap(veriseti, scale = "column", Colv = NA, Rowv = NA)


#KORELASYON MATRİSLERİ
install.packages("GGally")
library(GGally)

df <- mtcars[, c(1, 3:6)]
glimpse(df)
cor(df)
cor.test(df$mpg, df$disp)
plot(df)

#korelasyon şiddetini daha iyi görebilmek için ggplot içinden
ggcorr(df)
ggpairs(df)


# ZAMAN SERİSİ GRAFİKLEŞTİRME

df <- economics
glimpse(df)
#tarihin date şeklinde olmasına dikkat edilmeli, değilse dönüştürülmeli

ggplot(df, aes(x = date, y = pop)) +
  geom_line(aes(size=unemploy/pop), color="blue") #işsizlik oranına göre çizgikalınlığı


#başka bir değişkene bakalım
ggplot(df, aes(x = date, y = psavert)) +
  geom_line(color="red") + #çizgi grafiği çizildi zaman göre
  stat_smooth(color="blue") # çizgi grafiği ekleyip ortalamayı çizer


# TREEMAP GRAFİĞİ

install.packages("treemap")
library(treemap)

veri <- data.frame(
  gruplar = c("grup_1", "grup_2", "grup_3"),
  degerler = c(10, 60, 40)
  )
veri

treemap(veri, index = "gruplar", vSize = "degerler", type = "index")

# treemap ile alt gruplar oluşturma















