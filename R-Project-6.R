#TAB tuşuna basarak kod tamamlayıcı kullanılabilir.
#R Grafikler (Stacked Barchart, Mosaic Plot, Plot)

barplot(iris$Sepal.Length)

tablo = table(iris$Species, iris$Sepal.Length<5.5)

#sepal length değerinin 5.5 ten küçük veya büyük olmasına göre yaprak tiplerini ayırdık
#true olanlar küçük false olanlar büyük 5.5 ten
tablo

barplot(tablo, beside = TRUE, legend.text = TRUE) #barplot u şimdi tabloyla çizelim
#besaide yan yana çizer. yazmazsak üst üste çizecektir.

box(which = "plot") # grafiği kutu içine aldık

mosaicplot(tablo) # aşağı yukarı aynı ama daha farklı bir görünüm ile

cor(iris$Sepal.Length, iris$Sepal.Width) #korelasyon hesaplama
#DİKKAT! Eğer veri setinde eksik veya N/A değerler varsa hesaplamaz.


cor(iris$Sepal.Length, iris$Sepal.Width, use = "complete.obs")
#use ile complete observations yani tamamlanmış gözlemleri kullandık
#veri setinde eksik veri olması durumunda use ile düzeltilebilir bu durum
help("cor") # kullanılacak diğer ayarlara bakabiliriz.
#korelasyon değeri 1 veya -1'e yakınsa kuvvetli korelasyon var demektir
#korelasyon = değişim katsayısı veya ilişkililik diyebiliriz.
plot(iris$Sepal.Length, iris$Petal.Length, pch=2, col="blue") #pch işaretleme tipini değiştirir.
#bu grafikte yüksek bir ilişki olduğu görünüyor. şimdi cor ile bakalım var mı diye.
cor(iris$Sepal.Length, iris$Petal.Length)
#görüldüğü gibi 0.8 gibi yüksek bir oran çıktı
plot(iris$Sepal.Length, iris$Petal.Length, pch=2, col="blue", main="İris Grafiği" , cex=1.5, cex.axis=2, cex.lab=1.5, cex.main=1.5)
#cex default 1 değerindedir ve işaretleri büyütür ve küçültür.
plot(iris$Sepal.Length, iris$Petal.Length, col=2, col.axis=3, col.lab=4)
#col.isim ile color değerleri değiştirilebilir. 
plot(iris$Sepal.Length, iris$Petal.Length, pch=2,font=2, font.axis=3, font.lab=1)
#font ile yazı tipine ait özelliklker belirlenebilir
summary(iris) # iris datasının istatistiksel özellikleri
plot(iris$Sepal.Length[iris$Species=="virginica"], iris$Petal.Length[iris$Species=="virginica"], pch="v", col="red")
#sepal length ve petal lenth grafiğini sadece virginica türleri için çizdirdik
points(iris$Sepal.Length[iris$Species=="versicolor"], iris$Petal.Length[iris$Species=="versicolor"], pch="s", col="blue", )
#points aynı grafiğin üstüne başka bir şey daha çizdirmek istersek kullanılır

par(mfrow=c(1,2))  #grafiği 1satır ve 2 kolon olmak üzere iki bölgeye böler
# grafiklerimizi par komutunun ardından çizdirmemiz gerekiyor
plot(iris$Sepal.Length[iris$Species=="virginica"], iris$Petal.Length[iris$Species=="virginica"], pch="v", col="red")
plot(iris$Sepal.Length[iris$Species=="versicolor"], iris$Petal.Length[iris$Species=="versicolor"], pch="s", col="blue", )
#böylece 2 bölgeye farklı grafikler çizebiliriz.
par(mfrow=c(1,1))

summary(iris)
plot(iris$Sepal.Length, iris$Petal.Length, axes=F) #axesleri false yaptık yani çizmedik
#şimdi axis leri kendimiz dolduralım istediğimiz gibi
axis(side=1, at=c(4.300, 5.843, 7.900), labels =c("min", "mean","max"))
axis(side=2, at=c(1.000, 3.758, 6900), labels = c("min", "mean", "max"))
axis(side=3, at=c(5.100, 5.800, 6.400), labels=c("1.qrt", "median", "3.qrt"), col="red")

abline(lm(iris$Sepal.Length~iris$Petal.Length), col="green")
#abline ile eksenlerdeki veriler arası ilişkiyi çizdirdik grafiğimizin üstüne
