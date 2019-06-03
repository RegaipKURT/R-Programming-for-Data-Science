veri = iris # iris veri setini yükledik. #datasets i import etmeden yüklemez.
y = veri[5] #verinin 5. kolunu
x = veri[1:4] # verinin 1. ve 4. kolonları arası

names(veri) # verinin başlıkları
mean(veri$Sepal.Length) #veri den sonra dolar koyarak sütunun ortalaması alınabilir.

attach(iris) #iris verisini R içine yükledik direk erişim için
Sepal.Length #attach yaptıktan sonra direk veri sütununu çağırabiliyoruz.
detach(iris) # hafızadan iris verisini sildik. attach büyük verilerde çok hafıza kullanır
class(iris$Sepal.Width) # veideki sütunu tipini gösterir.
class(iris$Species)

dim(iris) # iris verisetinin boyutu

summary(iris) # veriseti ile ilgili istatistiki bilgileri gösterir(her sütun için)

iris$Sepal.Length[iris$Species == "setosa"]#ne yaptık altta açıklayalım

#iris'in sepal length değerlerini yazdır ama bu değerler species kolonu setosa olanlar olsun sadece
#daha farklı yapıp anlayalım

iris$Petal.Width[iris$Sepal.Length>=5.2]
#iris in petal width kolonunu al ama buna karşılık gelen sepal length 5.2 den küçük olsun

setos_low = iris$Sepal.Length[iris$Species=="setosa" & iris$Sepal.Length<=5.2]
setos_low
setos_high = iris$Sepal.Length[iris$Species=="virginica" & iris$Sepal.Length>=6.0]
setos_high

data1 = iris[iris["Species"] =="versicolor" & iris$Petal.Length>5.0,]
#irisin species i veriscolor ve sepal length i 5 den büyük olan değerlerini aldık.
data1
versi = iris[iris["Species"] == "versicolor",] #versicolor olanları seçtik
#sondaki virgül bütün veriyi almamızı sağlar. yani uyuşan tüm satır ve kolonlar
versi

#veriyi faktör verisine veya numerik veriye dönüştürme
as.factor(iris$Species)
as.factor(iris$Sepal.Length)
summary(iris)
class(iris$Sepal.Length)
class(iris$Species)
iris$Species = as.numeric(iris$Species)
class(iris$Species)
summary(iris)
iris$Species = as.factor(iris$Species)
#veri tipinin değiştiğini görebiliriz. bu özellik veri biliminde sıkça kullanılır.
#sınıflandırma gibi problemlerde lazım olacaktır.

#veriye bir kolon ekleme
#örneğin species versicolor ve sepal length > 5 ise 
#oluşan true false kolonunu iris içine ekleyelim

iris = cbind(iris, iris$Species==2 & iris$Sepal.Length>5.1)

#VERILERI CALISMA ORTAMINDA GORMEK ISTERSENIZ ASAGIDAKI KODU SILINIZ..
rm(list = ls()) # ls ile dönen bütün listeyi sil
#yukarıdaki konutla kısa yoldan bütün çalışma alanını temizledik.
