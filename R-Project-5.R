# R Paketler ve Grafikler 

installed.packages() # sistemde yüklü olan paketleri gösterir.

require("readxl") # hata verecek çünkü paket yüklü değil
#require() pythondaki import veya c# daki using gibi dışardan modülleri yükler.

install.packages("readxl") #internetten bu paketi yükleyecek
#excel ile ilgili işlem yapmak için bir pakettir readxl

require("readxl") #şimdi paketi sorunsuz import edecek. (Yüklemesi uzun sürebilir.)

veri = iris #iris verisini kullanacağız ve veri değişkenine atadık
#yan tarafta packages içinde datasets e tıklayıp başka verileri de kullanabilirsiniz

head(veri) #verinin baş kısmını göster

barplot(veri$Sepal.Length) # sepal length in barplot grafiği

table(veri$Species) # özelliklerin tablo halinde dağılımı (grafik değil)

barplot(table(veri$Species)) # şimdi grafik şeklinde dağılımı gösterecek

dagilim = table(veri$Species) # dagilim değişkenine dağılımları atadık
dagilim #şimdi bu değişkenle dağılımı gösterelim

yuzdeler = table(veri$Species) / 150 #yüzde olarak görmek için toplam sayıya böldük.
barplot(yuzdeler)

#barplot grafiğinin özelleştirilmesine bakalım

barplot(table(veri$Species)/150, xlab = "Yaprak Türü", ylab = "Yüzde oranı", main = "YÜZDE DAĞILIM GRAFİĞİ", names.arg=c("1. Yaprak Türü", "2. Yaprak Türü", "3. Yaprak Türü"), col = c("blue", "red", "yellow"))
# bazı özellikleri c() fonksiyonu ile liste halinde verdiğimize dikkat edelim
help("barplot") #kullanabileceğimiz özellikleri görelim help ile

pie(table(veri$Species), xlab = "Yaprak Türü", ylab = "Yüzde oranı", main = "YÜZDE DAĞILIM GRAFİĞİ", names.arg=c("1. Yaprak Türü", "2. Yaprak Türü", "3. Yaprak Türü"), col = c("blue", "red", "yellow")) # pasta grafiği oluşturma 

boxplot(veri$Sepal.Length) # sayıların hangi aralıkta değiştiğini görmemize yarar.
boxplot(veri$Sepal.Width ~ veri$Species, col=c("green","red", "blue")) #sepal width in dağılım aralığını yaprak türüne göre gördük
#ortada görünen kutu içindeki kalın çizgi ortalamadır. alt ve üstteki çizgiler max ve min değerleri gösterir.

hist(veri$Sepal.Length, xlab = "Sepal Length", freq = FALSE) #histogram grafiği çizdirme
#histogram ın barplot dan farkı her girdi için değil aralık grafiği oluşturur.

hist(veri$Petal.Width, freq = FALSE) 
hist(veri$Petal.Width)
lines(veri$Sepal.Width)

getwd() # çalışma dizinini bulmak için
setwd("/home/pars/Masaüstü/PARS/R_Projects/") #çalışma dizinini değiştirmek için
getwd() # kontrol edelim nerde olduğumuzu 

cancer = read.csv("cancer.csv", header = TRUE) # cancer isimli nternetten indirdiğim datayı yükledim
# kodun bulunduğu dizinde var indirip çalışma dizinine atın.
head(cancer)
hist(as.numeric(cancer$age), col="blue", freq = FALSE) #yaş kolonunun histogram grafiği
lines(density(as.numeric(cancer$age)), col="red", lwd=2) # çizgi ile yoğunluk çizer.
box(which = "plot") # grafiğin etrafına çerçeve koyar.
help("box") # help ile daha fazla özellik görüntülenebilir

names(cancer) # cancer verisindeki kolon isimleri
stem(as.numeric(cancer$deg.malig)) # as numeric yapmamın sebebi numerik olmayan verileri görüntülemiyor
stem(as.numeric(cancer$deg.malig), scale = 0.1) #ölçek aralığını onda birine düşürdük



