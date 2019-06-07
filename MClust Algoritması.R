# mclust özellikle büyük veri setleri üzerinde hızlı çalışan bir kümeleme ve sınıflandırma algoritmasıdır.
# mclust hem sınıflandırma hem kümeleme yapabilir. (Sınıflandırma gözetimli, kümeleme gözetimsiz algoritmadır.)

#Default olarak R içinde yok. O yüzden dışardan yüklememiz lazım
install.packages("mclust") # paketin yüklenmesini bekleyelim

require(mclust) # mclust kütüphanesini kullanmak için yükledik

help("mclust") #help metodunun altındaki örneklerde nasıl kullanıldığı görülebilir

setwd("Masaüstü/PARS/R_Projects/") # çalışma dizinini kendinize göre belirleyin

#VERİYİ BU ŞEKİLDE DE SEÇMEK MÜMKÜN. AKILDA BULUNSUN LAZIM OLUR DİYE
yol = file.choose() #ayrıca bu şekilde seçilirse verinin çalışma dizini ile aynı yerde olmasına gerek kalmaz.
deneme_Verisi = read.csv(yol) #çalışma dizini değiştirmeden seçmek istiyorsanız böyle kullanabilirsiniz.
# özellikle program şeklinde kod yazarken lazım olacaktır. Çünkü kullanıcının hatasız devam etmesini sağlar.

remove(deneme_Verisi) #deneme verisini silelim
remove(list=ls()) # temizleyelim tüm yüklenen verileri(üstte yaptık gerek yok ama garanti temiz olsun)

cancer1 = read.csv("cancer.csv")
#ben cancer verisini yükledim. siz herhangi bir veriyi seçebilirsiniz kümelemeye uygun olmak şartıyla.

x = cancer1[,1:9] # verimizin 1 ve 9 kolonları arasını tahminde kullanılcak veriler olarak kullanacağız.
y = cancer1[10] # verinin 10. kolonu tahmin etmek istediğimiz sınıf olsun

#ilk başta bütün veriyle eğitmeyi görelim
model1 = Mclust(cancer1) #modeli otomatik olarak eğitti ve optimal küme sayısını buldu.

#Optimal küme veya sınıf sayısını görmek için
model1$G

#veriye ait grafikleri görebiliriz.
plot(model1) # buradan 0 ile çıkmayı unutmayın yoksa alttaki kod çalışmaz.
plot(model1, what = "classification") # grafik ismi verilerek de çalıştırmak mümkün
plot(model1, what = "density")

#mclust SINIFLANDIRMA ALGORİTMASI, ŞİMDİ MClustDA modeli ile sınıflandırma yapalım

#şimdi belirli verilerden tahmin yapmayı görelim (yani sınıflandırma kısmı)
remove(list = ls()) # eski verilerin tamamımını kaldırdık hafızadan

veri = iris # iris verisetini kullanalım
veri$Species = as.numeric(veri$Species)
x = veri[,1:4] #verinin ilk 4 kolonu 
y = veri[5] #verinin son kolonu (tahmin edilcek sınıf)

model = MclustDA(veri[,1:4], veri$Species) #y kısmını yani 2. kısmı veri$kolon_adı şeklinde yazın
#ben x vey y yi belirtirken y = veri[5] şeklinde yaptığımda hata aldım, o yüzden y = veri$kolon şeklinde yazın

summary(model) #modelin özeti ve gayet başarılı bir sınıflandırma olduğu görülüyor.
plot(model, what="scatterplot") #yukarıdaki grafiklerle benzer şekilde oluşturulabilir.
plot(model)

# densityMclust() modelini de deneyebilirsiniz. Onu öğrenilmesi ve kullanılması için eksik bıraktım.
