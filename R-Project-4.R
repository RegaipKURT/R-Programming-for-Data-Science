# R Project Ortamın Kaydedilmesi ve Scriptler

x = c(1,2,3,4,5,6) # liste oluşturma
x = seq(1:6) # 1'den 6'ya kadar olan sayılar için bu şekilde de oluşturabiliriz.
getwd() # şu anki çalışma dizinini gösterir

setwd("/home/pars") # çalışma dizinini yeniden istediğimiz bir yere belirtebiliriz.

save.image("kayit.rdata") #şu anki çalışma içeriğimizi bir dosyaya kaydettik.

load("kayit.rdata") # kaydettiğimiz dosyayı çalışma ortamına yükeldik.

load(file.choose()) #burada açılan pencereden de yüklemek istediğimiz dosyayı seçebiliriz.
# yukarıdaki konutu program yazarken veriyi yüklemek için de kullanabiliriz.
ls # dosyayı yükledikten sonra oluşturduğumuz x burada görünecek.
#denemek için programı açıp kapatın ve değişkenlerin yüklendiğini göreceksiniz.

#remove(list = ls()) # oluşturduğumuz bütün değişkenleri hafızadan sildik
