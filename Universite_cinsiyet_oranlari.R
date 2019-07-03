#Dünya Üniversitelerinde Kadın ve Erkek Oranları ve Grafikleştirmesi

veri = read.csv(file.choose()) # veri.csv dosyasını indiriniz ve SOrunca seçiniz.
summary(veri) # verinin özet bilgilerine bakıyoruz.

#barplot grafiğini kadın-erkek sayısı farkına göre çizdirelim.
barplot(veri$Gap, col = heat.colors(21), ylab = "Erkek-Kadın Farkı", 
        names.arg = veri$School, srt=45, las=2, 
        main = "Üniversitelerde\n Kadın-Erkek Sayısı Farkı")

#kadın erkek oranına göre barplot çizdirelim. Sadece fark ile sonuca varamayabiliriz.
barplot(veri$Men/veri$Women, col = heat.colors(21), ylab = "Erkek/Kadın Oranı", 
        names.arg = veri$School, srt=45, las=2, 
        main = "Üniversitelerde\n Erkek/Kadın Oranı")

#FAKAT TAM OLARAK ANLAMAK İÇİN BOPXLOT ÇİZDİRMEMİZ GEREKİYOR Kİ ARALIĞI GÖRELİM
boxplot(veri$Men/veri$Women, xlab="Oranlar", main="Kadın-Erkek Dağılımı\nBoxPlot", 
        col.lab=2, col.main=4, col.axis="magenta4", col="orange", horizontal = T)
help("boxplot")
#bu grafikte gördük ki en düşük 1.2 katına yakın erkekler daha fazla iken
#en yüksekte erkekler kadınlardan 1.6 kat daha fazla
#ve bu ünivresitelerde erkekler kadınların ortalama 1.4 katı kadar
#Erkek oranı MIT de en fazla iken, SoCal (Southern California) ise en düşük orana sahip

