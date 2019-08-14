#TARİH VE SAAT DEĞİŞKENLERİ

#tarih ve saat işlemleri için lubridate kütüphanesi kullanılabilir.
#tidywords kütüphanesi de kullanılabilir.

install.packages("lubridate")

library(lubridate)

df_bir <- data.frame(
  yas = c(10, 11, 30),
  boy = c(120, 140, 160),
  dogum = c("20100201", "20110522", "20090430")
)


df_iki <- data.frame(
  yas = c(10, 11, 30),
  boy = c(120, 140, 160),
  dogum = c("2010_02_01", "2011_05_22", "2009_04_30")
)


df_uc <- data.frame(
  yas = c(10, 11, 30),
  boy = c(120, 140, 160),
  dogum = c("01022010", "22052011", "30042009")
)

str(df_bir) # gördüğümüz gibi doğum factor değişken ama tarih olması lazım.
#özellikle grafik işlemlerimnde, zaman serilerinde vs. 
#değişkenin tarih tipinde olması gerekiyor ki düzgünce çalışabilelim.

df_bir$dogum <- ymd(df_bir$dogum) #ymd --> year, month, day şeklinde sıralandığını belirtiyor
str(df_bir) # doğum değikenleri tarih haline döndü görüldüğü gibi 

plot(df_bir$dogum, df_bir$boy) #görmek açısından plot çizelim
#gördüğümüz gibi tarih sadece yıl olarak gösterildi. 

#bir de tarih haline çevirmesek nasıl olurdu bakalım
plot(df_iki$dogum, df_iki$boy)
#düzgün bir grafik oluşturulamadığı görülüyor.
#eğer çok veri olsaydı x eksenindeki yazılar birbirine girecekti zaten.


# ingilizce baş harfleri ile çok çeşitli başka tipler de dönüştürülebilir.
a <- ymd("20111123")
b <- dmy("21122018")
c <- mdy("06232011")
d <- mdy_hms("06252015025506")
e <- ymd_hms("2014_04_21_06_55_45")
f <- dmy_hm("301220192345")
a
b
c
d
e
f


#tarih ve saat işlemleri için bazı başka gömülü kütüphane ve farklı libraryler bulunabilir.

