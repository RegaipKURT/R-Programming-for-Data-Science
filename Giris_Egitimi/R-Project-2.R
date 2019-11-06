a = seq(from=5, to=25, by=3) #5'ten 25'e 3 artarak giden dizi
b = seq(from=40, to = 18, by = -4) #40'tan 18'e kadar 4 azalarak giden dizi
c = seq(from=1, to = 10, by = 1.5)

d = rep(10:14, times=3)
# yukarıdaki kod ile 10 dan 14 e kadar olan sayilari 3 kere tekrar ettik
e = rep(seq(from=1, to=2, by=0.25), times=2)
#1'den 2'ye kadar 0.25 artarak giden sayıları 2 kez tekrar ettik

length(e) #e serisinin eleman sayısı yani uzunluğu
a / 3 # a dizisinin her elemanını 3 e böler
a -2 #a nın her elemanından 2 çıkarır

max(a) #a'nın en büyük elemanı
min(b) #c'nin en küçük elemanı

a = 5:9 #a 5'den 9'a kadar sayılar

a[-2] # a'nın 2. elemanı dışındakiler

a[-2:-4] #a'nın 2. vew4. elemanları arasındakiler hariç kısmı

a + d # a ve d vektörlerini topladık
#iki vektörün toplanması için boyutlar aynı veya birbirinin katı olmalıdır.
#sadece toplama değil bütün işlemlerde geçerlidir.

x = 60:64

a *x 


# MATRİSLER

matris_row = matrix(1:9, ncol = 3, byrow = TRUE)
matris_col = matrix(1:9, nrow = 3, byrow = FALSE)
matris = matrix(1:8, nrow = 2, byrow = TRUE)
# kolon sayısı veya satır sayısından istenilen birisi belirtilebilir.

#kolon veya satır bazlı matris yapısı elemanlara erişimi etkiler
# aşağıdaki örneklerde bu fark görülebilir.
matris_row[4] #4. eleman (2 boyutlu vektör mantığına göre) (2)
matris_col[4] #4. eleman (2 boyutlu vektör mantığına göre) (4)

matris_col[3,2] # 3.satır 2. sütun elemanı (6)
matris_row[3,2] # 3.satır 2. sütun elemanı (8)

matris[1,] # 1. satır elemanlarına erişim.
matris[,2] # 2. sütun elemanlarına erişim.

matris[-2,2] # 2. sütunun 2. elemanı hariç olanlar kısmı

matris[1, -3] # matrisin 1. satırında 3. eleman hariç olanlar

matris_col / matris_row # matris bölümü. işlemlerin hepsinde boyut aynı olmalı
#dizilerle yapılan işlemlerin hepsi matrislerle de yapılabilir.

