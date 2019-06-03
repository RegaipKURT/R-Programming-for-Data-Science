# R programlama için örnek kodlar

print("Merhaba R") # ekrana bir şey yazdırma  

# Değişkene değer atama
a = 3
3 -> a
a <- 3
a
# her üç durumda da a'nın değeri 3 olarak belirleniyor.
yazi = "Merhaba R" # string ifadeler "" içinde belirtilir.
yazi

# Matematiksel operatörler
b = 50
c = 90

b/c
b+c
b-c
b*c

d = 5
e = 2

d^e #üs alma şlemi
#seri oluşturma
seri = seq(30:10)
seri2 = c(10:20) #10'dan 20'ye kadar sayılar serisi
seri3 = c(20:10) #20'den 10 a kadar sayılar serisi
seri4 = c(10,20) #sadece 10 ve 20 den oluşan seri

#seri işlemleri

seri[6] #serinin 6. elemanı

seri3[c(2,3,4)] #seri 3 ün 2,3, ve 4. elemanları sırasıyla 
seri3[2:4] #hemen üstteki ile aynı işi yapıyor
seri2[seri2<16 & seri2 >12] #seri2 nin 16 dan küçük ve 12'den büyük elemanları
seri[seri>15 | seri <15] #serinin 15 ten büyük veya 15 ten küçük elemanları (hepsi)
# & işareti ve anlamına gelir. | işareti ise veya anlamına gelir.
# bu işaretler yukarıdaki gibi işlemlerde kullanılabilir.

#seri elemanlarını birleştirme
paste(c("x","y"), 0:10, sep = ",")
paste(c(3,5), 0:10, sep = "*")
paste(c(10), 0:5, sep = "*")
paste(seri3, seri2, sep = "-") #seri3 ve seri2 elemanlarını "-" işaretiyle birleştirecek


#Matematiksel fonksiyonalar

log10(10) #log10 tabanında 10
log(3, 9) #logaritma 9 tabanında 3
tan(45)

# Fonksiyon oluşturma
g = function(x,y) 
{
(x^2) - (2*y) + 6
}


