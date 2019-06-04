# R ile İstatistiksel Dağılımlar

#binom dağılımı bir olayın arka arkaya meydana gelme sıklığını hesaplar
help("dbinom")
dbinom(x=10, prob = 1/6, size = 20)

#Normal dağılım
help("pnorm")
dnorm(x=10, mean = 0, sd=20)
pnorm(q=0, mean = 2, sd=10)
pnorm(q=0, mean = 2, sd=10, lower.tail = FALSE)

x = 0:25 #şimdi x gibi bir dizi için normal dağılıma bakalım
dnorm(x, mean = 12, sd=12) # x dizisi için normal dağılım değerleri 

a = dnorm(x, mean = 12, sd=12) # a değişkenine atayıp toplamına bakmak istiyorum sayıların
sum(a) # 0.72 çıktı ama ne yaptım ben de bilmiyorum. niye öyle çıktı acaba?

plot(x, a) #plot grafiğini çizdirince ne yaptığımız çok daha iyi anlaşılıyor
abline(v=12) # 12 yi orta değer belirlediğim için oraya bir çizgi koyduk grafikte

#POISSON Dağılımı  
help("dpois")
dpois(x=3, lambda = 10) #x=3 lambda=10 için dağılım
dpois(x=1:7, lambda = 10) # dizi için de kullanılabilir.
x=1:50
a = dpois(x, lambda = 10)
plot(x,a) # yine grafik ile daha iyi anlaşılıyor

#Student-T Dağılımı veya T-Dağılımı
qt(0.25, df=25)
pt(0.40, df=10)
help("qt") #kullanacağımız parametrelere bakabiliriz help ile

#aşağıdaki dağılımlar da istatistikte sıkça kullanılır. bakmakta fayda var.
help(pf) # f dağılımı
help(pexp) # üssel(exponential) dağılım
