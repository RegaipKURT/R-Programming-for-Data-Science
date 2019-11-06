# R One Sample ve Two Sample T dağılım Testleri

# t dağılımı veya t testi iki durmu karşılaştırmak ve sonuca etkisini bulmak için kullanılır
# H0: u1=u2 (0 hipotezi denen bu durum iki ortalama birbirine eşittir anlamına gelir)
# H1: u1=!u2  H1 hipotezimiz ise tam tersi (yani iki dağılımın ortalaması eşit değil demek)

# iki farklı grup üzerinde analiz yapacaksak t testine bakarak ortalamalar eşitse 
# iki grubu aynı kabul edip analizimizin iki taraf için özdeş olduğunu söyleyebiliriz. (KESİN DEĞİL, BAŞKA ŞEYLERE DE BAKMAK GEREKEBİLİR)
# Ama eğer farklı çıkıyorsa KESİNLİKLE bu iki grubu ayırıp ayrı ayrı analiz etmek gerekir!

setwd("Masaüstü/PARS/R_Projects/") #çalışma ortamını verinin olduğu yere aldık
cancer = read.csv("cancer2.csv") # cancer verisetini yükledik

summary(cancer) #verisetinin istatistiksel özeti

help("t.test") #t test

a = t.test(cancer$age) # one sample two test 
comment(a) = " 
%95 doğrulukla(kesinlikle) verisetinin ortalması 61.26330 ve 63.63143 arasında değişiyormuş. 
Ortalama ise 62.44737 imiş.
"
a
comment(a)

#şimdi bazı ayarları değiştirelim
t.test(cancer$age, conf.level = 0.99, alternative = "greater", mu=63)
# %99 doğrulukla 63 den büyük mü ortalama => test ettik.

#TWO-SAMPLE T-TEST
#Yaş ve cinsiyet arasındaki ilişki

boxplot(cancer$age ~ cancer$sex) #boxplot ile ortalamaların nasıl dağıldığını grafikleştirelim
t.test(cancer$age ~ cancer$sex)
t.test(cancer$age ~ cancer$sex, mu=62, alt="less")

#aşağıdaki şekilde değişkene atayıp verisetindeki gibi özelliklerini çağırabiliriz.
sonuc = t.test(cancer$age ~ cancer$sex, mu=62, alt="two.sided")
sonuc$statistic
sonuc$p.value
sonuc$alternative

