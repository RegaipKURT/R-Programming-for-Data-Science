library(tidyverse) 
library(cluster)    #kumeleme
library(factoextra) #gorsel
library(dplyr)
library(funModeling)
library(ggplot2)
library(ggExtra)
library(GGally)

df <- USArrests
apply(df, 2, var)
#varyanslar çok yüksek olduğu için standartlaştırma işlemi yapmamız gerekiyor.
df <- scale(df)
#uzaklık metrikleri oluşturalım
d <- dist(df, diag = TRUE)

#birleştirici kümeleme yöntemi için hclust, bölümleyici kümeleme için 

hc1 <- hclust(d, method = "complete")
plot(hc1)

hc2 <- agnes(df, method = "complete")
hc2$ac # method = "complete" parametresinin başarısını veren katsayıdır.
# fakat başka metodlar da var ve bunlardan hangisinin en iyi sonucu verdiğini
# görmek istersek bütün methodların başarısını aşağıdaki gibi bulabliriz.

#kullanabileceğimiz methodlardan oluşan bir vektör oluşturalım

m = c("average", "complete", "single", "ward")
names(m) <- c("average", "complete", "single", "ward")

ac <- function(x) {
  agnes(df, method = x)$ac
}

sapply(m, ac)
"
gördük ki ward methodu en iyi sonucu verdi ve son dendogramı buna göre oluşturalım
"

hc3 <- agnes(df, method = "ward")
hc3
pltree(hc3)

#şimdi bölümleyici kümeleme yöntemine bakalım
hc4 <- diana(df)
hc4
hc4$dc
pltree(hc4)

den <- hclust(d, method = "ward.D2")
alt_grup <- cutree(den, k = 3)
table(alt_grup)
pltree(alt_grup)

USArrests %>% mutate(cluster = alt_grup) %>% filter(cluster==1) %>% select(Murder)

plot(den)
rect.hclust(den, k = 4, border="blue")
rect.hclust(den, k = 3, border="red")
rect.hclust(den, k = 2, border="green")

fviz_cluster(list(data=df, cluster=alt_grup))

install.packages("dendextend")
library(dendextend)
df <- USArrests
df <- scale(df)

dd <- dist(df)
bir <- hclust(dd, method = "ward.D2")
iki <- hclust(dd, method = "complete")

d1 <- as.dendrogram(bir)
d2 <- as.dendrogram(iki)

tanglegram(d1, d2)

# optimum küme sayısının belirlenmesi
fviz_nbclust(x = df, FUNcluster = hcut, method = "wss")
fviz_nbclust(x = df, FUNcluster = hcut, method = "silhouette")
fviz_nbclust(x = df, FUNcluster = hcut, method = "gap")
#biz bu grseller ve incelemeler sonucunda 3 sınıflı yapı oluşturmaya karar verdik

son = hclust(dd, method = "ward.D")
alt_grup = cutree(son, k = 3)
fviz_cluster(data = df, show.clust.cent = T, object = son)

USArrests %>%
  mutate(cluster=alt_grup)

new_df <- USArrests %>%
  mutate(cluster=alt_grup)%>%
  group_by(cluster)%>%
  
new_df

par(mfrow=c(1,2))
barplot(height = new_df$Murder, names.arg = new_df$cluster, 
        col = 1:3, main = "Eyalet Gruplarının \nCinayet Oranları")
barplot(height = new_df$Assault, names.arg = new_df$cluster, 
        col = 1:3, main = "Eyalet Gruplarının \nSaldırı Oranları")
barplot(height = new_df$Rape, names.arg = new_df$cluster, 
        col = 1:3, main = "Eyalet Gruplarının \nTecavüz Oranları")
barplot(height = new_df$UrbanPop, names.arg = new_df$cluster, 
        col = 1:3, main = "Eyalet Gruplarının \nNüfusları")


isimli <- data.frame(grup=alt_grup, isim=rownames(USArrests))

Eyalet_ismi <- rownames(USArrests)




