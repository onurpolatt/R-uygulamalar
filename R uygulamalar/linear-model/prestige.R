data("Prestige")
#prestige veri k�mesi kullan�yoruz.
head(Prestige)

df<-Prestige[,c(1:4)]
#1 ila 4 aras� s�tunlar� df'ye aktard�k.
head(df)

plot(df, col="red", main="De�i�kenlerin Birbirleriyle ili�kisi")
#korelasyon 

education.n = scale(df$education, center=TRUE, scale=FALSE)
prestige.n = scale(df$prestige, center=TRUE, scale=FALSE)
women.n = scale(df$women, center=TRUE, scale=FALSE)
#�l�ekleme i�lemleri yap�ld�.

newpre<-cbind(education.n,prestige.n,women.n)
df<-cbind(df,newpre)
#newpre isimli vekt�r�m�z� df isimli data frame'e eklendi.
names(df)[5:7]<-c("education.n","prestige.n","women.n")
#5 ile 7 s�tunlar� aras�ndaki s�tun isimleri de�i�tirildi.

modelMLR<-lm(income~education.n + prestige.n + women.n,data=df)
#linear model

summary(modelMLR)
#model hakk�nda �zet


