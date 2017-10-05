data("Prestige")
#prestige veri kümesi kullanýyoruz.
head(Prestige)

df<-Prestige[,c(1:4)]
#1 ila 4 arasý sütunlarý df'ye aktardýk.
head(df)

plot(df, col="red", main="Deðiþkenlerin Birbirleriyle iliþkisi")
#korelasyon 

education.n = scale(df$education, center=TRUE, scale=FALSE)
prestige.n = scale(df$prestige, center=TRUE, scale=FALSE)
women.n = scale(df$women, center=TRUE, scale=FALSE)
#ölçekleme iþlemleri yapýldý.

newpre<-cbind(education.n,prestige.n,women.n)
df<-cbind(df,newpre)
#newpre isimli vektörümüzü df isimli data frame'e eklendi.
names(df)[5:7]<-c("education.n","prestige.n","women.n")
#5 ile 7 sütunlarý arasýndaki sütun isimleri deðiþtirildi.

modelMLR<-lm(income~education.n + prestige.n + women.n,data=df)
#linear model

summary(modelMLR)
#model hakkýnda özet


