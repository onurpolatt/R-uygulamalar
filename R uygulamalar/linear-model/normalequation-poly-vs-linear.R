x<-matrix(c(1,1,1,1,1,2,3,4),nrow=4)#x matrisimizi oluþturduk.
y<-matrix(c(1,2.2,2.8,4),nrow = 4)#y matrisimizi oluþturduk.


ginv((t(x)%*% x))%*% t(x)%*% y#normal equation 


df<-data.frame(c(1,2,3,4),c(1,2.2,2.8,4))
names(df)<-c("x","y")

linearModel<-lm(df$y~df$x,data=df)

predictdf<-as.data.frame(predict(linearModel,df))
names(predictdf)<-c("x")

plot(df$y~df$x)
abline(lm(df$y~df$x))
cor(df$x,df$y,method = "pearson")

sm<-summary(linearModel)

data("women")
plot(women)
cor(women$height,women$weight , method = "pearson") 
abline(lm(weight ~height , data=women))
mse <- function(sm) #mean squared error hesabý yapan fonksiyon
    mean(sm$residuals^2)
mse(sm)


###########
###########poly vs linear
df1<-data.frame(c(50,50,50,70,70,70,80,80,80,90,90,90,100,100,100),
                c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3,3.1,2.8,3.3,3.5,3))
colnames(df1)<-c("Temp","Yield")#sütun isimlerini deðiþtirdik.

linModel<-lm(df1$Yield~df1$Temp,data=df1)#linear model
polyModel<-lm(Yield~poly(Temp,2,raw=TRUE),data=df1)#polynomial model

summary(linModel)
summary(polyModel)


plot(df1$Yield~df1$Temp,main="Temp v Yield",xlab="Temp",ylab="Yield")
abline(lm(df1$Yield~df1$Temp))#linear çizgi

fit <- lm(df1$Yield ~ df1$Temp + I(df1$Temp^2))
lines(sort(df1$Temp), fitted(fit)[order(df1$Temp)], col='red', type='b') #eðri çizgi
#iki deðiþken arasý korelasyon
cor(df1$Yield,df1$Temp)



