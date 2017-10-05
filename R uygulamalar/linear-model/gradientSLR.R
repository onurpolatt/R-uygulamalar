#örnek bir data frame
df<-data.frame(c(0,0.22,0.24,0.33,0.37,0.44,0.44,0.57,0.93,1.00),c(0,0.22,0.58,0.20,0.55,0.39,0.54,0.53,1.00,0.61))
colnames(df)<-c("x","y")


#tolerans deðerini döngüyü sonlandýrma kriteri olarak kullanacaðýz.
tolerance<-0.01
#aplha-learning rate öðrenme oraný
alpha<-0.1
#theta deðerleri ilk etapta 0-1 arasýnda rastgele olarak seçildi.
theta0<-signif(runif(1,0,1),digits=2)
theta1<-signif(runif(1,0,1),digits=2)
#hipotezimiz.
hypothesis<-theta0+theta1*df$x
#oluþan hata deðerini data frame aktardýk.  
derSSETheta0<-sum((hypothesis-df$y))
paste("Deðer",sse[nrow(sse)-1,])
sse<-as.data.frame(sum((df$y-hypothesis)^2))
colnames(sse)<-c("sse")
sse>tolerance
k<-0
 while(TRUE) {
   #hata deðerleri formüle göre hesaplandý.
   derSSETheta0<-sum((hypothesis-df$y))
   derSSETheta1<-sum((hypothesis-df$y)*df$x)
   #theta deðerleri eþ zamanlý olarak güncellendi.
   theta0<-theta0-alpha*derSSETheta0
   theta1<-theta1-alpha*derSSETheta1
   #hipotez denklemimiz güncellendi.
   hypothesis<-theta0+theta1*df$x
   #oluþan yeni hata deðeri data frame'e eklendi
   sseNew<-as.data.frame(sum((df$y-hypothesis)^2))
   colnames(sseNew)<-c("sse")
   sse<-rbind(sse,sseNew)
   #oluþan yeni hata deðeri bir önceki hata deðeri ile karþýlaþtýrýldý.Eðer hata deðeri tolerans deðerinden küçük çýkarsa döngü sonlanýr.
      if(sse[nrow(sse)-1,]-sse[nrow(sse),]< tolerance)
      {
      break
      }
   k<-k+1
}
paste("Ýterasyon sayýsý:",k)
paste("en düþük hata miktarý:",sse)
paste("Intercept deðeri",theta0)
paste("Coefficient deðeri",theta1)
 
plot(1:52,t(sse))
plot(df)


#theta deðerlerini lm() fonksiyonu ile bulabiliriz. 
linearModel<-lm(df$y~df$x,data=df)
#summary() fonksiyonu ile elde ettiðimiz "Intercept" deðerleri bizim theta deðerlerimizdir.
summary(linearModel)
coef<-summary(linearModel)$coefficients[,1]
intercept<-coef[1]
coefficients<-coef[2]
paste("Theta0 deðeri",coef[1],"Theta1 deðeri ",coef[2])