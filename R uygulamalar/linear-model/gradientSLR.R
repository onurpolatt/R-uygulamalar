#�rnek bir data frame
df<-data.frame(c(0,0.22,0.24,0.33,0.37,0.44,0.44,0.57,0.93,1.00),c(0,0.22,0.58,0.20,0.55,0.39,0.54,0.53,1.00,0.61))
colnames(df)<-c("x","y")


#tolerans de�erini d�ng�y� sonland�rma kriteri olarak kullanaca��z.
tolerance<-0.01
#aplha-learning rate ��renme oran�
alpha<-0.1
#theta de�erleri ilk etapta 0-1 aras�nda rastgele olarak se�ildi.
theta0<-signif(runif(1,0,1),digits=2)
theta1<-signif(runif(1,0,1),digits=2)
#hipotezimiz.
hypothesis<-theta0+theta1*df$x
#olu�an hata de�erini data frame aktard�k.  
derSSETheta0<-sum((hypothesis-df$y))
paste("De�er",sse[nrow(sse)-1,])
sse<-as.data.frame(sum((df$y-hypothesis)^2))
colnames(sse)<-c("sse")
sse>tolerance
k<-0
 while(TRUE) {
   #hata de�erleri form�le g�re hesapland�.
   derSSETheta0<-sum((hypothesis-df$y))
   derSSETheta1<-sum((hypothesis-df$y)*df$x)
   #theta de�erleri e� zamanl� olarak g�ncellendi.
   theta0<-theta0-alpha*derSSETheta0
   theta1<-theta1-alpha*derSSETheta1
   #hipotez denklemimiz g�ncellendi.
   hypothesis<-theta0+theta1*df$x
   #olu�an yeni hata de�eri data frame'e eklendi
   sseNew<-as.data.frame(sum((df$y-hypothesis)^2))
   colnames(sseNew)<-c("sse")
   sse<-rbind(sse,sseNew)
   #olu�an yeni hata de�eri bir �nceki hata de�eri ile kar��la�t�r�ld�.E�er hata de�eri tolerans de�erinden k���k ��karsa d�ng� sonlan�r.
      if(sse[nrow(sse)-1,]-sse[nrow(sse),]< tolerance)
      {
      break
      }
   k<-k+1
}
paste("�terasyon say�s�:",k)
paste("en d���k hata miktar�:",sse)
paste("Intercept de�eri",theta0)
paste("Coefficient de�eri",theta1)
 
plot(1:52,t(sse))
plot(df)


#theta de�erlerini lm() fonksiyonu ile bulabiliriz. 
linearModel<-lm(df$y~df$x,data=df)
#summary() fonksiyonu ile elde etti�imiz "Intercept" de�erleri bizim theta de�erlerimizdir.
summary(linearModel)
coef<-summary(linearModel)$coefficients[,1]
intercept<-coef[1]
coefficients<-coef[2]
paste("Theta0 de�eri",coef[1],"Theta1 de�eri ",coef[2])