require(ggplot2)
#normal da��l�m konusu incelenmi�tir.
#on tane rastgele say� olu�turuldu.
rnorm(10)
#ortalamalar� 100 ve standart sapmas� 20 olucak �ekilde on tane say� olu�turuldur
rnorm(10, mean = 100, sd=20)

randNums<-rnorm(50000)
randDens<- dnorm(randNums)
#birbirine simetrik bir grafik elde ederiz.
ggplot(data.frame(x=randNums,y=randDens))+aes(x=x,y=y)+geom_point()
    +labs(x="Rastgele De�i�kenler",y="Yo�unluk de�erleri")

#10 deneme yap�l�r her denemede ba�ar� oran� %40'd�r ba�ar�l� olan denemeler return edilir.
rbinom(n=1,size=10,prob=0.4)

binomData <- data.frame(Success=rbinom(n=10000,size=10,prob=0.2))
ggplot(binomData,aes(x=Success))+geom_histogram(binwidth = 1)

numbers <- c(100,23,56,12,58,67,42,55) 
#say�lar�n ortalamas�
mean(numbers)


####d�ng�ler - if ko�ulu - etc.
topla<-function(x,y)
{
  x+y
}

do.call("topla",args=list(x=56,y=20))


deger<-0
if(deger==0)
{
  print("Do�ru")
} else {
  print("Yanl��")
}

ifelse(deger==0,"Do�ru","Yanl��")



switchOrnek<-function(x)
{
  switch(x,
         "1"="birinci",
         "2"="ikinci",
         "3"="���nc�",
         "di�er durum")
}

switchOrnek("1")

switchOrnek("5")


for(i in 1:10){
  print(i)
}


