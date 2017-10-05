require(ggplot2)
#normal daðýlým konusu incelenmiþtir.
#on tane rastgele sayý oluþturuldu.
rnorm(10)
#ortalamalarý 100 ve standart sapmasý 20 olucak þekilde on tane sayý oluþturuldur
rnorm(10, mean = 100, sd=20)

randNums<-rnorm(50000)
randDens<- dnorm(randNums)
#birbirine simetrik bir grafik elde ederiz.
ggplot(data.frame(x=randNums,y=randDens))+aes(x=x,y=y)+geom_point()
    +labs(x="Rastgele Deðiþkenler",y="Yoðunluk deðerleri")

#10 deneme yapýlýr her denemede baþarý oraný %40'dýr baþarýlý olan denemeler return edilir.
rbinom(n=1,size=10,prob=0.4)

binomData <- data.frame(Success=rbinom(n=10000,size=10,prob=0.2))
ggplot(binomData,aes(x=Success))+geom_histogram(binwidth = 1)

numbers <- c(100,23,56,12,58,67,42,55) 
#sayýlarýn ortalamasý
mean(numbers)


####döngüler - if koþulu - etc.
topla<-function(x,y)
{
  x+y
}

do.call("topla",args=list(x=56,y=20))


deger<-0
if(deger==0)
{
  print("Doðru")
} else {
  print("Yanlýþ")
}

ifelse(deger==0,"Doðru","Yanlýþ")



switchOrnek<-function(x)
{
  switch(x,
         "1"="birinci",
         "2"="ikinci",
         "3"="üçüncü",
         "diðer durum")
}

switchOrnek("1")

switchOrnek("5")


for(i in 1:10){
  print(i)
}


