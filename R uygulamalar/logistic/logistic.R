set.seed(1234)
library(sigmoid)

#.csv formatındaki dosyaları train ve test olmak üzere iki ayrı şekilde okuduk.
traindf<-read.csv("C:\\Users\\onurp\\Desktop\\train.csv",header = TRUE,sep = ",")
testdf<-read.csv("C:\\Users\\onurp\\Desktop\\test.csv",header = TRUE,sep = ",")

#ilgilendiğimiz sütunlar seçildi.
traindf<-traindf[,c("Survived","Pclass","SibSp","Parch","Age","Fare","Sex")]
newdf<-testdf[,c("PassengerId")]
newdf<-as.data.frame(newdf)

#na değerleri aynı sütun üzerindeki değerlerin ortalaması ile değiştirildi.
traindf$Fare[is.na(traindf$Fare)] <- median(traindf$Fare, na.rm=TRUE)
traindf$Age[is.na(traindf$Age)] <- median(traindf$Age, na.rm=TRUE)
#normalizasyon işlemi iki sütun için yapıldı.
traindf$Fare <- (traindf$Fare-min(traindf$Fare))/(max(traindf$Fare)-min(traindf$Fare))
traindf$Age <- (traindf$Age-min(traindf$Age))/(max(traindf$Age)-min(traindf$Age))

testdf<-testdf[,c("Pclass","Age","SibSp","Parch","Fare","Sex")]

testdf$Fare[is.na(testdf$Fare)] <- median(testdf$Fare, na.rm=TRUE)
testdf$Age[is.na(testdf$Age)] <- median(testdf$Age, na.rm=TRUE)
testdf$Fare <- (testdf$Fare-min(testdf$Fare))/(max(testdf$Fare)-min(testdf$Fare))
testdf$Age <- (testdf$Age-min(testdf$Age))/(max(testdf$Age)-min(testdf$Age))

#logistic model oluşturuldu ve model eğitildi.
logModel<-glm(Survived~.,family = binomial(link="logit"),data=traindf)
summary(logModel)

#train veri seti üzerinde yapılan tahminler
training_predictions <- predict(logModel, type = 'response')

#hata oranı hesaplandı.
training_error <-sum((training_predictions >= 0.5))/nrow(traindf)
training_error
1-training_error

#test veri seti üzerinde yapılan tahmin işlemleri.
test_predictions = predict(logModel,testdf,type="response")
test_predictions = predict(logModel,testdf[1,],type="response")

#tahmin değeri 0.5 değeri üzerinde ise sonuç olarak 1 değerini ata
test_predictions[test_predictions >= 0.5] <- 1
test_predictions[test_predictions != 1] <- 0
test_predictions[is.na(test_predictions)] <- 0

results<-as.data.frame(test_predictions)

colnames(results)<-c("Survived")

df<-cbind(newdf,results)
colnames(df)<-c("PassengerId","Survived")

write.csv(df,"C:\\Users\\onurp\\Desktop\\output.csv",row.names = FALSE,quote = FALSE)
