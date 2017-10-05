library(titanic)
data("Titanic")
df<-as.data.frame(Titanic)


split <- sample(nrow(df), size = floor(0.75 * nrow(df)))
split

newDf<-df[,c(0:4)]


trainData<-newDf[split,]
testData<-newDf[-split,]


        
logModel<-glm(Survived~.,family = binomial(link="logit"),data=newDf)
summary(logModel)

contrasts(newDf$Age)

fitted.results <- predict(logModel,newdata=testData,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
output<-as.data.frame(fitted.results)

is.na(df$Survived)
