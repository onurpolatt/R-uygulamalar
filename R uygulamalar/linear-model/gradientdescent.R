set.seed(1234)
## index iþlemlerinde sýkýntýlar var.
df<-data.frame(c(0,0.22,0.24,0.33,0.37,0.44,0.44,0.57,0.93,1.00),c(0,0.22,0.58,0.20,0.55,0.39,0.54,0.53,1.00,0.61))
colnames(df)<-c("x","y")



firstAttempt<-TRUE
methodMLR<-FALSE
methodSLR<-FALSE

thetaValues <- vector(mode="numeric", length=0)
derSSETheta <- vector(mode="numeric", length=0)

k<-0
createTheta<-function(countCol)
{
  for(i in 0:countCol+1){
     thetaValues[i]<-signif(runif(1,0,1),digits=2)
  }
  return(thetaValues)
}

gradientDescent<-function(dataY,dataX,alpha,tolerance=0.01,method)
{
  if(method=="SLR"){
    thetaCount<-1
    methodSLR<-TRUE
    methodMLR<-FALSE
  }else if(method=="MLR")
  {
    thetaCount<-ncol(dataX)
    methodMLR<-TRUE
    methodSLR<-FALSE
  }else
  {
    return(paste("Girilen method türü geçerli deðildir."))
  }
  
  thetaValues<-createTheta(thetaCount)
    for(j in 0:thetaCount+1)
    {
      if(firstAttempt)
      {
        hypothesis <- thetaValues[j] 
        
        firstAttempt=FALSE
      }else
      {
        hypothesis <- hypothesis+thetaValues[j]*dataX
      }
    }
  derSSETheta<-sum((hypothesis-dataY))
  sse<-as.data.frame(sum((dataY-hypothesis)^2))
  paste("Deðer",sse[nrow(sse)-1,])
  colnames(sse)<-c("sse")
  
  
  while(TRUE)
  {
    if(methodSLR & !methodMLR){
      derSSETheta0<-sum((hypothesis-dataY))
      derSSETheta1<-sum((hypothesis-dataY)*dataX)
      
      thetaValues[0]<-thetaValues[0]-alpha*derSSETheta0
      thetaValues[1]<-thetaValues[1]-alpha*derSSETheta1
      
      hypothesis<-thetaValues[0]+thetaValues[1]*dataX
      sseNew<-as.data.frame(sum((dataY-hypothesis)^2))
      colnames(sseNew)<-c("sse")
      sse<-rbind(sse,sseNew)
      if(sse[nrow(sse)-1,]-sse[nrow(sse),]< tolerance)
      {
        break
      }
      k<-k+1
    }else if(!methodSLR & methodMLR)
    {
      derSSETheta0<-sum((hypothesis-dataY[,]))
      for(j in 1:ncol(dataY)){
        derSSETheta[j-1]<-sum((hypothesis-dataY[,])*dataX[,j])
      }
      for(j in 1:thetaCount){
        if(j==1){
          thetaValues[j-1]<-thetaValues[j-1]-alpha*derSSETheta0
          hypothesis<-thetaValues[j-1]
        }else{
          thetaValues[j-1]<-thetaValues[j-1]-alpha*derSSETheta[j-1]
          hypothesis<-hypothesis+thetaValues[j-1]*dataX[,j]
        }
      }
      sseNew<-as.data.frame(sum((dataY[,]-hypothesis)^2))
      colnames(sseNew)<-c("sse")
      sse<-rbind(sse,sseNew)
      if(sse[nrow(sse)-1,]-sse[nrow(sse),]< tolerance)
      {
        break
      }
      k<-k+1
    }
  }
  return(thetaValues)
}

paste("Ýterasyon sayýsý:",k)
paste("en düþük hata miktarý:",sse)
paste("Intercept deðeri",thetaValues[0])
paste("Coefficient deðerleri",thetaValues)


 gradientDescent(df$y,df$x,0.1,0.01,"SLR")

 ncol(df)
 df$y


 
 
