library(jsonlite)
library(pROC)
library(party)
library(lubridate)

##JSON parse denemeleri.


cnames<-c("ay","donem","s?cakl?k","cloudcover","nem","wcondition")

df <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors=FALSE,check.names=FALSE)

z <- cnames
colnames(df) <- z

url <- 'http://api.wunderground.com/api/263a97472cc0ce7f/hourly10day/q/CA/Izmit.json'
urlSun<-'http://api.apixu.com/v1/forecast.json?key=a32bb89622ca45f29bb71349171307&q=Izmit&days=7'
document <- fromJSON(txt=url)
documentSun<-fromJSON(txt=urlSun)
sunRise<-documentSun$forecast$forecastday$astro$sunrise[1]
sunSet<-documentSun$forecast$forecastday$astro$sunset[1]
getMonth<-function(x){
  month<-strsplit(x,"-")[[1]][2]
  return(month)
}
convertCondition<- function(type) {
  print(type)
  switch(type,
         "Clear" = 1,
         "Cloudy" = 2,
         "Rain" = 3,
         "Thunderstorm"=4,
         "Mostly Cloudy"=5,
         "Partly Cloudy"=6,
         "Torrential rain shower"=7,
         "Overcast"=8,
         "Mist"=9,
         "Fog"=10 )
}
donemCevir<-function(hour,sunRise,SunSet){
  sonuc<-ifelse(strptime(hour, "%H:%M") > strptime(sunRise, "%H:%M") & strptime(hour,"%H:%M") < strptime(SunSet,"%H:%M"), "acik", "kapali")
  return (sonuc)
}

newHour<-format(parse_date_time(hour, "%I:%M %p"), format="%H:%M")
newsrTime<-format(parse_date_time(sunRise, "%I:%M %p"), format="%H:%M")
newssTime<-format(parse_date_time(sunSet, "%I:%M %p"), format="%H:%M")
response<-donemCevir(newHour,newsrTime,newssTime)
if(response=="acik"){
  month<-document$hourly_forecast$FCTTIME$mon
  hour<-document$hourly_forecast$FCTTIME$civil
  temp<-document$hourly_forecast$temp$metric
  cloudcover<-document$hourly_forecast$sky
  humidity<-document$hourly_forecast$humidity
  wcondition<-document$hourly_forecast$condition
}
 
cat(sprintf("\"%s\" \"%s\"\n", response,newHour))
#print(newHour)

