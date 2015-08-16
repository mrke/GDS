# read in weatherhawk data
# note that CR200Series_data2.dat has the date wrong, need to shift it one day back
#
weather.folder<-"Field Data/"

tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!

weather_obs<-as.data.frame(read.csv(paste(weather.folder,'CR200Series_data1.dat',sep=''),skip=1,stringsAsFactors=FALSE))
weather_obs<-weather_obs[-(1:2),]
weather_obs$TIMESTAMP<-as.POSIXct(weather_obs$TIMESTAMP,format="%Y-%m-%d %H:%M:%S",tz=tzone)
weather_obs<-subset(weather_obs,weather_obs$TIMESTAMP<as.POSIXct("2016-01-01 00:00:00",tz=tzone) & weather_obs$TIMESTAMP>as.POSIXct("2013-05-01 00:00:00",tz=tzone))
weather_obs$AirTemp_C_TMn<-as.POSIXct(weather_obs$AirTemp_C_TMn,format="%Y-%m-%d %H:%M:%S",tz=tzone)
weather_obs$AirTemp_C_TMx<-as.POSIXct(weather_obs$AirTemp_C_TMx,format="%Y-%m-%d %H:%M:%S",tz=tzone)
weather_obs$WindSpeed_ms_TMx<-as.POSIXct(weather_obs$WindSpeed_ms_TMx,format="%Y-%m-%d %H:%M:%S",tz=tzone)

weather_obs[,2:10]<-as.numeric(unlist(weather_obs[,2:10]))
weather_obs[,12]<-as.numeric(unlist(weather_obs[,12]))
weather_obs[,14:16]<-as.numeric(unlist(weather_obs[,14:16]))
weather_obs[,18:19]<-as.numeric(unlist(weather_obs[,18:19]))
# remove erroneous rainfall after gaps in data set
for(i in 2:nrow(weather_obs)){
  if(weather_obs[i,2]-weather_obs[i-1,2]!=1){
    weather_obs[i,19]=10000
  }
}

rain<-c(0,weather_obs[2:nrow(weather_obs),19]-weather_obs[1:(nrow(weather_obs)-1),19])
rain[rain<0]<-0
rain[rain>5000]<-0
weather_obs$RAIN<-rain



plot_weather_obs<-subset(weather_obs,weather_obs$TIMESTAMP>as.POSIXct("2014-01-01 00:00:00",tz=tzone) & weather_obs$TIMESTAMP<as.POSIXct("2014-01-31 00:00:00",tz=tzone))
#plot_weather_obs<-weather_obs
plot(plot_weather_obs$RAIN~plot_weather_obs$TIMESTAMP,type='h',col='blue')
plot(plot_weather_obs$AirTemp_C_Min~plot_weather_obs$TIMESTAMP,type='l',ylim=c(0,50))
points(plot_weather_obs$RAIN~plot_weather_obs$TIMESTAMP,type='h',col='blue')
plot(plot_weather_obs$AirTemp_C_Max~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$Solar_Avg~plot_weather_obs$TIMESTAMP,type='l')
points(plot_weather_obs$RAIN*100~plot_weather_obs$TIMESTAMP,type='h',col='blue')
plot(plot_weather_obs$Barometer_KPa~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$RH_Avg~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$ETo~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$WindSpeed_ms_Avg~plot_weather_obs$TIMESTAMP,type='l')



daily_rain<-aggregate(weather_obs$RAIN,by=list(as.Date(weather_obs$TIMESTAMP+9*3600,format="%Y-%m-%d")),FUN=sum) # note shifting 9 hours along to get 9am to 9am totals
colnames(daily_rain)<-c('date','rain')
daily_rain$date<-as.POSIXct(daily_rain$date,format=="%Y-%m-%d")-9.5*3600+24*3600
plot(daily_rain$rain~daily_rain$date,type='h',col='blue')

weather_obs2<-as.data.frame(read.csv(paste(weather.folder,'CR200Series_data2.dat',sep=''),skip=1,stringsAsFactors=FALSE))
weather_obs2<-weather_obs2[-(1:2),]
weather_obs2[,2:8]<-as.numeric(unlist(weather_obs2[,2:8]))
weather_obs2$TIMESTAMP<-as.POSIXct(weather_obs2$TIMESTAMP,format="%Y-%m-%d %H:%M:%S",tz=tzone)

rain<-c(0,weather_obs2[2:nrow(weather_obs2),7]-weather_obs2[1:(nrow(weather_obs2)-1),7])
rain[rain<0]<-0
weather_obs2$RAIN<-rain

plot_weather_obs2<-subset(weather_obs2,weather_obs2$TIMESTAMP>as.POSIXct("2014-07-01",tz=tzone) & weather_obs2$TIMESTAMP<as.POSIXct("2014-07-31",tz=tzone))

plot(daily_rain$rain~daily_rain$date,type='h',col='blue')


BoM_rain<-read.csv('microclimate/rainfall.csv')
dates2<-seq(ISOdate(2012,1,1,tz=tzone)-3600*12, ISOdate((2015),1,1,tz=tzone)-3600*13, by="days")
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
BoM_rain$dates<-dates2

plot(plot_weather_obs2$RAIN~plot_weather_obs2$TIMESTAMP,type='h',col='blue',ylim=c(0,50))
points(daily_rain$rain~daily_rain$date,type='h',col='red')
points(BoM_rain$nicheout.RAINFALL~BoM_rain$dates,type='h',col='green')

write.csv(weather_obs,'Field Data/weatherhawk_10min.csv')
write.csv(weather_obs2,'Field Data/weatherhawk_daily.csv')
