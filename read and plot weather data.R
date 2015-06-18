# read in weatherhawk data

weather.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Weather/"


weather_obs<-as.data.frame(read.csv(paste(weather.folder,'CR200Series_data1.dat',sep=''),skip=1,stringsAsFactors=FALSE))
weather_obs<-weather_obs[-(1:2),]
weather_obs$TIMESTAMP<-as.POSIXct(weather_obs$TIMESTAMP,format="%Y-%m-%d %H:%M:%S")
weather_obs<-subset(weather_obs,weather_obs$TIMESTAMP<as.POSIXct("2016-01-01 00:00:00") & weather_obs$TIMESTAMP>as.POSIXct("2013-05-01 00:00:00"))
weather_obs$AirTemp_C_TMn<-as.POSIXct(weather_obs$AirTemp_C_TMn,format="%Y-%m-%d %H:%M:%S")
weather_obs$AirTemp_C_TMx<-as.POSIXct(weather_obs$AirTemp_C_TMx,format="%Y-%m-%d %H:%M:%S")
weather_obs$WindSpeed_ms_TMx<-as.POSIXct(weather_obs$WindSpeed_ms_TMx,format="%Y-%m-%d %H:%M:%S")

weather_obs[,2:10]<-as.numeric(unlist(weather_obs[,2:10]))
weather_obs[,12]<-as.numeric(unlist(weather_obs[,12]))
weather_obs[,14:16]<-as.numeric(unlist(weather_obs[,14:16]))
weather_obs[,18:19]<-as.numeric(unlist(weather_obs[,18:19]))

plot(weather_obs$AirTemp_C_Min~weather_obs$TIMESTAMP,type='l')
plot(weather_obs$AirTemp_C_Max~weather_obs$TIMESTAMP,type='l')
plot(weather_obs$RainYearly_mm~weather_obs$TIMESTAMP,type='h')
plot(weather_obs$Solar_Avg~weather_obs$TIMESTAMP,type='l')
plot(weather_obs$Barometer_KPa~weather_obs$TIMESTAMP,type='l')

