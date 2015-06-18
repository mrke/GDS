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
rain<-c(0,weather_obs[2:nrow(weather_obs),19]-weather_obs[1:(nrow(weather_obs)-1),19])
rain[rain<0]<-0
weather_obs$RAIN<-rain

plot_weather_obs<-subset(weather_obs,weather_obs$TIMESTAMP>as.POSIXct("2014-03-01 00:00:00") & weather_obs$TIMESTAMP<as.POSIXct("2014-04-15 00:00:00"))
#plot_weather_obs<-weather_obs
plot(plot_weather_obs$RAIN~plot_weather_obs$TIMESTAMP,type='h',col='blue')
plot(plot_weather_obs$AirTemp_C_Min~plot_weather_obs$TIMESTAMP,type='l')
points(plot_weather_obs$RAIN~plot_weather_obs$TIMESTAMP,type='h',col='blue')
plot(plot_weather_obs$AirTemp_C_Max~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$Solar_Avg~plot_weather_obs$TIMESTAMP,type='l')
points(plot_weather_obs$RAIN*100~plot_weather_obs$TIMESTAMP,type='h',col='blue')
plot(plot_weather_obs$Barometer_KPa~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$RH_Avg~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$ETo~plot_weather_obs$TIMESTAMP,type='l')
plot(plot_weather_obs$WindSpeed_ms_Avg~plot_weather_obs$TIMESTAMP,type='l')

