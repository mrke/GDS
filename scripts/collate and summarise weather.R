library(NicheMapR) # load the NicheMapR package to get the function for calculating vapour density from relative humidity and air temp
# to install NicheMapR, follow instructions at https://camelunimelb.wordpress.com/resources/ 
library(zoo)
library(stringr)
# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"
############################# weather data ###################################################

# read in weatherhawk data to get rainfall, humidity and temperature

# note that CR200Series_data2.dat has the date wrong, need to shift it one day back
#
weather.folder<-paste0(dropbox,"raw data/weather data/")

tzone<-paste0("Etc/GMT-",10) # doing it this way ignores daylight savings!

weather_obs<-as.data.frame(read.csv(paste(weather.folder,'CR200Series_data1.dat',sep=''),skip=1,stringsAsFactors=FALSE)) # read in data, skip first row
weather_obs<-weather_obs[-(1:2),] # remove first two rows of what got read in 
weather_obs$TIMESTAMP<-as.POSIXct(weather_obs$TIMESTAMP,format="%Y-%m-%d %H:%M:%S",tz=tzone) # format date
weather_obs<-subset(weather_obs,weather_obs$TIMESTAMP<as.POSIXct("2016-01-01 00:00:00",tz=tzone) & weather_obs$TIMESTAMP>as.POSIXct("2013-05-01 00:00:00",tz=tzone)) #remove incorrect dates
weather_obs$AirTemp_C_TMn<-as.POSIXct(weather_obs$AirTemp_C_TMn,format="%Y-%m-%d %H:%M:%S",tz=tzone) # format time stamp of time of min air temp
weather_obs$AirTemp_C_TMx<-as.POSIXct(weather_obs$AirTemp_C_TMx,format="%Y-%m-%d %H:%M:%S",tz=tzone) # format time stamp of time of max air temp
weather_obs$WindSpeed_ms_TMx<-as.POSIXct(weather_obs$WindSpeed_ms_TMx,format="%Y-%m-%d %H:%M:%S",tz=tzone) # format time stamp of time of max wind speed
# convert from text to numbers
weather_obs[,2:10]<-as.numeric(unlist(weather_obs[,2:10])) 
weather_obs[,12]<-as.numeric(unlist(weather_obs[,12]))
weather_obs[,14:16]<-as.numeric(unlist(weather_obs[,14:16]))
weather_obs[,18:19]<-as.numeric(unlist(weather_obs[,18:19]))
# remove erroneous rainfall after gaps in data set
for(i in 2:nrow(weather_obs)){
  if(weather_obs[i,2]-weather_obs[i-1,2]!=1){
    weather_obs[i,19]=10000 # put a big number in to mark a problem
  }
}
rain<-c(0,weather_obs[2:nrow(weather_obs),19]-weather_obs[1:(nrow(weather_obs)-1),19]) # make a vector of just the rainfall
rain[rain<0]<-0 # get rid of negative values
rain[rain>5000]<-0 # get rid of really big values
weather_obs$RAIN<-rain # put rain back into main table

weather_obs2<-as.data.frame(read.csv(paste(weather.folder,'CR200Series_data2.dat',sep=''),skip=1,stringsAsFactors=FALSE))
weather_obs2<-weather_obs2[-(1:2),]
weather_obs2[,2:8]<-as.numeric(unlist(weather_obs2[,2:8]))
weather_obs2$TIMESTAMP<-as.POSIXct(weather_obs2$TIMESTAMP,format="%Y-%m-%d %H:%M:%S",tz=tzone)

rain<-c(0,weather_obs2[2:nrow(weather_obs2),7]-weather_obs2[1:(nrow(weather_obs2)-1),7])
rain[rain<0]<-0
weather_obs2$RAIN<-rain

weather_obs$vd<-WETAIR(db = weather_obs$AirTemp_C_Avg, rh = weather_obs$RH_Avg)$vd # compute vapour density
max.weather_obs= aggregate(weather_obs,by=list(format(weather_obs$TIMESTAMP,"%d-%m-%Y")), FUN = max)
max.weather_obs<-max.weather_obs[order(max.weather_obs$TIMESTAMP),] 
min.weather_obs= aggregate(weather_obs,by=list(format(weather_obs$TIMESTAMP,"%d-%m-%Y")), FUN = min)
min.weather_obs<-min.weather_obs[order(min.weather_obs$TIMESTAMP),] 
mean.weather_obs= aggregate(weather_obs,by=list(format(weather_obs$TIMESTAMP,"%d-%m-%Y")), FUN = mean)
mean.weather_obs<-mean.weather_obs[order(mean.weather_obs$TIMESTAMP),] 
max.weather_obs$Group.1<-as.POSIXct(mean.weather_obs$Group.1,format="%d-%m-%Y")
min.weather_obs$Group.1<-as.POSIXct(mean.weather_obs$Group.1,format="%d-%m-%Y")
mean.weather_obs$Group.1<-as.POSIXct(mean.weather_obs$Group.1,format="%d-%m-%Y")

write.csv(weather_obs,paste0(dropbox,"/csv summaries/weather_obs.csv"))
write.csv(weather_obs2,paste0(dropbox,"/csv summaries/weather_obs2.csv"))
write.csv(max.weather_obs,paste0(dropbox,"/csv summaries/max.weather_obs.csv"))
write.csv(min.weather_obs,paste0(dropbox,"/csv summaries/min.weather_obs.csv"))
write.csv(mean.weather_obs,paste0(dropbox,"/csv summaries/mean.weather_obs.csv"))
