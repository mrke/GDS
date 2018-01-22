library(dplyr)
# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"
source('polylims.R')

############################# read in weather data ###################################################
weather_obs = read.csv(paste0(dropbox,"csv summaries/weather_obs.csv"), stringsAsFactors = FALSE)[,-1]
weather_obs2 = read.csv(paste0(dropbox,"csv summaries/weather_obs2.csv"), stringsAsFactors = FALSE)[,-1]
max.weather_obs = read.csv(paste0(dropbox,"csv summaries/max.weather_obs.csv"), stringsAsFactors = FALSE)[,-1]
min.weather_obs = read.csv(paste0(dropbox,"csv summaries/min.weather_obs.csv"), stringsAsFactors = FALSE)[,-1]
mean.weather_obs = read.csv(paste0(dropbox,"csv summaries/mean.weather_obs.csv"), stringsAsFactors = FALSE)[,-1]
weather_obs$TIMESTAMP = as.POSIXct(weather_obs$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
weather_obs2$TIMESTAMP = as.POSIXct(weather_obs2$TIMESTAMP, format = "%Y-%m-%d")
max.weather_obs$TIMESTAMP = as.POSIXct(max.weather_obs$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
min.weather_obs$TIMESTAMP = as.POSIXct(min.weather_obs$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
mean.weather_obs$TIMESTAMP = as.POSIXct(mean.weather_obs$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
max.weather_obs$Group.1 = as.POSIXct(max.weather_obs$Group.1, format = "%Y-%m-%d")
min.weather_obs$Group.1 = as.POSIXct(min.weather_obs$Group.1, format = "%Y-%m-%d")
mean.weather_obs$Group.1 = as.POSIXct(mean.weather_obs$Group.1, format = "%Y-%m-%d")

############################# read in copper model data and format dates ###################################################

# read in data
mean.max.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/mean.max.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.adult.partshades=read.csv(paste0(dropbox,"csv summaries/mean.max.adult.partshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.adult.fullshades=read.csv(paste0(dropbox,"csv summaries/mean.max.adult.fullshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.juv.fullsuns=read.csv(paste0(dropbox,"csv summaries/mean.max.juv.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.juv.partshades=read.csv(paste0(dropbox,"csv summaries/mean.max.juv.partshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.juv.fullshades=read.csv(paste0(dropbox,"csv summaries/mean.max.juv.fullshades.csv"),stringsAsFactors = FALSE)[,-1]

mean.min.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/mean.min.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.adult.partshades=read.csv(paste0(dropbox,"csv summaries/mean.min.adult.partshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.adult.fullshades=read.csv(paste0(dropbox,"csv summaries/mean.min.adult.fullshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.fullsuns=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.partshades=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.partshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.fullshades=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.fullshades.csv"),stringsAsFactors = FALSE)[,-1]

# format dates
mean.max.adult.fullsuns$date = as.POSIXct(mean.max.adult.fullsuns$date, format = "%Y-%m-%d")
mean.max.adult.partshades$date = as.POSIXct(mean.max.adult.partshades$date, format = "%Y-%m-%d")
mean.max.adult.fullshades$date = as.POSIXct(mean.max.adult.fullshades$date, format = "%Y-%m-%d")
mean.max.juv.fullsuns$date = as.POSIXct(mean.max.juv.fullsuns$date, format = "%Y-%m-%d")
mean.max.juv.partshades$date = as.POSIXct(mean.max.juv.partshades$date, format = "%Y-%m-%d")
mean.max.juv.fullshades$date = as.POSIXct(mean.max.juv.fullshades$date, format = "%Y-%m-%d")

mean.min.adult.fullsuns$date = as.POSIXct(mean.min.adult.fullsuns$date, format = "%Y-%m-%d")
mean.min.adult.partshades$date = as.POSIXct(mean.min.adult.partshades$date, format = "%Y-%m-%d")
mean.min.adult.fullshades$date = as.POSIXct(mean.min.adult.fullshades$date, format = "%Y-%m-%d")
mean.min.juv.fullsuns$date = as.POSIXct(mean.min.juv.fullsuns$date, format = "%Y-%m-%d")
mean.min.juv.partshades$date = as.POSIXct(mean.min.juv.partshades$date, format = "%Y-%m-%d")
mean.min.juv.fullshades$date = as.POSIXct(mean.min.juv.fullshades$date, format = "%Y-%m-%d")

# get monthly means 
monthly.mean.max.adult.fullsuns=aggregate(mean.max.adult.fullsuns,by=list(format(mean.max.adult.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.max.adult.partshades=aggregate(mean.max.adult.partshades,by=list(format(mean.max.adult.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.max.adult.fullshades=aggregate(mean.max.adult.fullshades,by=list(format(mean.max.adult.fullshades$date,"%m-%y")), FUN=mean)
monthly.mean.max.juv.fullsuns=aggregate(mean.max.juv.fullsuns,by=list(format(mean.max.juv.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.max.juv.partshades=aggregate(mean.max.juv.partshades,by=list(format(mean.max.juv.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.max.juv.fullshades=aggregate(mean.max.juv.fullshades,by=list(format(mean.max.juv.fullshades$date,"%m-%y")), FUN=mean)

monthly.mean.min.adult.fullsuns=aggregate(mean.min.adult.fullsuns,by=list(format(mean.min.adult.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.min.adult.partshades=aggregate(mean.min.adult.partshades,by=list(format(mean.min.adult.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.min.adult.fullshades=aggregate(mean.min.adult.fullshades,by=list(format(mean.min.adult.fullshades$date,"%m-%y")), FUN=mean)
monthly.mean.min.juv.fullsuns=aggregate(mean.min.juv.fullsuns,by=list(format(mean.min.juv.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.min.juv.partshades=aggregate(mean.min.juv.partshades,by=list(format(mean.min.juv.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.min.juv.fullshades=aggregate(mean.min.juv.fullshades,by=list(format(mean.min.juv.fullshades$date,"%m-%y")), FUN=mean)

datestart="2013-10-05"# earliest is "2013-10-01"
datefinish="2014-09-30"# latest is "2014-09-20"

# plot the adult data
with(mean.max.adult.fullsuns,plot(date,mean.max.temperature, cex.axis=1.2, type='l',col='white', xaxs = "i",ylim=c(-5,75),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
title(ylab=expression(temperature~(degree*C)),mgp=c(2,1,0), cex.lab = 1.5)

# create polygons for weather station temperature
ts <- seq.POSIXt(as.POSIXlt(min(max.weather_obs$Group.1)), as.POSIXlt(max(max.weather_obs$Group.1)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
max.weather_obs$merge <- format.POSIXct(max.weather_obs$Group.1,'%Y-%m-%d')
df <- data.frame(merge=ts)
max.weather_obs2 <- full_join(df,max.weather_obs)
max.weather_obs2$merge <- as.POSIXct(max.weather_obs2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(min.weather_obs$Group.1)), as.POSIXlt(max(min.weather_obs$Group.1)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
min.weather_obs$merge <- format.POSIXct(min.weather_obs$Group.1,'%Y-%m-%d')
df <- data.frame(merge=ts)
min.weather_obs2 <- full_join(df,min.weather_obs)
min.weather_obs2$merge <- as.POSIXct(min.weather_obs2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(max.weather_obs2$merge, min.weather_obs2$AirTemp_C_Avg, max.weather_obs2$AirTemp_C_Avg)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col = "light grey", border = NA)
}

# create polygons for adult full sun
ts <- seq.POSIXt(as.POSIXlt(min(mean.min.adult.fullsuns$date)), as.POSIXlt(max(mean.min.adult.fullsuns$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.adult.fullsuns$merge <- format.POSIXct(mean.min.adult.fullsuns$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.adult.fullsuns2 <- full_join(df,mean.min.adult.fullsuns)
mean.min.adult.fullsuns2$merge <- as.POSIXct(mean.min.adult.fullsuns2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.max.adult.fullsuns$date)), as.POSIXlt(max(mean.max.adult.fullsuns$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.adult.fullsuns$merge <- format.POSIXct(mean.max.adult.fullsuns$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.adult.fullsuns2 <- full_join(df,mean.max.adult.fullsuns)
mean.max.adult.fullsuns2$merge <- as.POSIXct(mean.max.adult.fullsuns2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.adult.fullsuns2$merge, mean.min.adult.fullsuns2$mean.min.temperature, mean.max.adult.fullsuns2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='red', border = NA)
}

# create polygons for adult part shade
ts <- seq.POSIXt(as.POSIXlt(min(mean.min.adult.partshades$date)), as.POSIXlt(max(mean.min.adult.partshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.adult.partshades$merge <- format.POSIXct(mean.min.adult.partshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.adult.partshades2 <- full_join(df,mean.min.adult.partshades)
mean.min.adult.partshades2$merge <- as.POSIXct(mean.min.adult.partshades2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.max.adult.partshades$date)), as.POSIXlt(max(mean.max.adult.partshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.adult.partshades$merge <- format.POSIXct(mean.max.adult.partshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.adult.partshades2 <- full_join(df,mean.max.adult.partshades)
mean.max.adult.partshades2$merge <- as.POSIXct(mean.max.adult.partshades2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.adult.partshades2$merge, mean.min.adult.partshades2$mean.min.temperature, mean.max.adult.partshades2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='orange', border = NA)
}


# create polygons for adult part shade
ts <- seq.POSIXt(as.POSIXlt(min(mean.min.adult.fullshades$date)), as.POSIXlt(max(mean.min.adult.fullshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.adult.fullshades$merge <- format.POSIXct(mean.min.adult.fullshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.adult.fullshades2 <- full_join(df,mean.min.adult.fullshades)
mean.min.adult.fullshades2$merge <- as.POSIXct(mean.min.adult.fullshades2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.max.adult.fullshades$date)), as.POSIXlt(max(mean.max.adult.fullshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.adult.fullshades$merge <- format.POSIXct(mean.max.adult.fullshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.adult.fullshades2 <- full_join(df,mean.max.adult.fullshades)
mean.max.adult.fullshades2$merge <- as.POSIXct(mean.max.adult.fullshades2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.adult.fullshades2$merge, mean.min.adult.fullshades2$mean.min.temperature, mean.max.adult.fullshades2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='light blue', border = NA)
}
#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)

#with(mean.max.adult.fullsuns,polygon(c(date,rev(mean.min.adult.fullsuns$date)),c(mean.max.temperature,rev(mean.min.adult.fullsuns$mean.min.temperature)),col='red', border=NA)) # plot the data
#with(mean.max.adult.partshades,polygon(c(date,rev(mean.min.adult.partshades$date)),c(mean.max.temperature,rev(mean.min.adult.partshades$mean.min.temperature)),col='orange', border=NA)) # plot the data
#with(mean.max.adult.fullshades,polygon(c(date,rev(mean.min.adult.fullshades$date)),c(mean.max.temperature,rev(mean.min.adult.fullshades$mean.min.temperature)),col='light blue', border=NA)) # plot the data

with(monthly.mean.max.adult.fullsuns,points(date,mean.max.temperature, type='p',col='black', bg="red",pch=22)) # plot the data
with(monthly.mean.max.adult.fullsuns,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.adult.partshades,points(date,mean.max.temperature, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.max.adult.partshades,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.adult.fullshades,points(date,mean.max.temperature, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.max.adult.fullshades,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.adult.fullsuns,points(date,mean.min.temperature, type='p',col='black',bg='red',pch=22)) # plot the data
with(monthly.mean.min.adult.fullsuns,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.adult.partshades,points(date,mean.min.temperature, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.min.adult.partshades,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.adult.fullshades,points(date,mean.min.temperature, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.min.adult.fullshades,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

axis.POSIXct(side = 1, x = as.POSIXct(mean.max.deep$date_time,tz=tzone),
  at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
  las = 2, cex.axis =1.3)
points(weather_obs$RAIN~weather_obs$TIMESTAMP,type='h',col='blue')
points(weather_obs2$RAIN~weather_obs2$TIMESTAMP,type='h',col='blue')

#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)
points(max.weather_obs$AirTemp_C_Avg~max.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
#points(min.weather_obs$AirTemp_C_Avg~min.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
abline(0,0)
abline(43,0,lty=1)
abline(39.6,0,lty=2)
abline(25.7,0,lty=2)
box()

# plot the juv data
with(mean.max.juv.fullsuns,plot(date,mean.max.temperature, cex.axis=1.2, type='l',col='white', xaxs = "i",ylim=c(-5,75),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
title(ylab=expression(temperature~(degree*C)),mgp=c(2,1,0), cex.lab = 1.5)

# create polygons for weather station temperature
ts <- seq.POSIXt(as.POSIXlt(min(max.weather_obs$Group.1)), as.POSIXlt(max(max.weather_obs$Group.1)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
max.weather_obs$merge <- format.POSIXct(max.weather_obs$Group.1,'%Y-%m-%d')
df <- data.frame(merge=ts)
max.weather_obs2 <- full_join(df,max.weather_obs)
max.weather_obs2$merge <- as.POSIXct(max.weather_obs2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(min.weather_obs$Group.1)), as.POSIXlt(max(min.weather_obs$Group.1)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
min.weather_obs$merge <- format.POSIXct(min.weather_obs$Group.1,'%Y-%m-%d')
df <- data.frame(merge=ts)
min.weather_obs2 <- full_join(df,min.weather_obs)
min.weather_obs2$merge <- as.POSIXct(min.weather_obs2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(max.weather_obs2$merge, min.weather_obs2$AirTemp_C_Avg, max.weather_obs2$AirTemp_C_Avg)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col = "light grey", border = NA)
}

# create polygons for juv full sun
ts <- seq.POSIXt(as.POSIXlt(min(mean.min.juv.fullsuns$date)), as.POSIXlt(max(mean.min.juv.fullsuns$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.juv.fullsuns$merge <- format.POSIXct(mean.min.juv.fullsuns$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.juv.fullsuns2 <- full_join(df,mean.min.juv.fullsuns)
mean.min.juv.fullsuns2$merge <- as.POSIXct(mean.min.juv.fullsuns2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.max.juv.fullsuns$date)), as.POSIXlt(max(mean.max.juv.fullsuns$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.juv.fullsuns$merge <- format.POSIXct(mean.max.juv.fullsuns$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.juv.fullsuns2 <- full_join(df,mean.max.juv.fullsuns)
mean.max.juv.fullsuns2$merge <- as.POSIXct(mean.max.juv.fullsuns2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.juv.fullsuns2$merge, mean.min.juv.fullsuns2$mean.min.temperature, mean.max.juv.fullsuns2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='red', border = NA)
}

# create polygons for juv part shade
ts <- seq.POSIXt(as.POSIXlt(min(mean.min.juv.partshades$date)), as.POSIXlt(max(mean.min.juv.partshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.juv.partshades$merge <- format.POSIXct(mean.min.juv.partshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.juv.partshades2 <- full_join(df,mean.min.juv.partshades)
mean.min.juv.partshades2$merge <- as.POSIXct(mean.min.juv.partshades2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.max.juv.partshades$date)), as.POSIXlt(max(mean.max.juv.partshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.juv.partshades$merge <- format.POSIXct(mean.max.juv.partshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.juv.partshades2 <- full_join(df,mean.max.juv.partshades)
mean.max.juv.partshades2$merge <- as.POSIXct(mean.max.juv.partshades2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.juv.partshades2$merge, mean.min.juv.partshades2$mean.min.temperature, mean.max.juv.partshades2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='orange', border = NA)
}


# create polygons for juv part shade
ts <- seq.POSIXt(as.POSIXlt(min(mean.min.juv.fullshades$date)), as.POSIXlt(max(mean.min.juv.fullshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.juv.fullshades$merge <- format.POSIXct(mean.min.juv.fullshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.juv.fullshades2 <- full_join(df,mean.min.juv.fullshades)
mean.min.juv.fullshades2$merge <- as.POSIXct(mean.min.juv.fullshades2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.max.juv.fullshades$date)), as.POSIXlt(max(mean.max.juv.fullshades$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.juv.fullshades$merge <- format.POSIXct(mean.max.juv.fullshades$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.juv.fullshades2 <- full_join(df,mean.max.juv.fullshades)
mean.max.juv.fullshades2$merge <- as.POSIXct(mean.max.juv.fullshades2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.juv.fullshades2$merge, mean.min.juv.fullshades2$mean.min.temperature, mean.max.juv.fullshades2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='light blue', border = NA)
}
#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)

#with(mean.max.juv.fullsuns,polygon(c(date,rev(mean.min.juv.fullsuns$date)),c(mean.max.temperature,rev(mean.min.juv.fullsuns$mean.min.temperature)),col='red', border=NA)) # plot the data
#with(mean.max.juv.partshades,polygon(c(date,rev(mean.min.juv.partshades$date)),c(mean.max.temperature,rev(mean.min.juv.partshades$mean.min.temperature)),col='orange', border=NA)) # plot the data
#with(mean.max.juv.fullshades,polygon(c(date,rev(mean.min.juv.fullshades$date)),c(mean.max.temperature,rev(mean.min.juv.fullshades$mean.min.temperature)),col='light blue', border=NA)) # plot the data

with(monthly.mean.max.juv.fullsuns,points(date,mean.max.temperature, type='p',col='black', bg="red",pch=22)) # plot the data
with(monthly.mean.max.juv.fullsuns,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.juv.partshades,points(date,mean.max.temperature, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.max.juv.partshades,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.juv.fullshades,points(date,mean.max.temperature, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.max.juv.fullshades,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.juv.fullsuns,points(date,mean.min.temperature, type='p',col='black',bg='red',pch=22)) # plot the data
with(monthly.mean.min.juv.fullsuns,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.juv.partshades,points(date,mean.min.temperature, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.min.juv.partshades,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.juv.fullshades,points(date,mean.min.temperature, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.min.juv.fullshades,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

axis.POSIXct(side = 1, x = as.POSIXct(mean.max.deep$date_time,tz=tzone),
  at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
  las = 2, cex.axis =1.3)
points(weather_obs$RAIN~weather_obs$TIMESTAMP,type='h',col='blue')
points(weather_obs2$RAIN~weather_obs2$TIMESTAMP,type='h',col='blue')

#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)
points(max.weather_obs$AirTemp_C_Avg~max.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
#points(min.weather_obs$AirTemp_C_Avg~min.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
abline(0,0)
abline(43,0,lty=1)
abline(39.6,0,lty=2)
abline(25.7,0,lty=2)
box()