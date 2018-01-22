# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"

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
mean.max.surface=read.csv(paste0(dropbox,"csv summaries/mean.max.surface.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.5cms=read.csv(paste0(dropbox,"csv summaries/mean.max.5cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.15cms=read.csv(paste0(dropbox,"csv summaries/mean.max.15cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.30cms=read.csv(paste0(dropbox,"csv summaries/mean.max.30cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.50cms=read.csv(paste0(dropbox,"csv summaries/mean.max.50cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.1ms=read.csv(paste0(dropbox,"csv summaries/mean.max.1ms.csv"),stringsAsFactors = FALSE)[,-1]

mean.min.surface=read.csv(paste0(dropbox,"csv summaries/mean.min.surface.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.5cms=read.csv(paste0(dropbox,"csv summaries/mean.min.5cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.15cms=read.csv(paste0(dropbox,"csv summaries/mean.min.15cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.30cms=read.csv(paste0(dropbox,"csv summaries/mean.min.30cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.50cms=read.csv(paste0(dropbox,"csv summaries/mean.min.50cms.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.1ms=read.csv(paste0(dropbox,"csv summaries/mean.min.1ms.csv"),stringsAsFactors = FALSE)[,-1]

# format dates
mean.max.surface$date = as.POSIXct(mean.max.surface$date, format = "%Y-%m-%d")
mean.max.5cms$date = as.POSIXct(mean.max.5cms$date, format = "%Y-%m-%d")
mean.max.15cms$date = as.POSIXct(mean.max.15cms$date, format = "%Y-%m-%d")
mean.max.30cms$date = as.POSIXct(mean.max.30cms$date, format = "%Y-%m-%d")
mean.max.50cms$date = as.POSIXct(mean.max.50cms$date, format = "%Y-%m-%d")
mean.max.1ms$date = as.POSIXct(mean.max.1ms$date, format = "%Y-%m-%d")

mean.min.surface$date = as.POSIXct(mean.min.surface$date, format = "%Y-%m-%d")
mean.min.5cms$date = as.POSIXct(mean.min.5cms$date, format = "%Y-%m-%d")
mean.min.15cms$date = as.POSIXct(mean.min.15cms$date, format = "%Y-%m-%d")
mean.min.30cms$date = as.POSIXct(mean.min.30cms$date, format = "%Y-%m-%d")
mean.min.50cms$date = as.POSIXct(mean.min.50cms$date, format = "%Y-%m-%d")
mean.min.1ms$date = as.POSIXct(mean.min.1ms$date, format = "%Y-%m-%d")

# get monthly means 
monthly.mean.max.surface=aggregate(mean.max.surface,by=list(format(mean.max.surface$date,"%m-%y")), FUN=mean)
monthly.mean.max.5cms=aggregate(mean.max.5cms,by=list(format(mean.max.5cms$date,"%m-%y")), FUN=mean)
monthly.mean.max.15cms=aggregate(mean.max.15cms,by=list(format(mean.max.15cms$date,"%m-%y")), FUN=mean)
monthly.mean.max.30cms=aggregate(mean.max.30cms,by=list(format(mean.max.30cms$date,"%m-%y")), FUN=mean)
monthly.mean.max.50cms=aggregate(mean.max.50cms,by=list(format(mean.max.50cms$date,"%m-%y")), FUN=mean)
monthly.mean.max.1ms=aggregate(mean.max.1ms,by=list(format(mean.max.1ms$date,"%m-%y")), FUN=mean)

monthly.mean.min.surface=aggregate(mean.min.surface,by=list(format(mean.min.surface$date,"%m-%y")), FUN=mean)
monthly.mean.min.5cms=aggregate(mean.min.5cms,by=list(format(mean.min.5cms$date,"%m-%y")), FUN=mean)
monthly.mean.min.15cms=aggregate(mean.min.15cms,by=list(format(mean.min.15cms$date,"%m-%y")), FUN=mean)
monthly.mean.min.30cms=aggregate(mean.min.30cms,by=list(format(mean.min.30cms$date,"%m-%y")), FUN=mean)
monthly.mean.min.50cms=aggregate(mean.min.50cms,by=list(format(mean.min.50cms$date,"%m-%y")), FUN=mean)
monthly.mean.min.1ms=aggregate(mean.min.1ms,by=list(format(mean.min.1ms$date,"%m-%y")), FUN=mean)

datestart="2013-10-05"# earliest is "2013-10-01"
datefinish="2014-09-30"# latest is "2014-09-20"

# plot the burrow data
with(mean.max.surface,plot(date,mean.max.temperature, cex.axis=1.2, type='l',col='white', xaxs = "i",ylim=c(-5,75),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)
title(ylab=expression(temperature~(degree*C)),mgp=c(2,1,0), cex.lab = 1.5)


# create polygons for surface
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.surface$date)), as.POSIXlt(max(mean.max.surface$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.surface$merge <- format.POSIXct(mean.max.surface$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.surface2 <- full_join(df,mean.max.surface)
mean.max.surface2$merge <- as.POSIXct(mean.max.surface2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.surface$date)), as.POSIXlt(max(mean.min.surface$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.surface$merge <- format.POSIXct(mean.min.surface$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.surface2 <- full_join(df,mean.min.surface)
mean.min.surface2$merge <- as.POSIXct(mean.max.surface2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.surface2$merge, mean.min.surface2$mean.min.temperature, mean.max.surface2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='red', border = NA)
}

# create polygons for 5cm
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.5cms$date)), as.POSIXlt(max(mean.max.5cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.5cms$merge <- format.POSIXct(mean.max.5cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.5cms2 <- full_join(df,mean.max.5cms)
mean.max.5cms2$merge <- as.POSIXct(mean.max.5cms2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.5cms$date)), as.POSIXlt(max(mean.min.5cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.5cms$merge <- format.POSIXct(mean.min.5cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.5cms2 <- full_join(df,mean.min.5cms)
mean.min.5cms2$merge <- as.POSIXct(mean.max.5cms2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.5cms2$merge, mean.min.5cms2$mean.min.temperature, mean.max.5cms2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='orange', border = NA)
}

# create polygons for 15cm
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.15cms$date)), as.POSIXlt(max(mean.max.15cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.15cms$merge <- format.POSIXct(mean.max.15cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.15cms2 <- full_join(df,mean.max.15cms)
mean.max.15cms2$merge <- as.POSIXct(mean.max.15cms2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.15cms$date)), as.POSIXlt(max(mean.min.15cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.15cms$merge <- format.POSIXct(mean.min.15cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.15cms2 <- full_join(df,mean.min.15cms)
mean.min.15cms2$merge <- as.POSIXct(mean.max.15cms2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.15cms2$merge, mean.min.15cms2$mean.min.temperature, mean.max.15cms2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='brown', border = NA)
}


# create polygons for 30cm
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.30cms$date)), as.POSIXlt(max(mean.max.30cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.30cms$merge <- format.POSIXct(mean.max.30cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.30cms2 <- full_join(df,mean.max.30cms)
mean.max.30cms2$merge <- as.POSIXct(mean.max.30cms2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.30cms$date)), as.POSIXlt(max(mean.min.30cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.30cms$merge <- format.POSIXct(mean.min.30cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.30cms2 <- full_join(df,mean.min.30cms)
mean.min.30cms2$merge <- as.POSIXct(mean.max.30cms2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.30cms2$merge, mean.min.30cms2$mean.min.temperature, mean.max.30cms2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='dark green', border = NA)
}

# create polygons for 50cm
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.50cms$date)), as.POSIXlt(max(mean.max.50cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.50cms$merge <- format.POSIXct(mean.max.50cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.50cms2 <- full_join(df,mean.max.50cms)
mean.max.50cms2$merge <- as.POSIXct(mean.max.50cms2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.50cms$date)), as.POSIXlt(max(mean.min.50cms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.50cms$merge <- format.POSIXct(mean.min.50cms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.50cms2 <- full_join(df,mean.min.50cms)
mean.min.50cms2$merge <- as.POSIXct(mean.max.50cms2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.50cms2$merge, mean.min.50cms2$mean.min.temperature, mean.max.50cms2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='blue', border = NA)
}

# create polygons for 1m
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.1ms$date)), as.POSIXlt(max(mean.max.1ms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.1ms$merge <- format.POSIXct(mean.max.1ms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.1ms2 <- full_join(df,mean.max.1ms)
mean.max.1ms2$merge <- as.POSIXct(mean.max.1ms2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.1ms$date)), as.POSIXlt(max(mean.min.1ms$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.1ms$merge <- format.POSIXct(mean.min.1ms$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.1ms2 <- full_join(df,mean.min.1ms)
mean.min.1ms2$merge <- as.POSIXct(mean.max.1ms2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.min.1ms2$merge, mean.min.1ms2$mean.min.temperature, mean.max.1ms2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='black', border = NA)
}
# with(mean.max.surface,polygon(c(date,rev(mean.min.surface$date)),c(mean.max.temperature,rev(mean.min.surface$mean.min.temperature)),col='red', border=NA)) # plot the data
# with(mean.max.5cms,polygon(c(date,rev(mean.min.5cms$date)),c(mean.max.temperature,rev(mean.min.5cms$mean.min.temperature)),col='orange', border=NA)) # plot the data
# with(mean.max.15cms,polygon(c(date,rev(mean.min.15cms$date)),c(mean.max.temperature,rev(mean.min.15cms$mean.min.temperature)),col='brown', border=NA)) # plot the data
# with(mean.max.30cms,polygon(c(date,rev(mean.min.30cms$date)),c(mean.max.temperature,rev(mean.min.30cms$mean.min.temperature)),col='dark green', border=NA)) # plot the data
# with(mean.max.50cms,polygon(c(date,rev(mean.min.50cms$date)),c(mean.max.temperature,rev(mean.min.50cms$mean.min.temperature)),col='blue', border=NA)) # plot the data
# with(mean.max.1ms,polygon(c(date,rev(mean.min.1ms$date)),c(mean.max.temperature,rev(mean.min.1ms$mean.min.temperature)),col='black', border=NA)) # plot the data

with(monthly.mean.max.surface,points(date,mean.max.temperature, type='p',col='black', bg="red",pch=22)) # plot the data
with(monthly.mean.max.surface,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.5cms,points(date,mean.max.temperature, type='p',col='black', bg="orange",pch=22)) # plot the data
with(monthly.mean.max.5cms,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.15cms,points(date,mean.max.temperature, type='p',col='black',bg='brown',pch=22)) # plot the data
with(monthly.mean.max.15cms,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.30cms,points(date,mean.max.temperature, type='p',col='black',bg='dark green',pch=22)) # plot the data
with(monthly.mean.max.30cms,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.50cms,points(date,mean.max.temperature, type='p',col='black',bg='blue',pch=22)) # plot the data
with(monthly.mean.max.50cms,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.1ms,points(date,mean.max.temperature, type='p',col='black',bg='black',pch=22)) # plot the data
with(monthly.mean.max.1ms,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.surface,points(date,mean.min.temperature, type='p',col='black', bg="red",pch=22)) # plot the data
with(monthly.mean.min.surface,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.5cms,points(date,mean.min.temperature, type='p',col='black', bg="orange",pch=22)) # plot the data
with(monthly.mean.min.5cms,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.15cms,points(date,mean.min.temperature, type='p',col='black',bg='brown',pch=22)) # plot the data
with(monthly.mean.min.15cms,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.30cms,points(date,mean.min.temperature, type='p',col='black',bg='dark green',pch=22)) # plot the data
with(monthly.mean.min.30cms,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.50cms,points(date,mean.min.temperature, type='p',col='black',bg='blue',pch=22)) # plot the data
with(monthly.mean.min.50cms,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.1ms,points(date,mean.min.temperature, type='p',col='black',bg='black',pch=22)) # plot the data
with(monthly.mean.min.1ms,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

axis.POSIXct(side = 1, x = as.POSIXct(mean.max.1ms$date_time,tz=tzone),
  at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
  las = 2, cex.axis =1.3)
points(weather_obs$RAIN~weather_obs$TIMESTAMP,type='h',col='blue')
points(weather_obs2$RAIN~weather_obs2$TIMESTAMP,type='h',col='blue')

#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)
#points(max.weather_obs$AirTemp_C_Avg~max.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
#points(min.weather_obs$AirTemp_C_Avg~min.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
abline(0,0)
abline(43.3,0,lty=1)
abline(39.6,0,lty=2)
box()
