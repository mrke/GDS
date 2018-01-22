library(dplyr)
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
mean.max.surf=read.csv(paste0(dropbox,"csv summaries/mean.max.surf.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.mid=read.csv(paste0(dropbox,"csv summaries/mean.max.mid.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.deep=read.csv(paste0(dropbox,"csv summaries/mean.max.deep.csv"),stringsAsFactors = FALSE)[,-1]

mean.min.surf=read.csv(paste0(dropbox,"csv summaries/mean.min.surf.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.mid=read.csv(paste0(dropbox,"csv summaries/mean.min.mid.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.deep=read.csv(paste0(dropbox,"csv summaries/mean.min.deep.csv"),stringsAsFactors = FALSE)[,-1]

# format dates
mean.max.surf$date = as.POSIXct(mean.max.surf$date, format = "%Y-%m-%d")
mean.max.mid$date = as.POSIXct(mean.max.mid$date, format = "%Y-%m-%d")
mean.max.deep$date = as.POSIXct(mean.max.deep$date, format = "%Y-%m-%d")

mean.min.surf$date = as.POSIXct(mean.min.surf$date, format = "%Y-%m-%d")
mean.min.mid$date = as.POSIXct(mean.min.mid$date, format = "%Y-%m-%d")
mean.min.deep$date = as.POSIXct(mean.min.deep$date, format = "%Y-%m-%d")

# get monthly means 
monthly.mean.max.surf=aggregate(mean.max.surf,by=list(format(mean.max.surf$date,"%m-%y")), FUN=mean)
monthly.mean.max.mid=aggregate(mean.max.mid,by=list(format(mean.max.mid$date,"%m-%y")), FUN=mean)
monthly.mean.max.deep=aggregate(mean.max.deep,by=list(format(mean.max.deep$date,"%m-%y")), FUN=mean)

monthly.mean.min.surf=aggregate(mean.min.surf,by=list(format(mean.min.surf$date,"%m-%y")), FUN=mean)
monthly.mean.min.mid=aggregate(mean.min.mid,by=list(format(mean.min.mid$date,"%m-%y")), FUN=mean)
monthly.mean.min.deep=aggregate(mean.min.deep,by=list(format(mean.min.deep$date,"%m-%y")), FUN=mean)

datestart="2013-10-05"# earliest is "2013-10-01"
datefinish="2014-09-30"# latest is "2014-09-30"

# plot the burrow data
with(mean.max.surf,plot(date,mean.max.vd, cex.axis=1.2, xaxs = 'i', type='l',col='white',ylim=c(0,0.05),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$vd,rev(min.weather_obs$vd)),col='light grey', border=NA)
title(ylab=expression(paste("vapour density (kg m"^-3,")")),mgp=c(2.3,1,0), cex.lab = 1.5)

# create polygons for weather station vd
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
polys = polylims(max.weather_obs2$merge, min.weather_obs2$vd, max.weather_obs2$vd)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col = "light grey", border = NA)
}

# create polygons for weather surf vd
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.surf$date)), as.POSIXlt(max(mean.max.surf$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.surf$merge <- format.POSIXct(mean.max.surf$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.surf2 <- full_join(df,mean.max.surf)
mean.max.surf2$merge <- as.POSIXct(mean.max.surf2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.surf$date)), as.POSIXlt(max(mean.min.surf$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.surf$merge <- format.POSIXct(mean.min.surf$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.surf2 <- full_join(df,mean.min.surf)
mean.min.surf2$merge <- as.POSIXct(mean.min.surf2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.max.surf2$merge, mean.min.surf2$mean.min.vd, mean.max.surf2$mean.max.vd)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='red', border = NA)
}

# create polygons for weather mid vd
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.mid$date)), as.POSIXlt(max(mean.max.mid$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.mid$merge <- format.POSIXct(mean.max.mid$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.mid2 <- full_join(df,mean.max.mid)
mean.max.mid2$merge <- as.POSIXct(mean.max.mid2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.mid$date)), as.POSIXlt(max(mean.min.mid$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.mid$merge <- format.POSIXct(mean.min.mid$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.mid2 <- full_join(df,mean.min.mid)
mean.min.mid2$merge <- as.POSIXct(mean.min.mid2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.max.mid2$merge, mean.min.mid2$mean.min.vd, mean.max.mid2$mean.max.vd)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='orange', border = NA)
}

# create polygons for weather deep vd
ts <- seq.POSIXt(as.POSIXlt(min(mean.max.deep$date)), as.POSIXlt(max(mean.max.deep$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.max.deep$merge <- format.POSIXct(mean.max.deep$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.max.deep2 <- full_join(df,mean.max.deep)
mean.max.deep2$merge <- as.POSIXct(mean.max.deep2$merge, format = "%Y-%m-%d")

ts <- seq.POSIXt(as.POSIXlt(min(mean.min.deep$date)), as.POSIXlt(max(mean.min.deep$date)), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
mean.min.deep$merge <- format.POSIXct(mean.min.deep$date,'%Y-%m-%d')
df <- data.frame(merge=ts)
mean.min.deep2 <- full_join(df,mean.min.deep)
mean.min.deep2$merge <- as.POSIXct(mean.min.deep2$merge, format = "%Y-%m-%d")

# add the polygons
polys = polylims(mean.max.deep2$merge, mean.min.deep2$mean.min.vd, mean.max.deep2$mean.max.vd)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='light blue', border = NA)
}

#with(mean.max.surf,polygon(c(date,rev(mean.min.surf$date)),c(mean.max.vd,rev(mean.min.surf$mean.min.vd)),col='red', border=NA)) # plot the data
#with(mean.max.mid,polygon(c(date,rev(mean.min.mid$date)),c(mean.max.vd,rev(mean.min.mid$mean.min.vd)),col='orange', border=NA)) # plot the data
#with(mean.max.deep,polygon(c(date,rev(mean.min.deep$date)),c(mean.max.vd,rev(mean.min.deep$mean.min.vd)),col='light blue', border=NA)) # plot the data

with(monthly.mean.max.surf,points(date,mean.max.vd, type='p',col='black', bg="red",pch=22)) # plot the data
with(monthly.mean.max.surf,arrows(date,mean.max.vd-sd.max.vd,date,mean.max.vd+sd.max.vd, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.mid,points(date,mean.max.vd, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.max.mid,arrows(date,mean.max.vd-sd.max.vd,date,mean.max.vd+sd.max.vd, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.deep,points(date,mean.max.vd, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.max.deep,arrows(date,mean.max.vd-sd.max.vd,date,mean.max.vd+sd.max.vd, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.surf,points(date,mean.min.vd, type='p',col='black',bg='red',pch=22)) # plot the data
with(monthly.mean.min.surf,arrows(date,mean.min.vd-sd.min.vd,date,mean.min.vd+sd.min.vd, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.mid,points(date,mean.min.vd, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.min.mid,arrows(date,mean.min.vd-sd.min.vd,date,mean.min.vd+sd.min.vd, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.deep,points(date,mean.min.vd, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.min.deep,arrows(date,mean.min.vd-sd.min.vd,date,mean.min.vd+sd.min.vd, code=3, length=0.02, angle = 90,col='black')) # plot the data

axis.POSIXct(side = 1, x = as.POSIXct(mean.max.deep$date_time,tz=tzone),
  at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
  las = 2, cex.axis =1.3)
points(weather_obs$RAIN/5000~weather_obs$TIMESTAMP,type='h',col='blue')
points(weather_obs2$RAIN/5000~weather_obs2$TIMESTAMP,type='h',col='blue')
box()
#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$vd,rev(min.weather_obs$vd)),col='light grey', border=NA)
#points(max.weather_obs$vd~max.weather_obs$TIMESTAMP,type='l',col='blue', pch="+",lwd=1)
#points(min.weather_obs$vd~min.weather_obs$TIMESTAMP,type='l',col='blue', pch="+",lwd=1)
