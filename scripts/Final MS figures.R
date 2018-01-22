
#Figure 2

# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"

time_budget=read.csv(paste0(dropbox,"csv summaries/time_budgets.csv"), stringsAsFactors = FALSE)[,-1]
time_budget$timeday[time_budget$timeday == 'duskdawn' & time_budget$hour > 12] <- "dusk"
time_budget$timeday[time_budget$timeday == 'duskdawn' & time_budget$hour < 12] <- "dawn"

tzone=paste0("Etc/GMT-10") # doing it this way ignores daylight savings!
time_budget$date=as.POSIXct(time_budget$days.i.,format="%d/%m/%Y",tz=tzone)
time_budget$date_time=time_budget$date+time_budget$hour*60

tpref_upper<-quantile(time_budget$Tb,0.75,na.rm=TRUE)
tpref_lower<-quantile(time_budget$Tb,0.25,na.rm=TRUE)
maxTb=max(time_budget$Tb,na.rm=TRUE)
minTb=min(time_budget$Tb,na.rm=TRUE)

pdf(paste("Figure2.pdf",sep=""),paper="A4",width=15,height=8)

# histograms across all times of day
brks<-seq(0,80,0.5)
hist(x=time_budget$Tb,breaks=brks,col='NA',border="NA",main="",ylim=c(0,3000), xlab = expression(temperature~(degree*C)), cex.lab = 1.5, cex.axis = 1.5, yaxs = 'i', xaxs = 'i')
hist(x=time_budget$c.shd,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$c.sun,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$c.part,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$s.5cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.15cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.30cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.50cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.1m,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$Tb,breaks=brks,col='orange',border="NA",add=TRUE)
points(rbind(c(maxTb,0),c(maxTb,10000)),type='l',lty=2,col='red')
points(rbind(c(minTb,0),c(minTb,10000)),type='l',lty=2,col='blue')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='black')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='black')
text(minTb-1, 2500, "voluntary minimum", col = 'blue', srt=90, cex = 1.2)
text(maxTb-1, 2500, "voluntary maximum", col = 'red', srt=90, cex = 1.2)
text(tpref_upper-2, 2500, "preferred temperature", srt=90, col = 'black', cex = 1.2)

            legend("topright", pch = rep(15,3), inset=c(.1,0), col=rev(c("black", 
  "grey", "orange")), legend = rev(c("copper models",
    "underground", "body temperature")),
   bty = 'n', cex = 1.5)
box()

dev.off()



# bar charts
counts <- table(time_budget$state, time_budget$timeday)
props <- prop.table(counts, margin = 2)


counts <- table(time_budget$state, time_budget$timeday)
order=c(3,4,5,1,2,6)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,5]),][,-5]

pdf(paste("Figure3.pdf",sep=""),paper="A4",width=15,height=8)

barplot(props, ylim = c(0,1.5), cex.lab = 1.5, cex = 1.5, cex.axis =1.5, tck=0.01, yaxs = "i", ylab = "proportion of time", main="",
  xlab="", col=c("black", "dark grey", "red", "orange", "blue", "light blue"))

legend("topright", pch = rep(15,3), col=rev(c("black", "dark grey", "red")), legend = rev(c("inactive deep","inactive shallow", "active deep")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.14, .005))
legend("topleft", pch = rep(15,3), col=rev(c("orange", "blue", "light blue")), legend = rev(c("active shallow" , "entrance", "surface")),
  bty = 'n', cex = 1.5 ,pt.cex = 1.5,inset=c(0.14, .005))
text(x = .7, y = 1.1, "1 hour", cex = 1.5)
text(x = 1.9, y = 1.1, "11.5 hours", cex = 1.5)
text(x = 3.1, y = 1.1, "1 hour", cex = 1.5)
text(x = 4.3, y = 1.1, "10.5 hours", cex = 1.5)
box()
dev.off()

# Figure 4

library(dplyr)
# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"
source('polylims.R')

pdf(paste("Figure4.pdf",sep=""),paper="A4r",width=15,height=8)

par(mfrow = c(3,1))
par(mar = c(1, 4, 1, 1))
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

mean.min.adult.fullsuns$date = as.POSIXct(mean.min.adult.fullsuns$date, format = "%Y-%m-%d")
mean.min.adult.partshades$date = as.POSIXct(mean.min.adult.partshades$date, format = "%Y-%m-%d")
mean.min.adult.fullshades$date = as.POSIXct(mean.min.adult.fullshades$date, format = "%Y-%m-%d")

# get monthly means 
monthly.mean.max.adult.fullsuns=aggregate(mean.max.adult.fullsuns,by=list(format(mean.max.adult.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.max.adult.partshades=aggregate(mean.max.adult.partshades,by=list(format(mean.max.adult.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.max.adult.fullshades=aggregate(mean.max.adult.fullshades,by=list(format(mean.max.adult.fullshades$date,"%m-%y")), FUN=mean)

monthly.mean.min.adult.fullsuns=aggregate(mean.min.adult.fullsuns,by=list(format(mean.min.adult.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.min.adult.partshades=aggregate(mean.min.adult.partshades,by=list(format(mean.min.adult.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.min.adult.fullshades=aggregate(mean.min.adult.fullshades,by=list(format(mean.min.adult.fullshades$date,"%m-%y")), FUN=mean)

datestart="2013-10-05"# earliest is "2013-10-01"
datefinish="2014-09-30"# latest is "2014-09-20"

# plot the adult data
with(mean.max.adult.fullsuns,plot(date,mean.max.temperature, cex.axis=1.2, type='l',col='white', xaxs = "i",ylim=c(-5,75),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
title(ylab="operative temperature (°C)",mgp=c(2,1,0), cex.lab = 1.5, line = 2.5)

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
  las = 2, cex.axis =1.3, col.axis = "white")
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


legend("topright", pch = rep(15,3), col=rev(c("light blue", "orange", "red")), legend = rev(c("deep shade","part shade", "no shade")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.20, -.05))

legend("topright", pch = 15, col=rev(c("grey")), legend = rev(c("weather station air temp.")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.00, -.05))
legend("topright", pch = "+", col=rev(c("blue")), legend = rev(c( "mean max. air temp.")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.31, .03))
legend("topright", pch = "|", col=rev(c("blue")), legend = rev(c( "rainfall (mm)")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.35, -.03))

legend("topleft", legend = "a)", bty = 'n', cex = 1.5, inset=c(-0.02, -0.05))

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
datefinish="2014-09-30"# latest is "2014-09-20"

# plot the burrow data
with(mean.max.surf,plot(date,mean.max.temperature, cex.axis=1.2, xaxs = 'i', type='l',col='white',ylim=c(-5,75),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)
title(ylab="burrow temperature (°C)",mgp=c(2,1,0), cex.lab = 1.5, line = 2.5)

# create polygons for weather station air temperature
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

# create polygons for weather surf temperature
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
polys = polylims(mean.max.surf2$merge, mean.min.surf2$mean.min.temperature, mean.max.surf2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='red', border = NA)
}

# create polygons for weather mid temperature
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
polys = polylims(mean.max.mid2$merge, mean.min.mid2$mean.min.temperature, mean.max.mid2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='orange', border = NA)
}

# create polygons for weather deep temperature
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
polys = polylims(mean.max.deep2$merge, mean.min.deep2$mean.min.temperature, mean.max.deep2$mean.max.temperature)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='light blue', border = NA)
}

#with(mean.max.surf,polygon(c(date,rev(mean.min.surf$date)),c(mean.max.temperature,rev(mean.min.surf$mean.min.temperature)),col='red', border=NA)) # plot the data
#with(mean.max.mid,polygon(c(date,rev(mean.min.mid$date)),c(mean.max.temperature,rev(mean.min.mid$mean.min.temperature)),col='orange', border=NA)) # plot the data
#with(mean.max.deep,polygon(c(date,rev(mean.min.deep$date)),c(mean.max.temperature,rev(mean.min.deep$mean.min.temperature)),col='light blue', border=NA)) # plot the data

with(monthly.mean.max.surf,points(date,mean.max.temperature, type='p',col='black', bg="red",pch=22)) # plot the data
with(monthly.mean.max.surf,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.mid,points(date,mean.max.temperature, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.max.mid,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.deep,points(date,mean.max.temperature, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.max.deep,arrows(date,mean.max.temperature-sd.max.temperature,date,mean.max.temperature+sd.max.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.surf,points(date,mean.min.temperature, type='p',col='black',bg='red',pch=22)) # plot the data
with(monthly.mean.min.surf,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.mid,points(date,mean.min.temperature, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.min.mid,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.deep,points(date,mean.min.temperature, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.min.deep,arrows(date,mean.min.temperature-sd.min.temperature,date,mean.min.temperature+sd.min.temperature, code=3, length=0.02, angle = 90,col='black')) # plot the data

axis.POSIXct(side = 1, x = as.POSIXct(mean.max.deep$date_time,tz=tzone),
  at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
  las = 2, cex.axis =1.3, col.axis = "white")
points(weather_obs$RAIN~weather_obs$TIMESTAMP,type='h',col='blue')
points(weather_obs2$RAIN~weather_obs2$TIMESTAMP,type='h',col='blue')

#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$AirTemp_C_Avg,rev(min.weather_obs$AirTemp_C_Avg)),col='light grey', border=NA)
#points(max.weather_obs$AirTemp_C_Avg~max.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
#points(min.weather_obs$AirTemp_C_Avg~min.weather_obs$TIMESTAMP,type='p',col='blue', pch="+",lwd=1)
abline(0,0)
abline(43.3,0,lty=1)
abline(39.6,0,lty=2)
abline(25.7,0,lty=2)
box()

legend("topright", pch = rep(15,3), col=rev(c("light blue", "orange", "red")), legend = rev(c("deep","middle", "shallow")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.21, -.05))
#legend("topright", pch = rep(15,1), col=rev(c("grey")), legend = rev(c("weather station")),
  #bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.05, -.05))


legend("topright", pch = 15, col=rev(c("grey")), legend = rev(c("weather station air temp.")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.00, -.05))
legend("topright", pch = "|", col=rev(c("blue")), legend = rev(c( "rainfall (mm)")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.35, -.03))
legend("topleft", legend = "b)", bty = 'n', cex = 1.5, inset=c(-0.02, -0.05))

############################# read in copper model data and format dates ###################################################
par(mar = c(3, 4, 1, 1))

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
datefinish="2014-09-30"# latest is "2014-09-20"

# plot the burrow data
with(mean.max.surf,plot(date,mean.max.humidity, cex.axis=1.1, xaxs = "i", type='l',col='white', ylim=c(0,130),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
#polygon(c(max.weather_obs$TIMESTAMP,rev(min.weather_obs$TIMESTAMP)),c(max.weather_obs$RH_Avg,rev(min.weather_obs$RH_Avg)),col='light grey', border=NA)
title(ylab="burrow humidity (%)",mgp=c(2.3,1,0), cex.lab = 1.5, line = 2.5)

# create polygons for weather station humidity
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
polys = polylims(max.weather_obs2$merge, min.weather_obs2$RH_Avg, max.weather_obs2$RH_Avg)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col = "light grey", border = NA)
}

# create polygons for weather surf humidity
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
polys = polylims(mean.max.surf2$merge, mean.min.surf2$mean.min.humidity, mean.max.surf2$mean.max.humidity)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='red', border = NA)
}

# create polygons for weather mid humidity
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
polys = polylims(mean.max.mid2$merge, mean.min.mid2$mean.min.humidity, mean.max.mid2$mean.max.humidity)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='orange', border = NA)
}

# create polygons for weather deep humidity
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
polys = polylims(mean.max.deep2$merge, mean.min.deep2$mean.min.humidity, mean.max.deep2$mean.max.humidity)
for(i in 1:length(polys)){
	polygon(polys[[i]]$x, polys[[i]]$y, col='light blue', border = NA)
}

#with(mean.max.surf,polygon(c(date,rev(mean.min.surf$date)),c(mean.max.humidity,rev(mean.min.surf$mean.min.humidity)),col='red', border=NA)) # plot the data
#with(mean.max.mid,polygon(c(date,rev(mean.min.mid$date)),c(mean.max.humidity,rev(mean.min.mid$mean.min.humidity)),col='orange', border=NA)) # plot the data
#with(mean.max.deep,polygon(c(date,rev(mean.min.deep$date)),c(mean.max.humidity,rev(mean.min.deep$mean.min.humidity)),col='light blue', border=NA)) # plot the data

with(monthly.mean.max.surf,points(date,mean.max.humidity, type='p',col='black', bg="red",pch=22)) # plot the data
with(monthly.mean.max.surf,arrows(date,mean.max.humidity-sd.max.humidity,date,mean.max.humidity+sd.max.humidity, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.mid,points(date,mean.max.humidity, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.max.mid,arrows(date,mean.max.humidity-sd.max.humidity,date,mean.max.humidity+sd.max.humidity, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.max.deep,points(date,mean.max.humidity, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.max.deep,arrows(date,mean.max.humidity-sd.max.humidity,date,mean.max.humidity+sd.max.humidity, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.surf,points(date,mean.min.humidity, type='p',col='black',bg='red',pch=22)) # plot the data
with(monthly.mean.min.surf,arrows(date,mean.min.humidity-sd.min.humidity,date,mean.min.humidity+sd.min.humidity, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.mid,points(date,mean.min.humidity, type='p',col='black',bg='orange',pch=22)) # plot the data
with(monthly.mean.min.mid,arrows(date,mean.min.humidity-sd.min.humidity,date,mean.min.humidity+sd.min.humidity, code=3, length=0.02, angle = 90,col='black')) # plot the data

with(monthly.mean.min.deep,points(date,mean.min.humidity, type='p',col='black',bg='light blue',pch=22)) # plot the data
with(monthly.mean.min.deep,arrows(date,mean.min.humidity-sd.min.humidity,date,mean.min.humidity+sd.min.humidity, code=3, length=0.02, angle = 90,col='black')) # plot the data

axis.POSIXct(side = 1, x = as.POSIXct(mean.max.deep$date_time,tz=tzone),
  at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
  las = 1, cex.axis =1.3)
points(weather_obs$RAIN~weather_obs$TIMESTAMP,type='h',col='blue')
points(weather_obs2$RAIN~weather_obs2$TIMESTAMP,type='h',col='blue')
box()
abline(h=0)

legend("topright", pch = rep(15,3), col=rev(c("light blue", "orange", "red")), legend = rev(c("deep","middle", "shallow")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.21, -.05))
#legend("topright", pch = rep(15,1), col=rev(c("grey")), legend = rev(c("weather station")),
  #bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.05, -.05))

legend("topright", pch = 15, col=rev(c("grey")), legend = rev(c("weather station air temp.")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.00, -.05))
legend("topright", pch = "|", col=rev(c("blue")), legend = rev(c( "rainfall (mm)")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.35, -.03))
legend("topleft", legend = "c)", bty = 'n', cex = 1.5, inset=c(-0.02, -0.05))
dev.off()



# Figure 5

pdf(paste("Figure5.pdf",sep=""),paper="A4",width=15,height=5)


par(mfrow=c(1,2))

warm <- 0

potential.act <- time_budget[,c(1, 2, 15, 16, 17, 21, 6, 7, 8)]
potential.act$c.sun[potential.act$c.sun+warm>maxTb]<-0 # zero too hot
potential.act$c.sun[potential.act$c.sun+warm<minTb]<-0 # zero too cold
potential.act$c.sun[potential.act$c.sun!=0]<-1 # 1 for just right
potential.act$c.shd[potential.act$c.sun==1]<-0 # zero suitable in open
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm>maxTb]<-0 # zero too hot in shade
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm<minTb]<-0 # zero too cold in shade
potential.act$c.shd[potential.act$c.shd!=0]<-1 # 1 for just right in shade
potential.act$b.surf[potential.act$c.sun==1 | potential.act$c.shd==1]<-0 # zero suitable on surface
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm<minTb]<-0 # zero too cold shallow
potential.act$b.surf[potential.act$b.surf!=0]<-1 # 1 for just right shallow
potential.act$b.deep[potential.act$c.sun==1 | potential.act$c.shd==1 | potential.act$b.surf==1]<-0 # zero suitable on surface or shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm<minTb]<-0 # zero too cold shallow
potential.act$b.deep[potential.act$b.deep!=0]<-1 # 1 for just right shallow
potential.act$state <- "inactive.deep"
potential.act$state[potential.act$c.sun==1]<-"active.open"
potential.act$state[potential.act$c.shd==1]<-"active.shade"
potential.act$state[potential.act$b.surf==1]<-"active.shallow"


counts <- table(potential.act$state, potential.act$timeday)
order=c(4, 3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,5]),][,-5]


barplot(props, ylim = c(0,1.2), cex.lab = 1.1, cex = 1.1, cex.axis =1.1, tck=0.01, yaxs = "i", ylab = "proportion of time",
  xlab="", col=c("black", "orange", "blue", "light blue") )
#barplot(props, ylim = c(0,1.2), cex.lab = 1.3, cex = 1.3, cex.axis =1.3, tck=0.01, yaxs = "i", ylab = "proportion of time",
#  xlab="", col=c("black", "orange", "blue", "light blue"), legend = c("inactive deep","active shallow", "active shade",  "active open"), 
#  args.legend = list(x = "topright",cex=1.3,pt.cex = 1, bty = "n", inset=c(0.01, .005)) )
box()

legend("topright", pch = rep(15,2), col=rev(c("black", "orange")), legend = rev(c("inactive deep","active shallow")),
  cex = 1, pt.cex = 1, bty = "n", inset=c(0.01, .004))
legend("topleft", pch = rep(15,2), col=rev(c("blue", "light blue")), legend = rev(c("active shade",  "active open")),
  cex = 1, pt.cex = 1, bty = "n", inset=c(0.01, .004))

warm <- 2.8

potential.act <- time_budget[,c(1, 2, 15, 16, 17, 21, 6, 7, 8)]
potential.act$c.sun[potential.act$c.sun+warm>maxTb]<-0 # zero too hot
potential.act$c.sun[potential.act$c.sun+warm<minTb]<-0 # zero too cold
potential.act$c.sun[potential.act$c.sun!=0]<-1 # 1 for just right
potential.act$c.shd[potential.act$c.sun==1]<-0 # zero suitable in open
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm>maxTb]<-0 # zero too hot in shade
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm<minTb]<-0 # zero too cold in shade
potential.act$c.shd[potential.act$c.shd!=0]<-1 # 1 for just right in shade
potential.act$b.surf[potential.act$c.sun==1 | potential.act$c.shd==1]<-0 # zero suitable on surface
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm<minTb]<-0 # zero too cold shallow
potential.act$b.surf[potential.act$b.surf!=0]<-1 # 1 for just right shallow
potential.act$b.deep[potential.act$c.sun==1 | potential.act$c.shd==1 | potential.act$b.surf==1]<-0 # zero suitable on surface or shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm<minTb]<-0 # zero too cold shallow
potential.act$b.deep[potential.act$b.deep!=0]<-1 # 1 for just right shallow
potential.act$state <- "inactive.deep"
potential.act$state[potential.act$c.sun==1]<-"active.open"
potential.act$state[potential.act$c.shd==1]<-"active.shade"
potential.act$state[potential.act$b.surf==1]<-"active.shallow"


counts <- table(potential.act$state, potential.act$timeday)
order=c(4, 3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,5]),][,-5]


barplot(props, ylim = c(0,1.2), cex.lab = 1.1, cex = 1.1, cex.axis =1.1, tck=0.01, yaxs = "i", ylab = "proportion of time",
  xlab="", col=c("black", "orange", "blue", "light blue") )
box()

dev.off()

# Figure 6

# hourly summaries
pdf(paste("Figure6.pdf",sep=""),paper="A4",width=15,height=5)

plot(c.sun~hour,data=time_budget,cex=0.5,pch=16,main='', ylab="temperature (°C)", xlab = "hour of day", cex.lab = 1.3, cex.axis = 1.3)
points(c.shd~hour,data=time_budget,cex=0.5,pch=16)
points(c.part~hour,data=time_budget,cex=0.5,pch=16)
points(s.5cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.15cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.30cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.50cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.1m~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(Tb~hour,data=time_budget,col='orange',cex=0.5)
#abline(tpref_lower,0,col='red',lty=2,lwd=2)
#abline(tpref_upper,0,col='red',lty=2,lwd=2)
legend("topleft", pch = rep(16,3), col=rev(c("black", "grey", "orange")), legend = c("body temperature","burrow", "surface"),
  bty = 'n', cex = 1.2, pt.cex = 1.2,inset=c(0.10, .05))

dev.off()



# Figure S2

options(stringsAsFactors = FALSE)
loc <- "Nyrripi, Northern Territory, Australia" # type in a location here, used if option 1 is chosen above
longlat <- dismo::geocode(loc)[3:4] # assumes first geocode match is correct
if(nrow(longlat>1)){longlat<-longlat[1,]}
x <- t(as.matrix(as.numeric(c(longlat[1,1],longlat[1,2]))))
nyears=1

scenarios=c("Access 1.3", "Access 1.0", "CanESM2","GDFLCM3", "HadGEM2-CC", "HadGEM2-ES")
years=c(2050,2070)




# diff spline function
getdiff<-function(diffs,grid){
  diff1<-(unlist(diffs[1])+unlist(diffs[12]))/2
  
  # generate list of days
  for(ys in 1:nyears){
    day<-c(1,15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5, 365)
    day.leap<-c(1,15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5, 366)
    if(ys==1){
      days2=day
      days=day
    }else{
      days2=c(days2,(day+365*(ys-1)))
      days=c(days,day)
    }
  }
  
  if(is.na(diffs[1])==TRUE){
    # find the nearest cell with data
    NArem<-grid[[1]]
    NArem<-Which(!is.na(NArem), cells=TRUE)
    dist<-distanceFromPoints(maxTst05[[1]],x)
    distNA<-extract(dist,NArem)
    cellsR<-cbind(distNA,NArem)
    distmin<-which.min(distNA)
    cellrep<-cellsR[distmin,2]
    diffs<-extract(maxTst05,cellrep)
    diff1<-(unlist(diffs[1])+unlist(diffs[12]))/2
  }
  diffs3=rep(c(diff1,diffs,diff1),nyears)
  days_diffs<-data.frame(matrix(NA, nrow = nyears*14, ncol = 3))
  days_diffs[,1]<-days
  days_diffs[,3]<-days2
  days_diffs[,2]<-diffs3
  colnames(days_diffs)<-c("days","diffs","new_day")
  
  # interpolate monthly differences
  f<-approxfun(x=days_diffs$new_day, y=days_diffs$diffs)
  xx<-seq(1,max(days2),1)
  sp_diff<-f(xx)
  return(sp_diff)
}

for(i in 1:length(scenarios)){
  for(j in 1:length(years)){
    scenario=scenarios[i]
    year=years[j]
    
    ########### Max and Min Air Temps ################
    
    load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","maxTst05_",scenario,"_",year,".Rda",sep="")) #maxTst05
    
    diffs<-extract(maxTst05,x)
    TMAXX_diff<-getdiff(diffs,maxTst05)
    
    load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","minTst05_",scenario,"_",year,".Rda",sep="")) #minTst05
    
    diffs<-extract(minTst05,x)
    TMINN_diff<-getdiff(diffs,minTst05)
    
#     ################ RH ############################
#     
#     load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","RHst05_",scenario,"_",year,".Rda",sep="")) #maxTst05
#     
#     diffs<-extract(RHst05,x)
#     RH_diff<-getdiff(diffs,RHst05)
#     
#     ################ wind ############################
#     
#     load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","PT_VELst05_",scenario,"_",year,".Rda",sep=""))
#     
#     diffs<-extract(PT_VELst05,x)
#     WIND_diff<-getdiff(diffs,PT_VELst05)
#     
#     ############# SOLAR/CLOUD COVER ##################
#     
#     load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","SOLCst05_",scenario,"_",year,".Rda",sep=""))
#     
#     diffs<-extract(SOLCst05,x)
#     SOLAR_diff<-getdiff(diffs,SOLCst05)
    
    results<-as.data.frame(cbind(seq(1,365), year, round(TMAXX_diff,1),round(TMINN_diff,1)) )
    results<-cbind(results,scenario)
    if(i==1 & j==1){
      allresults<-results
    }else{
      allresults<-rbind(allresults,results)
    }
  }
}
colnames(allresults)<-c("day","year","dTmax","dTmin","scenario")
par(mfrow=c(3,2))
for(j in 1:2){
for(i in 1:length(scenarios)){
plot(dTmax~day,type='l',data=subset(allresults,year==years[j] & scenario==scenarios[i]),main=paste(scenarios[i],years[j]),ylim=c(0,6),col='red', ylab = expression(Delta~(degree*C)))
points(dTmin~day,type='l',data=subset(allresults,year==years[j] & scenario==scenarios[i]),main=paste(scenarios[i],years[j]),ylim=c(0,6),col='blue')
if(i == 1){
legend(x = 20, y = 5, legend = c("minimum", "maximum"), col = c("blue", "red"), lty = 1, bty = 'n')
}
}
}

# Figure S3

pdf(paste("FigureS3.pdf",sep=""),paper="A4r",width=15,height=5)


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
abline(25.7,0,lty=2)
box()

legend("topright", pch = rep(15,3), col=c("red", "orange", "brown"), legend = c("0 cms", "5 cms", "15 cms"),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.21, -.02))

legend("topright", pch = rep(15,3), col=c("dark green", "blue", "black"), legend = c("30 cms", "50 cms", "100 cms"),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.05, -.02))

legend("topright", pch = "|", col=rev(c("blue")), legend = rev(c( "rainfall (mm)")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.36, -.03))
dev.off()







### Figure S4

pdf(paste("FigureS4.pdf",sep=""),paper="A4r",width=15,height=5)

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

legend("topright", pch = rep(15,3), col=rev(c("grey", "light blue", "orange", "red")), legend = rev(c("weather station", "deep","middle", "shallow")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.1, -.05))
#legend("topright", pch = rep(15,1), col=rev(c("grey")), legend = rev(c("weather station")),
 # bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.05, -.05))

legend("topright", pch = "|", col=rev(c("blue")), legend = rev(c( "rainfall (mm)")),
  bty = 'n', cex = 1.5, pt.cex = 1.5,inset=c(0.35, -.03))
dev.off()


## Figure S5

pdf(paste("FigureS5.pdf",sep=""),paper="A4r",width=18,height=10)


# plot for each day for each lizard

start <- 15
finish <- 24

ndays <- c(19, 28, 27, 6, 4, 12, 26, 3, 27, 25, 21, 6, 20, 22, 3, 21, 50)
par(mar=c(1,1,2,1))
#par(oma=c(2,1,1,1))

lizards <- unique(time_budget$animalID)

lizards <- lizards[order(-ndays)]
ndays <- ndays[order(-ndays)]
alldays <- 11/10 - 14/12
tzone<-paste("Etc/GMT+",10,sep="") 

alldays<-seq(ISOdate(2013,10,11,tz=tzone)-3600*12, ISOdate(2013,12,17,tz=tzone)-3600*13, by="days")
par(mfrow=c(10,10))
for(i in 1:length(lizards)){
  time_budget_id <- subset(time_budget, animalID == lizards[i])
  days <- unique(time_budget_id$date)  
  #par(mfrow=c(ceiling(sqrt(length(days))),ceiling(sqrt(length(days)))))
  #par(mfrow=c(7,10))
  #par(mfrow=c(2,4))
  count <- 1
  for(j in 1:length(alldays)){
    
    if(as.character(alldays[j]) == as.character(days[count]) & count <= length(days)){
      time_budget_day <- subset(time_budget_id, date == days[count])
      if(nrow(time_budget_day) > 0){  
        counts <- table(time_budget_day$state, time_budget_day$timeday)
        if(length(unique(time_budget_day$timeday))<4){
          if("dusk" %in% unique(time_budget_day$timeday) == FALSE){
            time_budget_day <- rbind(time_budget_day, time_budget_day[nrow(time_budget_day), ])
            time_budget_day[nrow(time_budget_day), 20] <- "inact.burrow.shallow"
            time_budget_day[nrow(time_budget_day), 21] <- "dusk"
          }
          if("dawn" %in% unique(time_budget_day$timeday) == FALSE){
            time_budget_day <- rbind(time_budget_day, time_budget_day[nrow(time_budget_day), ])
            time_budget_day[nrow(time_budget_day), 20] <- "inact.burrow.shallow"
            time_budget_day[nrow(time_budget_day), 21] <- "dawn"
          }  
          counts <- table(time_budget_day$state, time_budget_day$timeday)
        }
        if(nrow(counts) > 1 ){
          order=c(3,4,5,1,2,6)
          props <- prop.table(counts, margin = 2)
          props = cbind(props,order)
          props = props[order(props[,5]),][,-5]
        if(j >= start  & j <= finish){
          if(i==1){
          barplot(props, ylim = c(0,1), cex.lab = .8, cex = .6, cex.axis =.8, tck=0.01, yaxs = "i", ylab = "", main = days[count],
            xlab="", col=c("black", "dark grey", "red", "orange", "blue", "light blue") )
          
          if(count==1){
            legend("topleft", pch = rep(15,6), inset=c(-1.5,-.3), col=rev(c("black", 
  "dark grey", "red", "orange", "blue", "light blue")), legend = rev(c("inactive deep",
    "inactive shallow", "active deep",  "active shallow" , "entrance", "surface")),
  xpd=NA, bty = 'n', cex = .85)
          }
            
          }else{
          barplot(props, ylim = c(0,1), cex.lab = .8, cex = .6, cex.axis =.8, tck=0.01, yaxs = "i", ylab = "",
            xlab="", col=c("black", "dark grey", "red", "orange", "blue", "light blue") )          
          mtext(outer = TRUE, text = 'proportion of time',side = 2, line = -1, cex = .8)
          }
          box()
        }
        }else{
        if(j >= start  & j <= finish){
        plot.new()
        }
        }
      }else{
        if(j >= start  & j <= finish){ 
        plot.new()
        }
      }
      count <- count + 1
    }else{
        if(j >= start  & j <= finish){ 
        plot.new()
        }
      
    }
  }
}

dev.off()


# Figure S6
pdf(paste("FigureS6.pdf",sep=""),paper="A4r",width=15,height=8)


# read in data
mean.max.juv.fullsuns=read.csv(paste0(dropbox,"csv summaries/mean.max.juv.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.juv.partshades=read.csv(paste0(dropbox,"csv summaries/mean.max.juv.partshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.max.juv.fullshades=read.csv(paste0(dropbox,"csv summaries/mean.max.juv.fullshades.csv"),stringsAsFactors = FALSE)[,-1]

mean.min.juv.fullsuns=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.partshades=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.partshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.fullshades=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.fullshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.fullsuns=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.partshades=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.partshades.csv"),stringsAsFactors = FALSE)[,-1]
mean.min.juv.fullshades=read.csv(paste0(dropbox,"csv summaries/mean.min.juv.fullshades.csv"),stringsAsFactors = FALSE)[,-1]

# format dates
mean.max.juv.fullsuns$date = as.POSIXct(mean.max.juv.fullsuns$date, format = "%Y-%m-%d")
mean.max.juv.partshades$date = as.POSIXct(mean.max.juv.partshades$date, format = "%Y-%m-%d")
mean.max.juv.fullshades$date = as.POSIXct(mean.max.juv.fullshades$date, format = "%Y-%m-%d")

mean.min.juv.fullsuns$date = as.POSIXct(mean.min.juv.fullsuns$date, format = "%Y-%m-%d")
mean.min.juv.partshades$date = as.POSIXct(mean.min.juv.partshades$date, format = "%Y-%m-%d")
mean.min.juv.fullshades$date = as.POSIXct(mean.min.juv.fullshades$date, format = "%Y-%m-%d")

# get monthly means 
monthly.mean.max.juv.fullsuns=aggregate(mean.max.juv.fullsuns,by=list(format(mean.max.juv.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.max.juv.partshades=aggregate(mean.max.juv.partshades,by=list(format(mean.max.juv.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.max.juv.fullshades=aggregate(mean.max.juv.fullshades,by=list(format(mean.max.juv.fullshades$date,"%m-%y")), FUN=mean)

monthly.mean.min.juv.fullsuns=aggregate(mean.min.juv.fullsuns,by=list(format(mean.min.juv.fullsuns$date,"%m-%y")), FUN=mean)
monthly.mean.min.juv.partshades=aggregate(mean.min.juv.partshades,by=list(format(mean.min.juv.partshades$date,"%m-%y")), FUN=mean)
monthly.mean.min.juv.fullshades=aggregate(mean.min.juv.fullshades,by=list(format(mean.min.juv.fullshades$date,"%m-%y")), FUN=mean)

datestart="2013-10-05"# earliest is "2013-10-01"
datefinish="2014-09-30"# latest is "2014-09-20"

# plot the juv data
with(mean.max.juv.fullsuns,plot(date,mean.max.temperature, cex.axis=1.2, type='l',col='white', xaxs = "i",ylim=c(-5,75),xlim=c(as.numeric(as.POSIXct(datestart,origin="1970-01-01")),as.numeric(as.POSIXct(datefinish,origin="1970-01-01"))),xlab="",ylab="",xaxt = "n")) # plot the data
title(ylab="operative temperature (°C)",mgp=c(2,1,0), cex.lab = 1.5, line = 2.5)

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


legend("topright", pch = rep(15,3), col=rev(c("light blue", "orange", "red")), legend = rev(c("deep shade","part shade", "no shade")),
  bty = 'n', cex = 1, pt.cex = 1,inset=c(0.10, 0))

legend("topright", pch = 15, col=rev(c("grey")), legend = rev(c("weather station air temp.")),
  bty = 'n', cex = 1.2, pt.cex = 1.2,inset=c(0.25, .06))
legend("topright", pch = "+", col=rev(c("blue")), legend = rev(c( "mean max. air temp.")),
  bty = 'n', cex = 1.2, pt.cex = 1.2,inset=c(0.28, .03))
legend("topright", pch = "|", col=rev(c("blue")), legend = rev(c( "rainfall (mm)")),
  bty = 'n', cex = 1.2, pt.cex = 1.2,inset=c(0.35, 0))

dev.off()


#Figure S7

pdf(paste("FigureS7.pdf",sep=""),paper="A4r",width=15,height=8)

source("burrow chapter final scripts/bar.R")
library(plyr)
# read in data
all.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/all.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.fullshades=read.csv(paste0(dropbox,"csv summaries/all.adult.fullshades.csv"),stringsAsFactors = FALSE)[,-1]

all.adult.fullsuns$date_time = as.POSIXct(all.adult.fullsuns$date_time, format = "%Y-%m-%d %H")
all.adult.fullshades$date_time = as.POSIXct(all.adult.fullshades$date_time, format = "%Y-%m-%d %H")

all.adult.fullsuns_hourly <- ddply(all.adult.fullsuns, .(date_time, timeday), summarize, temperature=mean(temperature))
all.adult.fullsuns_hourly <- all.adult.fullsuns_hourly[which(!duplicated(all.adult.fullsuns_hourly$date_time)),]

all.adult.fullshades_hourly <- ddply(all.adult.fullshades, .(date_time, timeday), summarize, temperature=mean(temperature))
all.adult.fullshades_hourly <- all.adult.fullshades_hourly[which(!duplicated(all.adult.fullshades_hourly$date_time)),]

colnames(all.adult.fullsuns_hourly)[3] <- "c.sun"
colnames(all.adult.fullshades_hourly)[3] <- "c.shd"

# read in data
all.burrow.surf=read.csv(paste0(dropbox,"csv summaries/all.burrow.surf.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.deep=read.csv(paste0(dropbox,"csv summaries/all.burrow.deep.csv"),stringsAsFactors = FALSE)[,-1]
colnames(all.burrow.surf)[6] <- "timeday"
colnames(all.burrow.deep)[6] <- "timeday"

all.burrow.surf$date_time = as.POSIXct(all.burrow.surf$date_time, format = "%Y-%m-%d %H")
all.burrow.deep$date_time = as.POSIXct(all.burrow.deep$date_time, format = "%Y-%m-%d %H")

all.burrow.surf_hourly <- ddply(all.burrow.surf, .(date_time, timeday), summarize, temperature=mean(temperature))
all.burrow.surf_hourly <- all.burrow.surf_hourly[which(!duplicated(all.burrow.surf_hourly$date_time)),]
all.burrow.deep_hourly <- ddply(all.burrow.deep, .(date_time, timeday), summarize, temperature=mean(temperature))
all.burrow.deep_hourly <- all.burrow.deep_hourly[which(!duplicated(all.burrow.deep_hourly$date_time)),]

colnames(all.burrow.surf_hourly)[3] <- "b.surf"
colnames(all.burrow.deep_hourly)[3] <- "b.deep"

pdf(paste("FigureS8.pdf",sep=""),paper="A4r",width=15,height=11)

warming <- c(0,2.8)


# loop through warming scenarios
for(j in 1:2){
warm <- warming[j]

potential.act <- merge(all.adult.fullsuns_hourly, all.adult.fullshades_hourly, by = "date_time")
potential.act <- merge(potential.act, all.burrow.surf_hourly, by = "date_time")
potential.act <- merge(potential.act, all.burrow.deep_hourly, by = "date_time")
potential.act <- potential.act[, -c(4,6,8)]
#potential.act$timeday.x[potential.act$timeday.x == "dusk" | potential.act$timeday.x == "dawn"] <- "duskdawn"



# max and min Tb from lines 39 and 40 in 'activity figures.R'
maxTb <- 39.61
minTb <- 25.67

potential.act$c.sun[potential.act$c.sun+warm>maxTb]<-0 # zero too hot
potential.act$c.sun[potential.act$c.sun+warm<minTb]<-0 # zero too cold
potential.act$c.sun[potential.act$c.sun!=0]<-1 # 1 for just right
potential.act$c.shd[potential.act$c.sun==1]<-0 # zero, suitable in open
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm>maxTb]<-0 # zero, too hot in shade
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm<minTb]<-0 # zero, too cold in shade
potential.act$c.shd[potential.act$c.shd!=0]<-1 # 1 for just right in shade
potential.act$b.surf[potential.act$c.sun==1 | potential.act$c.shd==1]<-0 # zero, suitable on surface
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm>maxTb]<-0 # zero, too hot shallow
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm<minTb]<-0 # zero, too cold shallow
potential.act$b.surf[potential.act$b.surf!=0]<-1 # 1 for just right shallow
potential.act$b.deep[potential.act$c.sun==1 | potential.act$c.shd==1 | potential.act$b.surf==1]<-0 # zero suitable on surface or shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm>maxTb]<-0 # zero, too hot shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm<minTb]<-0 # zero, too cold shallow
potential.act$b.deep[potential.act$b.deep!=0]<-1 # 1 for just right shallow
potential.act$state <- "inactive.deep"
potential.act$state[potential.act$c.sun==1]<-"active.open"
potential.act$state[potential.act$c.shd==1]<-"active.shade"
potential.act$state[potential.act$b.surf==1]<-"active.shallow"

mons <- c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')

par(mfrow = c(3,4))
for(i in 1:12){
  potential.act.seas <- subset(potential.act, as.numeric(format(potential.act$date_time, "%m")) == i)
counts <- table(potential.act.seas$state, potential.act.seas$timeday)

if(nrow(counts)<4){
  if(!"active.shallow" %in% row.names(counts)){
cols <- c("black", "blue", "light blue")
order=c(3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,5]),][,-5]
}else{
cat('check')
}
}else{

cols <- c("black", "orange", "blue", "light blue")
order=c(4, 3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,5]),][,-5]
}


barplot(props, ylim = c(0,1.5), cex.lab = 1.3, cex = 1.3, cex.axis =1.3, tck=0.01, yaxs = "i", ylab = "proportion of time",
  xlab="", col=cols, main = mons[i] )
box()
title(main=paste0(warming[j], " degrees C warming"),outer=T, line = -2)
if(i == 1){
legend("topright", pch = rep(15,2), col=rev(c("black", "orange")), legend = rev(c("inactive deep","active shallow")),
  cex = 1, pt.cex = 1, bty = "n", inset=c(0.01, .004))
legend("topleft", pch = rep(15,2), col=rev(c("blue", "light blue")), legend = rev(c("active shade",  "active open")),
  cex = 1, pt.cex = 1, bty = "n", inset=c(0.01, .004))
}
text(x = .7, y = 1.1, "1 hour", cex = 1)
text(x = 1.9, y = 1.1, "11.5 hours", cex = 1)
text(x = 3.1, y = 1.1, "1 hour", cex = 1)
text(x = 4.3, y = 1.1, "10.5 hours", cex = 1)
}
}
dev.off()

