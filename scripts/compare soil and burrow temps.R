# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"

burrow.folder<-paste0(dropbox,"burrow data")
burrow.folders<-list.dirs(burrow.folder)[-1]
burrows<-strsplit(burrow.folders, "/")
burrows<-as.character(sapply(burrows, "[[", 8)) # get burrow names)

# read in data
all.burrow.surf=read.csv(paste0(dropbox,"csv summaries/all.burrow.surf.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.mid=read.csv(paste0(dropbox,"csv summaries/all.burrow.mid.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.deep=read.csv(paste0(dropbox,"csv summaries/all.burrow.deep.csv"),stringsAsFactors = FALSE)[,-1]

all.burrow.surf$date_time = as.POSIXct(all.burrow.surf$date_time, format = "%Y-%m-%d %H:%M:%S")
all.burrow.mid$date_time = as.POSIXct(all.burrow.mid$date_time, format = "%Y-%m-%d %H:%M:%S")
all.burrow.deep$date_time = as.POSIXct(all.burrow.deep$date_time, format = "%Y-%m-%d %H:%M:%S")

all.soil.surfaces=read.csv(paste0(dropbox,"csv summaries/all.soil.surfaces.csv"),stringsAsFactors = FALSE)[,-1]
all.soil.5cms=read.csv(paste0(dropbox,"csv summaries/all.soil.5cms.csv"),stringsAsFactors = FALSE)[,-1]
all.soil.15cms=read.csv(paste0(dropbox,"csv summaries/all.soil.15cms.csv"),stringsAsFactors = FALSE)[,-1]
all.soil.30cms=read.csv(paste0(dropbox,"csv summaries/all.soil.30cms.csv"),stringsAsFactors = FALSE)[,-1]
all.soil.50cms=read.csv(paste0(dropbox,"csv summaries/all.soil.50cms.csv"),stringsAsFactors = FALSE)[,-1]
all.soil.1ms=read.csv(paste0(dropbox,"csv summaries/all.soil.1ms.csv"),stringsAsFactors = FALSE)[,-1]

all.soil.surfaces$date_time = as.POSIXct(all.soil.surfaces$date_time, format = "%Y-%m-%d %H:%M:%S")
all.soil.5cms$date_time = as.POSIXct(all.soil.5cms$date_time, format = "%Y-%m-%d %H:%M:%S")
all.soil.15cms$date_time = as.POSIXct(all.soil.15cms$date_time, format = "%Y-%m-%d %H:%M:%S")
all.soil.30cms$date_time = as.POSIXct(all.soil.30cms$date_time, format = "%Y-%m-%d %H:%M:%S")
all.soil.50cms$date_time = as.POSIXct(all.soil.50cms$date_time, format = "%Y-%m-%d %H:%M:%S")
all.soil.1ms$date_time = as.POSIXct(all.soil.1ms$date_time, format = "%Y-%m-%d %H:%M:%S")

######################## compare burrow to soil temps ###################################### 

daystart="2013-11-01" # choose start day
dayfinish="2013-11-30" # choose end day

# this saves plots as pdf files

for(bur in 1:length(burrows)){ # loop through each burrow
  
  # plot surface temps against each depth for soil temperature
  data=subset(all.burrow.surf,site==burrows[bur] & as.numeric(date_time)>as.numeric(as.POSIXct(daystart,origin="1970-01-01")) & as.numeric(date_time)<as.numeric(as.POSIXct(dayfinish,origin="1970-01-01")))
  pdf(paste0(dropbox,"burrow chapter final scripts/burrow vs soil/",burrows[bur], "surface.pdf"),paper="A4",width=11,height=15)
  par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," surface vs. soil surface",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.surfaces,points(temperature~date_time,type='l',cex=0.1,col='dark grey',ylim=c(-10,80),xlim=c(as.numeric(as.POSIXct("2013-09-01",origin="1970-01-01")),as.numeric(as.POSIXct("2014-10-01",origin="1970-01-01"))),xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," surface vs. soil 5 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.5cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," surface vs. soil 15 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.15cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," surface vs. soil 30 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.30cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," surface vs. soil 50 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.50cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," surface vs. soil 1 ms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.1ms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)
  dev.off()
  
  # plot mid burrow temps against each depth for soil temperature
  data=subset(all.burrow.mid,site==burrows[bur] & as.numeric(date_time)>as.numeric(as.POSIXct(daystart,origin="1970-01-01")) & as.numeric(date_time)<as.numeric(as.POSIXct(dayfinish,origin="1970-01-01")))
  
  pdf(paste0(dropbox,"burrow chapter final scripts/burrow vs soil/",burrows[bur], "mid.pdf"),paper="A4",width=11,height=15)
  par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," mid vs. soil surface",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.surfaces,points(temperature~date_time,type='l',cex=0.1,col='dark grey',ylim=c(-10,80),xlim=c(as.numeric(as.POSIXct("2013-09-01",origin="1970-01-01")),as.numeric(as.POSIXct("2014-10-01",origin="1970-01-01"))),xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," mid vs. soil 5 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.5cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," mid vs. soil 15 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.15cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," mid vs. soil 30 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.30cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," mid vs. soil 50 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.50cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," mid vs. soil 1 ms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.1ms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)
  dev.off()
  
  # plot deep burrow temps against each depth for soil temperature
  data=subset(all.burrow.deep,site==burrows[bur] & as.numeric(date_time)>as.numeric(as.POSIXct(daystart,origin="1970-01-01")) & as.numeric(date_time)<as.numeric(as.POSIXct(dayfinish,origin="1970-01-01")))
  
  pdf(paste0(dropbox,"burrow chapter final scripts/burrow vs soil/",burrows[bur], "deep.pdf"),paper="A4",width=11,height=15)
  par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," deep vs. soil surface",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.surfaces,points(temperature~date_time,type='l',cex=0.1,col='dark grey',ylim=c(-10,80),xlim=c(as.numeric(as.POSIXct("2013-09-01",origin="1970-01-01")),as.numeric(as.POSIXct("2014-10-01",origin="1970-01-01"))),xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," deep vs. soil 5 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.5cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," deep vs. soil 15 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.15cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," deep vs. soil 30 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.30cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," deep vs. soil 50 cms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.50cms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)  
  with(data,plot(temperature~date_time,main=paste(burrows[bur]," deep vs. soil 1 ms",sep=""),type='l',col='red',xlab="",ylab="temperature (deg C)",xaxt = "n"))
  with(all.soil.1ms,points(temperature~date_time,type='l',cex=0.1,col='dark grey')) # plot the data
  axis.POSIXct(side = 1, x = soil.surfaces$date_time,
    at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "days"), format = "%d %b %y",
    las = 2)
  dev.off()
}
