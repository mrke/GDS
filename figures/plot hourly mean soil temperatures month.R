################################### soil temperatures #####################################################################
library(stringr)

soil.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Soil Profiles/"
soil.files<-list.files(soil.folder)
soil.files<-soil.files[grep(soil.files,pattern = ".txt")]
soil.files<-soil.files[-grep(soil.files,pattern = "backup")] # remove backups

sites<-c("C1","T1","C2","T2","C3","T3")
dates<-c("01-Jan-15","09-Dec-14","10-Dec-14","10-Nov-13","15-May-14","21-Apr-14","22-Apr-14","23-Apr-14")
depths<-c("_surface","_5cm","_15cm","_30cm","_50cm","_1m")

# note 11-13th Nov 2013 and 20th-24th April 2014 are data logger download events, remove these from the plots

# read in and plot results per depth for a given site and date
soil.files.subset<-soil.files # specify site

################################### soil temp surface #####################################################################
setwd('figures/')
soil.files.surface<-soil.files.subset[grep(soil.files.subset,pattern = depths[1])]

for(i in 1:length(soil.files.surface)){
  soil.surface<-read.csv(paste(soil.folder,soil.files.surface[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.surface)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.surface[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.surface$temperature<-type.convert(sub("\\p{So}C", "", soil.surface$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.surface$date_time<-as.POSIXct(soil.surface$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
# get rid of values in November April when loggers were downloaded
soil.surface<-subset(soil.surface,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
soil.surface<-subset(soil.surface,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
if(length(grep(soil.files.surface[i],pattern = "C3"))>0){
  soil.surface<-subset(soil.surface,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
}
  if(i==1){
  soil.surfaces<-soil.surface
  }else{
  soil.surfaces<-rbind(soil.surfaces,soil.surface)
  }
}
soil.surfaces<-soil.surfaces[order(soil.surfaces$date_time),] 

hourly.mean<-aggregate(soil.surfaces$temperature,by=list(format(soil.surfaces$date_time,"%m %H")),FUN=mean)
hourly.max<-aggregate(soil.surfaces$temperature,by=list(format(soil.surfaces$date_time,"%m %H")),FUN=max)
hourly.min<-aggregate(soil.surfaces$temperature,by=list(format(soil.surfaces$date_time,"%m %H")),FUN=min)
mon_hr<-cbind(as.numeric(substr(hourly.mean$Group.1,1,2)),as.numeric(substr(hourly.mean$Group.1,4,5)))
hourly.surface<-as.data.frame(cbind(mon_hr,hourly.mean[,2],hourly.max[,2],hourly.min[,2]))
colnames(hourly.surface)<-c("month","hour","mean","max","min")

pdf("surface_hourly_soil.pdf",paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,4,3,1) + 0.1) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 

months<-c("January","February","March","April","May","June","July","August","September","October","November","December")

for(i in 1:12){
  sub<-subset(hourly.surface,month==i)
  with(sub,plot(mean~hour,col="orange",ylab="temperature (deg C)",xlab="",ylim=c(-10,70),type='p',main=paste("surface ",months[i],sep="")))
  with(sub,points(max~hour,col="red",type='p'))
  with(sub,points(min~hour,col="blue",type='p'))
  abline(34,0,col='orange',lty=2)
  abline(30,0,col='orange',lty=2)
  abline(42,0,col='red',lty=2)
  abline(20,0,col='blue',lty=2)  
}
dev.off()

################################### soil temp 5cm #####################################################################


soil.files.5cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[2])]

for(i in 1:length(soil.files.5cm)){
  soil.5cm<-read.csv(paste(soil.folder,soil.files.5cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.5cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.5cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.5cm$temperature<-type.convert(sub("\\p{So}C", "", soil.5cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.5cm$date_time<-as.POSIXct(soil.5cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
# get rid of values in November April when loggers were downloaded
soil.5cm<-subset(soil.5cm,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
soil.5cm<-subset(soil.5cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
if(length(grep(soil.files.5cm[i],pattern = "C3"))>0){
  soil.5cm<-subset(soil.5cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
}
  if(i==1){
  soil.5cms<-soil.5cm
  }else{
  soil.5cms<-rbind(soil.5cms,soil.5cm)
  }
}
soil.5cms<-soil.5cms[order(soil.5cms$date_time),] 

hourly.mean<-aggregate(soil.5cms$temperature,by=list(format(soil.5cms$date_time,"%m %H")),FUN=mean)
hourly.max<-aggregate(soil.5cms$temperature,by=list(format(soil.5cms$date_time,"%m %H")),FUN=max)
hourly.min<-aggregate(soil.5cms$temperature,by=list(format(soil.5cms$date_time,"%m %H")),FUN=min)
mon_hr<-cbind(as.numeric(substr(hourly.mean$Group.1,1,2)),as.numeric(substr(hourly.mean$Group.1,4,5)))
hourly.5cm<-as.data.frame(cbind(mon_hr,hourly.mean[,2],hourly.max[,2],hourly.min[,2]))
colnames(hourly.5cm)<-c("month","hour","mean","max","min")

pdf("5cm_hourly_soil.pdf",paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,4,3,1) + 0.1) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:12){
  sub<-subset(hourly.5cm,month==i)
  with(sub,plot(mean~hour,col="orange",ylab="temperature (deg C)",xlab="",ylim=c(-10,70),type='p',main=paste("5cm ",months[i],sep="")))
  with(sub,points(max~hour,col="red",type='p'))
  with(sub,points(min~hour,col="blue",type='p'))
  abline(34,0,col='orange',lty=2)
  abline(30,0,col='orange',lty=2)
  abline(42,0,col='red',lty=2)
  abline(20,0,col='blue',lty=2)
}
dev.off()
################################### soil temp 15cm #####################################################################


soil.files.15cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[3])]

for(i in 1:length(soil.files.15cm)){
  soil.15cm<-read.csv(paste(soil.folder,soil.files.15cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.15cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.15cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.15cm$temperature<-type.convert(sub("\\p{So}C", "", soil.15cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.15cm$date_time<-as.POSIXct(soil.15cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
# get rid of values in November April when loggers were downloaded
soil.15cm<-subset(soil.15cm,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
soil.15cm<-subset(soil.15cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
if(length(grep(soil.files.15cm[i],pattern = "C3"))>0){
  soil.15cm<-subset(soil.15cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
}
  if(i==1){
  soil.15cms<-soil.15cm
  }else{
  soil.15cms<-rbind(soil.15cms,soil.15cm)
  }
}
soil.15cms<-soil.15cms[order(soil.15cms$date_time),] 

hourly.mean<-aggregate(soil.15cms$temperature,by=list(format(soil.15cms$date_time,"%m %H")),FUN=mean)
hourly.max<-aggregate(soil.15cms$temperature,by=list(format(soil.15cms$date_time,"%m %H")),FUN=max)
hourly.min<-aggregate(soil.15cms$temperature,by=list(format(soil.15cms$date_time,"%m %H")),FUN=min)
mon_hr<-cbind(as.numeric(substr(hourly.mean$Group.1,1,2)),as.numeric(substr(hourly.mean$Group.1,4,5)))
hourly.15cm<-as.data.frame(cbind(mon_hr,hourly.mean[,2],hourly.max[,2],hourly.min[,2]))
colnames(hourly.15cm)<-c("month","hour","mean","max","min")

pdf("15cm_hourly_soil.pdf",paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,4,3,1) + 0.1) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:12){
  sub<-subset(hourly.15cm,month==i)
  with(sub,plot(mean~hour,col="orange",ylab="temperature (deg C)",xlab="",ylim=c(-10,70),type='p',main=paste("15cm ",months[i],sep="")))
  with(sub,points(max~hour,col="red",type='p'))
  with(sub,points(min~hour,col="blue",type='p'))
  abline(34,0,col='orange',lty=2)
  abline(30,0,col='orange',lty=2)
  abline(42,0,col='red',lty=2)
  abline(20,0,col='blue',lty=2)
}
dev.off()

################################### soil temp 30cm #####################################################################


soil.files.30cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[4])]

for(i in 1:length(soil.files.30cm)){
  soil.30cm<-read.csv(paste(soil.folder,soil.files.30cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.30cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.30cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.30cm$temperature<-type.convert(sub("\\p{So}C", "", soil.30cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.30cm$date_time<-as.POSIXct(soil.30cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
# get rid of values in November April when loggers were downloaded
soil.30cm<-subset(soil.30cm,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
soil.30cm<-subset(soil.30cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
if(length(grep(soil.files.30cm[i],pattern = "C3"))>0){
  soil.30cm<-subset(soil.30cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
}
  if(i==1){
  soil.30cms<-soil.30cm
  }else{
  soil.30cms<-rbind(soil.30cms,soil.30cm)
  }
}
soil.30cms<-soil.30cms[order(soil.30cms$date_time),] 

hourly.mean<-aggregate(soil.30cms$temperature,by=list(format(soil.30cms$date_time,"%m %H")),FUN=mean)
hourly.max<-aggregate(soil.30cms$temperature,by=list(format(soil.30cms$date_time,"%m %H")),FUN=max)
hourly.min<-aggregate(soil.30cms$temperature,by=list(format(soil.30cms$date_time,"%m %H")),FUN=min)
mon_hr<-cbind(as.numeric(substr(hourly.mean$Group.1,1,2)),as.numeric(substr(hourly.mean$Group.1,4,5)))
hourly.30cm<-as.data.frame(cbind(mon_hr,hourly.mean[,2],hourly.max[,2],hourly.min[,2]))
colnames(hourly.30cm)<-c("month","hour","mean","max","min")

pdf("30cm_hourly_soil.pdf",paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,4,3,1) + 0.1) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:12){
  sub<-subset(hourly.30cm,month==i)
  with(sub,plot(mean~hour,col="orange",ylab="temperature (deg C)",xlab="",ylim=c(-10,70),type='p',main=paste("30cm ",months[i],sep="")))
  with(sub,points(max~hour,col="red",type='p'))
  with(sub,points(min~hour,col="blue",type='p'))
  abline(34,0,col='orange',lty=2)
  abline(30,0,col='orange',lty=2)
  abline(42,0,col='red',lty=2)
  abline(20,0,col='blue',lty=2)
}
dev.off()

################################### soil temp 50cm #####################################################################


soil.files.50cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[5])]

for(i in 1:length(soil.files.50cm)){
  soil.50cm<-read.csv(paste(soil.folder,soil.files.50cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.50cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.50cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.50cm$temperature<-type.convert(sub("\\p{So}C", "", soil.50cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.50cm$date_time<-as.POSIXct(soil.50cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
# get rid of values in November April when loggers were downloaded
soil.50cm<-subset(soil.50cm,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
soil.50cm<-subset(soil.50cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
if(length(grep(soil.files.50cm[i],pattern = "C3"))>0){
  soil.50cm<-subset(soil.50cm,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
}
  if(i==1){
  soil.50cms<-soil.50cm
  }else{
  soil.50cms<-rbind(soil.50cms,soil.50cm)
  }
}
soil.50cms<-soil.50cms[order(soil.50cms$date_time),] 

hourly.mean<-aggregate(soil.50cms$temperature,by=list(format(soil.50cms$date_time,"%m %H")),FUN=mean)
hourly.max<-aggregate(soil.50cms$temperature,by=list(format(soil.50cms$date_time,"%m %H")),FUN=max)
hourly.min<-aggregate(soil.50cms$temperature,by=list(format(soil.50cms$date_time,"%m %H")),FUN=min)
mon_hr<-cbind(as.numeric(substr(hourly.mean$Group.1,1,2)),as.numeric(substr(hourly.mean$Group.1,4,5)))
hourly.50cm<-as.data.frame(cbind(mon_hr,hourly.mean[,2],hourly.max[,2],hourly.min[,2]))
colnames(hourly.50cm)<-c("month","hour","mean","max","min")

pdf("50cm_hourly_soil.pdf",paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,4,3,1) + 0.1) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:12){
  sub<-subset(hourly.50cm,month==i)
  with(sub,plot(mean~hour,col="orange",ylab="temperature (deg C)",xlab="",ylim=c(-10,70),type='p',main=paste("50cm ",months[i],sep="")))
  with(sub,points(max~hour,col="red",type='p'))
  with(sub,points(min~hour,col="blue",type='p'))
  abline(34,0,col='orange',lty=2)
  abline(30,0,col='orange',lty=2)
  abline(42,0,col='red',lty=2)
  abline(20,0,col='blue',lty=2)
}
dev.off()

################################### soil temp 1m #####################################################################


soil.files.1m<-soil.files.subset[grep(soil.files.subset,pattern = depths[6])]

for(i in 1:length(soil.files.1m)){
  soil.1m<-read.csv(paste(soil.folder,soil.files.1m[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.1m)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.1m[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.1m$temperature<-type.convert(sub("\\p{So}C", "", soil.1m$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.1m$date_time<-as.POSIXct(soil.1m$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
# get rid of values in November April when loggers were downloaded
soil.1m<-subset(soil.1m,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
soil.1m<-subset(soil.1m,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
if(length(grep(soil.files.1m[i],pattern = "C3"))>0){
  soil.1m<-subset(soil.1m,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
}
  if(i==1){
  soil.1ms<-soil.1m
  }else{
  soil.1ms<-rbind(soil.1ms,soil.1m)
  }
}
soil.1ms<-soil.1ms[order(soil.1ms$date_time),] 

hourly.mean<-aggregate(soil.1ms$temperature,by=list(format(soil.1ms$date_time,"%m %H")),FUN=mean)
hourly.max<-aggregate(soil.1ms$temperature,by=list(format(soil.1ms$date_time,"%m %H")),FUN=max)
hourly.min<-aggregate(soil.1ms$temperature,by=list(format(soil.1ms$date_time,"%m %H")),FUN=min)
mon_hr<-cbind(as.numeric(substr(hourly.mean$Group.1,1,2)),as.numeric(substr(hourly.mean$Group.1,4,5)))
hourly.1m<-as.data.frame(cbind(mon_hr,hourly.mean[,2],hourly.max[,2],hourly.min[,2]))
colnames(hourly.1m)<-c("month","hour","mean","max","min")

pdf("1m_hourly_soil.pdf",paper="A4",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow = c(3,2)) # set up for 6 plots in 2 columns
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,4,3,1) + 0.1) # margin spacing stuff 
par(mgp = c(3,1,0) ) # margin spacing stuff 

for(i in 1:12){
  sub<-subset(hourly.1m,month==i)
  with(sub,plot(mean~hour,col="orange",ylab="temperature (deg C)",xlab="",ylim=c(-10,70),type='p',main=paste("100cm ",months[i],sep="")))
  with(sub,points(max~hour,col="red",type='p'))
  with(sub,points(min~hour,col="blue",type='p'))
  abline(34,0,col='orange',lty=2)
  abline(30,0,col='orange',lty=2)
  abline(42,0,col='red',lty=2)
  abline(20,0,col='blue',lty=2)
}
dev.off()
