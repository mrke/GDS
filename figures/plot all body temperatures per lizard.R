################################### GDS temperatures #####################################################################
library(stringr)

GDS.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/GDS body temps/"

GDS.files<-list.files(GDS.folder)
GDS.files<-GDS.files[grep(GDS.files,pattern = ".txt")]
GDS.files<-GDS.files[-grep(GDS.files,pattern = "backup")] # remove backups
par(mfrow = c(3,2)) # set up for 5 plots in 2 columns

# read and plot all data for each skink

for(i in 1:length(GDS.files)){
  GDS.data<-read.csv(paste(GDS.folder,GDS.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(GDS.data)<-c('date_time','temperature') #give the columns names
  GDS.title<-GDS.files[i]
  GDS.title<-str_replace_all(GDS.title,'.txt','') #get rid of ".txt"

  GDS.data$temperature<-type.convert(sub("\\p{So}C", "", GDS.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  GDS.data$date_time<-as.POSIXct(GDS.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
with(GDS.data,plot(temperature~date_time,type='l',col='red',ylim=c(-10,70),xlim=c(as.numeric(as.POSIXct("2013-10-01",origin="1970-01-01")),as.numeric(as.POSIXct("2014-01-01",origin="1970-01-01"))),main=GDS.title,xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
axis.POSIXct(side = 1, x = soil.surfaces$date_time,
             at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
             las = 2)

grid(NA, NULL) # grid only in y-direction
}


################################### GDS temperatures #####################################################################

GDS.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/GDS body temps/"

GDS.files<-list.files(GDS.folder)
GDS.files<-GDS.files[grep(GDS.files,pattern = ".txt")]
GDS.files<-GDS.files[-grep(GDS.files,pattern = "backup")] # remove backups
par(mfrow = c(1,1)) # set up for 5 plots in 2 columns

# read and plot all data for each skink

 i<-1
  GDS.data<-read.csv(paste(GDS.folder,GDS.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(GDS.data)<-c('date_time','temperature') #give the columns names
  GDS.title<-GDS.files[i]
  GDS.title<-str_replace_all(GDS.title,'.txt','') #get rid of ".txt"

  GDS.data$temperature<-type.convert(sub("\\p{So}C", "", GDS.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  GDS.data$date_time<-as.POSIXct(GDS.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum

  # get rid of values in November April when loggers were downloaded
GDS.data<-subset(GDS.data,as.numeric(date_time)>as.numeric(as.POSIXct("2013-10-24",origin="1970-01-01")) & as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-11",origin="1970-01-01")))

  with(GDS.data,plot(temperature~date_time,type='l',col='red',ylim=c(25,40),main=GDS.title,xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
axis.POSIXct(side = 1, x = GDS.data$date_time,
             at = seq(ISOdate(2013,10,23), ISOdate(2013,11,13), "days"), format = "%d %b %y",
             las = 2)

grid(seq(ISOdate(2013,10,23), ISOdate(2013,11,13), "days"), NA) # grid only in x-direction
