################################### copper models #####################################################################

adult.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/adult models/"
adult.folders<-list.dirs(adult.folder)[-1]
par(mfrow = c(3,2)) # set up for 6 plots in 2 columns

sites<-c("C1","T1","C2","T2","C3","T3")
for(k in 1:length(sites)){
################################### adult copper model full sun #####################################################################

adult.folders.fullsun<-adult.folders[grep(adult.folders,pattern = "full sun")]
if(length(adult.folders.fullsun)>1){
  adult.folders.fullsun<-adult.folders.fullsun[1]
}
adult.files.fullsun<-list.files(adult.folders.fullsun)
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = ".txt")]
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = sites[k])]

# read and plot all data

for(i in 1:length(adult.files.fullsun)){
  adult.fullsun<-read.csv(paste(adult.folders.fullsun,"/",adult.files.fullsun[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(adult.fullsun)<-c('date_time','temperature') #give the columns names
  adult.title<-adult.files.fullsun[i]
  adult.title<-str_replace_all(adult.title,'.txt','') #get rid of ".txt"
  adult.title<-str_replace_all(adult.title,'fullsun','') #get rid of "fullsun"

  adult.fullsun$temperature<-type.convert(sub("\\p{So}C", "", adult.fullsun$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  adult.fullsun$date_time<-as.POSIXct(adult.fullsun$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   adult.fullsuns<-adult.fullsun
  }else{
   adult.fullsuns<-rbind(adult.fullsun,adult.fullsun)
  }
 }
adult.fullsun<-adult.fullsun[order(adult.fullsun$date_time),] 

  with(adult.fullsun,plot(temperature~date_time,type='l',col='red',ylim=c(-10,70),xlim=c(as.numeric(as.POSIXct("2013-11-01",origin="1970-01-01")),as.numeric(as.POSIXct("2014-02-01",origin="1970-01-01"))),main=adult.title,xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
axis.POSIXct(side = 1, x = soil.surfaces$date_time,
             at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "months"), format = "%b %y",
             las = 2)

################################### adult copper model part shade #####################################################################

adult.folders.partshade<-adult.folders[grep(adult.folders,pattern = "part shade")]
if(length(adult.folders.partshade)>1){
  adult.folders.partshade<-adult.folders.partshade[1]
}
adult.files.partshade<-list.files(adult.folders.partshade)
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = ".txt")]
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = sites[k])]

# read and plot all data

for(i in 1:length(adult.files.partshade)){
  adult.partshade<-read.csv(paste(adult.folders.partshade,"/",adult.files.partshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(adult.partshade)<-c('date_time','temperature') #give the columns names
  adult.title<-adult.files.partshade[i]
  adult.title<-str_replace_all(adult.title,'.txt','') #get rid of ".txt"

  adult.partshade$temperature<-type.convert(sub("\\p{So}C", "", adult.partshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  adult.partshade$date_time<-as.POSIXct(adult.partshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   adult.partshades<-adult.partshade
  }else{
   adult.partshades<-rbind(adult.partshade,adult.partshade)
  }
 }
adult.partshade<-adult.partshade[order(adult.partshade$date_time),] 

  with(adult.partshade,points(temperature~date_time,type='l',col='orange')) # plot the data

################################### adult copper model full shade #####################################################################

adult.folders.fullshade<-adult.folders[grep(adult.folders,pattern = "full shade")]
adult.files.fullshade<-list.files(adult.folders.fullshade)
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = ".txt")]
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = sites[k])]

# read and plot all data

for(i in 1:length(adult.files.fullshade)){
  adult.fullshade<-read.csv(paste(adult.folders.fullshade,"/",adult.files.fullshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(adult.fullshade)<-c('date_time','temperature') #give the columns names
  adult.title<-adult.files.fullshade[i]
  adult.title<-str_replace_all(adult.title,'.txt','') #get rid of ".txt"

  adult.fullshade$temperature<-type.convert(sub("\\p{So}C", "", adult.fullshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  adult.fullshade$date_time<-as.POSIXct(adult.fullshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   adult.fullshades<-adult.fullshade
  }else{
   adult.fullshades<-rbind(adult.fullshade,adult.fullshade)
  }
 }
adult.fullshade<-adult.fullshade[order(adult.fullshade$date_time),] 

  
  with(adult.fullshade,points(temperature~date_time,type='l',col='blue')) # plot the data


grid(NA, NULL) # grid only in y-direction

}
################################### juvenile copper model full shade #####################################################################

juvenile.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Juvenile models/"
juvenile.folders<-list.dirs(juvenile.folder)[-1]
juvenile.folders.fullshade<-juvenile.folders[grep(juvenile.folders,pattern = "full shade")]
juvenile.files.fullshade<-list.files(juvenile.folders.fullshade)
juvenile.files.fullshade<-juvenile.files.fullshade[grep(juvenile.files.fullshade,pattern = ".txt")]
juvenile.files.fullshade<-juvenile.files.fullshade[grep(juvenile.files.fullshade,pattern = sites[k])]

# read and plot all data

for(i in 1:length(juvenile.files.fullshade)){
  juv.fullshade<-read.csv(paste(juvenile.folders.fullshade,"/",juvenile.files.fullshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(juv.fullshade)<-c('date_time','temperature') #give the columns names
  juv.title<-juvenile.files.fullshade[i]
  juv.title<-str_replace_all(juv.title,'.txt','') #get rid of ".txt"

  juv.fullshade$temperature<-type.convert(sub("\\p{So}C", "", juv.fullshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  juv.fullshade$date_time<-as.POSIXct(juv.fullshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   juv.fullshades<-juv.fullshade
  }else{
   juv.fullshades<-rbind(juv.fullshade,juv.fullshade)
  }
 }
juv.fullshade<-juv.fullshade[order(juv.fullshade$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,juv.title),ylim=c(10,70))) # plot the data
  with(juv.fullshade,points(temperature~date_time,type='l',col='red')) # plot the data

################################### juvenile copper model full sun #####################################################################

juvenile.folders.fullsun<-juvenile.folders[grep(juvenile.folders,pattern = "full sun")]
if(length(juvenile.folders.fullsun)>1){
  juvenile.folders.fullsun<-juvenile.folders.fullsun[1]
}
juvenile.files.fullsun<-list.files(juvenile.folders.fullsun)
juvenile.files.fullsun<-juvenile.files.fullsun[grep(juvenile.files.fullsun,pattern = ".txt")]
juvenile.files.fullsun<-juvenile.files.fullsun[grep(juvenile.files.fullsun,pattern = sites[k])]

# read and plot all data

for(i in 1:length(juvenile.files.fullsun)){
  juv.fullsun<-read.csv(paste(juvenile.folders.fullsun,"/",juvenile.files.fullsun[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(juv.fullsun)<-c('date_time','temperature') #give the columns names
  juv.title<-juvenile.files.fullsun[i]
  juv.title<-str_replace_all(juv.title,'.txt','') #get rid of ".txt"

  juv.fullsun$temperature<-type.convert(sub("\\p{So}C", "", juv.fullsun$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  juv.fullsun$date_time<-as.POSIXct(juv.fullsun$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   juv.fullsuns<-juv.fullsun
  }else{
   juv.fullsuns<-rbind(juv.fullsun,juv.fullsun)
  }
 }
juv.fullsun<-juv.fullsun[order(juv.fullsun$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,juv.title),ylim=c(10,70))) # plot the data
  with(juv.fullsun,points(temperature~date_time,type='l',col='red')) # plot the data

################################### juvenile copper model part shade #####################################################################

juvenile.folders.partshade<-juvenile.folders[grep(juvenile.folders,pattern = "part shade")]
if(length(juvenile.folders.partshade)>1){
  juvenile.folders.partshade<-juvenile.folders.partshade[1]
}
juvenile.files.partshade<-list.files(juvenile.folders.partshade)
juvenile.files.partshade<-juvenile.files.partshade[grep(juvenile.files.partshade,pattern = ".txt")]
juvenile.files.partshade<-juvenile.files.partshade[grep(juvenile.files.partshade,pattern = sites[k])]

# read and plot all data

for(i in 1:length(juvenile.files.partshade)){
  juv.partshade<-read.csv(paste(juvenile.folders.partshade,"/",juvenile.files.partshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(juv.partshade)<-c('date_time','temperature') #give the columns names
  juv.title<-juvenile.files.partshade[i]
  juv.title<-str_replace_all(juv.title,'.txt','') #get rid of ".txt"

  juv.partshade$temperature<-type.convert(sub("\\p{So}C", "", juv.partshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  juv.partshade$date_time<-as.POSIXct(juv.partshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   juv.partshades<-juv.partshade
  }else{
   juv.partshades<-rbind(juv.partshade,juv.partshade)
  }
 }
juv.partshade<-juv.partshade[order(juv.partshade$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,juv.title),ylim=c(10,70))) # plot the data
  with(juv.partshade,points(temperature~date_time,type='l',col='red')) # plot the data
