# script to plot GDS Tbs against available temperatures

# load the packages needed for this session
library(stringr)

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

################################### GDS temperatures #####################################################################

GDS.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/GDS body temps/"

GDS.files<-list.files(GDS.folder)
GDS.files<-GDS.files[grep(GDS.files,pattern = ".txt")]
GDS.files<-GDS.files[-grep(GDS.files,pattern = "backup")] # remove backups

# read and plot all data for first skink
i<-14
  GDS.data<-read.csv(paste(GDS.folder,GDS.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(GDS.data)<-c('date_time','temperature') #give the columns names
  GDS.title<-GDS.files[i]
  GDS.title<-str_replace_all(GDS.title,'.txt','') #get rid of ".txt"

  GDS.data$temperature<-type.convert(sub("\\p{So}C", "", GDS.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  GDS.data$date_time<-as.POSIXct(GDS.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  with(GDS.data,plot(temperature~date_time,type='l',main=GDS.title,ylim=c(10,70))) # plot the data


################################### burrow temperatures #####################################################################

burrow.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/burrow data/"
burrow.folders<-list.dirs(burrow.folder)[-1]

m<-5 # choose burrow folder
burrow.files<-list.files(burrow.folders[m])
burrow.files<-burrow.files[grep(burrow.files,pattern = ".txt")]
burrow.files<-burrow.files[-grep(burrow.files,pattern = "backup")] # remove backups

################################### burrow surface #####################################################################

burrow.files.surf<-burrow.files[grep(burrow.files,pattern = "surf")] # get surface files

# read in all burrow surface data
for(j in 1:length(burrow.files.surf)){
  burrow.data.surf<-read.csv(paste(burrow.folders[m],"/",burrow.files.surf[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data.surf)<-c('date_time','temperature',"humidity") #give the columns names
  burrow.title<-burrow.files.surf[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.surf$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.surf$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.surf$date_time<-as.POSIXct(burrow.data.surf$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1){
  burrow.surf<-burrow.data.surf
  }else{
  burrow.surf<-rbind(burrow.surf,burrow.data.surf)
  }
}
burrow.surf<-burrow.surf[order(burrow.surf$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,burrow.title),ylim=c(10,70))) # plot the data
  with(burrow.surf,points(temperature~date_time,type='l',col='red')) # plot the data

################################### burrow mid #####################################################################

burrow.files.mid<-burrow.files[grep(burrow.files,pattern = "mid")] # get mid files

# read in all burrow mid data
for(j in 1:length(burrow.files.mid)){
  burrow.data.mid<-read.csv(paste(burrow.folders[m],"/",burrow.files.mid[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data.mid)<-c('date_time','temperature',"humidity") #give the columns names
  burrow.title<-burrow.files.mid[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.mid$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.mid$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.mid$date_time<-as.POSIXct(burrow.data.mid$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1){
  burrow.mid<-burrow.data.mid
  }else{
  burrow.mid<-rbind(burrow.mid,burrow.data.mid)
  }
}
burrow.mid<-burrow.mid[order(burrow.mid$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,burrow.title),ylim=c(10,70))) # plot the data
  with(burrow.mid,points(temperature~date_time,type='l',col='red')) # plot the data

################################### burrow deep #####################################################################

burrow.files.deep<-burrow.files[grep(burrow.files,pattern = "deep")] # get deep files

# read in all burrow deep data
for(j in 1:length(burrow.files.deep)){
  burrow.data.deep<-read.csv(paste(burrow.folders[m],"/",burrow.files.deep[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data.deep)<-c('date_time','temperature',"hudeepity") #give the columns names
  burrow.title<-burrow.files.deep[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.deep$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.deep$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.deep$date_time<-as.POSIXct(burrow.data.deep$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1){
  burrow.deep<-burrow.data.deep
  }else{
  burrow.deep<-rbind(burrow.deep,burrow.data.deep)
  }
}
burrow.deep<-burrow.deep[order(burrow.deep$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,burrow.title),ylim=c(10,70))) # plot the data
  with(burrow.deep,points(temperature~date_time,type='l',col='red')) # plot the data

################################### soil temperatures #####################################################################

soil.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Soil Profiles/"
soil.files<-list.files(soil.folder)
soil.files<-soil.files[grep(soil.files,pattern = ".txt")]
soil.files<-soil.files[-grep(soil.files,pattern = "backup")] # remove backups

sites<-c("C1","C2","C3","C3_northernest one","T1","T2","T2a","T3")
dates<-c("01-Jan-15","09-Dec-14","10-Dec-14","10-Nov-13","15-May-14","21-Apr-14","22-Apr-14","23-Apr-14")
depths<-c("_surface","_5cm","_15cm","_30cm","_50cm","_1m")

# read in and plot results per depth for a given site and date

soil.files.subset<-soil.files[grep(soil.files,pattern = "C1")] # specify site

################################### soil temp surface #####################################################################

soil.files.surface<-soil.files.subset[grep(soil.files.subset,pattern = depths[1])]

for(i in 1:length(soil.files.surface)){
  soil.surface<-read.csv(paste(soil.folder,soil.files.surface[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.surface)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.surface[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.surface$temperature<-type.convert(sub("\\p{So}C", "", soil.surface$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.surface$date_time<-as.POSIXct(soil.surface$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
  soil.surfaces<-soil.surface
  }else{
  soil.surfaces<-rbind(soil.surfaces,soil.surface)
  }
}
soil.surfaces<-soil.surfaces[order(soil.surfaces$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,soil.title),ylim=c(10,70))) # plot the data
  with(soil.surfaces,points(temperature~date_time,type='l',col='red')) # plot the data

################################### soil temp 5cm #####################################################################

soil.files.5cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[2])]

for(i in 1:length(soil.files.5cm)){
  soil.5cm<-read.csv(paste(soil.folder,soil.files.5cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.5cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.5cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.5cm$temperature<-type.convert(sub("\\p{So}C", "", soil.5cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.5cm$date_time<-as.POSIXct(soil.5cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
  soil.5cms<-soil.5cm
  }else{
  soil.5cms<-rbind(soil.5cms,soil.5cm)
  }
}
soil.5cms<-soil.5cms[order(soil.5cms$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,soil.title),ylim=c(10,70))) # plot the data
  with(soil.5cms,points(temperature~date_time,type='l',col='red')) # plot the data

################################### soil temp 15cm #####################################################################

soil.files.15cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[3])]

for(i in 1:length(soil.files.15cm)){
  soil.15cm<-read.csv(paste(soil.folder,soil.files.15cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.15cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.15cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.15cm$temperature<-type.convert(sub("\\p{So}C", "", soil.15cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.15cm$date_time<-as.POSIXct(soil.15cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
  soil.15cms<-soil.15cm
  }else{
  soil.15cms<-rbind(soil.15cms,soil.15cm)
  }
}
soil.15cms<-soil.15cms[order(soil.15cms$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,soil.title),ylim=c(10,70))) # plot the data
  with(soil.15cms,points(temperature~date_time,type='l',col='red')) # plot the data

################################### soil temp 30cm#####################################################################

soil.files.30cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[4])]

for(i in 1:length(soil.files.30cm)){
  soil.30cm<-read.csv(paste(soil.folder,soil.files.30cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.30cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.30cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.30cm$temperature<-type.convert(sub("\\p{So}C", "", soil.30cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.30cm$date_time<-as.POSIXct(soil.30cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
  soil.30cms<-soil.30cm
  }else{
  soil.30cms<-rbind(soil.30cms,soil.30cm)
  }
}
soil.30cms<-soil.30cms[order(soil.30cms$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,soil.title),ylim=c(10,70))) # plot the data
  with(soil.30cms,points(temperature~date_time,type='l',col='red')) # plot the data

################################### soil temp 50cm#####################################################################

soil.files.50cm<-soil.files.subset[grep(soil.files.subset,pattern = depths[5])]

for(i in 1:length(soil.files.50cm)){
  soil.50cm<-read.csv(paste(soil.folder,soil.files.50cm[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.50cm)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.50cm[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.50cm$temperature<-type.convert(sub("\\p{So}C", "", soil.50cm$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.50cm$date_time<-as.POSIXct(soil.50cm$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
  soil.50cms<-soil.50cm
  }else{
  soil.50cms<-rbind(soil.50cms,soil.50cm)
  }
}
soil.50cms<-soil.50cms[order(soil.50cms$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,soil.title),ylim=c(10,70))) # plot the data
  with(soil.50cms,points(temperature~date_time,type='l',col='red')) # plot the data

################################### soil temp 1m#####################################################################

soil.files.1m<-soil.files.subset[grep(soil.files.subset,pattern = depths[6])]

for(i in 1:length(soil.files.1m)){
  soil.1m<-read.csv(paste(soil.folder,soil.files.1m[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(soil.1m)<-c('date_time','temperature') #give the columns names
  soil.title<-soil.files.1m[i]
  soil.title<-str_replace_all(soil.title,'.txt','') #get rid of ".txt"

  soil.1m$temperature<-type.convert(sub("\\p{So}C", "", soil.1m$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  soil.1m$date_time<-as.POSIXct(soil.1m$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
  soil.1ms<-soil.1m
  }else{
  soil.1ms<-rbind(soil.1ms,soil.1m)
  }
}
soil.1ms<-soil.1ms[order(soil.1ms$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,soil.title),ylim=c(10,70))) # plot the data
  with(soil.1ms,points(temperature~date_time,type='l',col='red')) # plot the data

################################### adult copper model full shade #####################################################################

adult.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/adult models/"
adult.folders<-list.dirs(adult.folder)[-1]
adult.folders.fullshade<-adult.folders[grep(adult.folders,pattern = "full shade")]
adult.files.fullshade<-list.files(adult.folders.fullshade)
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = ".txt")]
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = "C1")]

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

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,adult.title),ylim=c(10,70))) # plot the data
  with(adult.fullshade,points(temperature~date_time,type='l',col='red')) # plot the data

################################### adult copper model full sun #####################################################################

adult.folders.fullsun<-adult.folders[grep(adult.folders,pattern = "full sun")]
if(length(adult.folders.fullsun)>1){
  adult.folders.fullsun<-adult.folders.fullsun[1]
}
adult.files.fullsun<-list.files(adult.folders.fullsun)
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = ".txt")]
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = "C1")]

# read and plot all data

for(i in 1:length(adult.files.fullsun)){
  adult.fullsun<-read.csv(paste(adult.folders.fullsun,"/",adult.files.fullsun[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(adult.fullsun)<-c('date_time','temperature') #give the columns names
  adult.title<-adult.files.fullsun[i]
  adult.title<-str_replace_all(adult.title,'.txt','') #get rid of ".txt"

  adult.fullsun$temperature<-type.convert(sub("\\p{So}C", "", adult.fullsun$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  adult.fullsun$date_time<-as.POSIXct(adult.fullsun$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   adult.fullsuns<-adult.fullsun
  }else{
   adult.fullsuns<-rbind(adult.fullsun,adult.fullsun)
  }
 }
adult.fullsun<-adult.fullsun[order(adult.fullsun$date_time),] 

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,adult.title),ylim=c(10,70))) # plot the data
  with(adult.fullsun,points(temperature~date_time,type='l',col='red')) # plot the data

################################### adult copper model part shade #####################################################################

adult.folders.partshade<-adult.folders[grep(adult.folders,pattern = "part shade")]
if(length(adult.folders.partshade)>1){
  adult.folders.partshade<-adult.folders.partshade[1]
}
adult.files.partshade<-list.files(adult.folders.partshade)
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = ".txt")]
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = "C1")]

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

  with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,adult.title),ylim=c(10,70))) # plot the data
  with(adult.partshade,points(temperature~date_time,type='l',col='red')) # plot the data

################################### juvenile copper model full shade #####################################################################

juvenile.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Juvenile models/"
juvenile.folders<-list.dirs(juvenile.folder)[-1]
juvenile.folders.fullshade<-juvenile.folders[grep(juvenile.folders,pattern = "full shade")]
juvenile.files.fullshade<-list.files(juvenile.folders.fullshade)
juvenile.files.fullshade<-juvenile.files.fullshade[grep(juvenile.files.fullshade,pattern = ".txt")]
juvenile.files.fullshade<-juvenile.files.fullshade[grep(juvenile.files.fullshade,pattern = "C1")]

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
juvenile.files.fullsun<-juvenile.files.fullsun[grep(juvenile.files.fullsun,pattern = "C1")]

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
juvenile.files.partshade<-juvenile.files.partshade[grep(juvenile.files.partshade,pattern = "C1")]

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

################################### weather station #####################################################################

with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'rainfall'),ylim=c(10,70))) # plot the data
points(weather_obs$RAIN+10~weather_obs$TIMESTAMP,type='h',col='blue')
with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'Tair min'),ylim=c(10,70))) # plot the data
points(weather_obs$AirTemp_C_Min~weather_obs$TIMESTAMP,type='l',col='red')
with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'Tair max'),ylim=c(10,70))) # plot the data
points(weather_obs$AirTemp_C_Max~weather_obs$TIMESTAMP,type='l',col='red')
with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'solar'),ylim=c(10,70))) # plot the data
points(weather_obs$Solar_Avg/50+10~weather_obs$TIMESTAMP,type='l',col='red')
with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'air pressure'),ylim=c(10,70))) # plot the data
points(weather_obs$Barometer_KPa/2~weather_obs$TIMESTAMP,type='l',col='red')
with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'relative humditiy'),ylim=c(10,70))) # plot the data
points(weather_obs$RH_Avg~weather_obs$TIMESTAMP,type='l',col='red')
with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'evapotranspiration'),ylim=c(10,70))) # plot the data
points(weather_obs$ETo*100+10~weather_obs$TIMESTAMP,type='l',col='red')
with(GDS.data,plot(temperature~date_time,type='l',main=paste(GDS.title,'wind speed'),ylim=c(10,70))) # plot the data
points(weather_obs$WindSpeed_ms_Avg*5+10~weather_obs$TIMESTAMP,type='l',col='red')
