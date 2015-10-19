activity<-function(lizard,timestart,timefinish,GDS.folder,burrow.folder,soil.folder,adult.folder){
  
  ################ read in raw data and make initial guesses ###########

# load the packages needed for this session
library(stringr)
library(plyr)

#GDS.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/GDS body temps/"
#burrow.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/burrow data/"
#soil.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Soil Profiles/"
#adult.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/adult models/"

GDS.files<-list.files(GDS.folder)
GDS.files<-GDS.files[grep(GDS.files,pattern = ".txt")]
GDS.files<-GDS.files[-grep(GDS.files,pattern = "backup")] # remove backups

# read skink data
  GDS.data<-read.csv(paste(GDS.folder,GDS.files[lizard],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(GDS.data)<-c('date_time','temperature') #give the columns names
  GDS.title<-GDS.files[lizard]
  GDS.title<-str_replace_all(GDS.title,'.txt','') #get rid of ".txt"

  GDS.data$temperature<-type.convert(sub("\\p{So}C", "", GDS.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  GDS.data$date_time<-as.POSIXct(GDS.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum


GDS.data<-subset(GDS.data,date_time>=as.POSIXct(timestart,origin="1970-01-01",tz=tzone) & date_time<=as.POSIXct(timefinish,origin="1970-01-01",tz=tzone)+24*3600)


# burrow site to plot
#burrow_site<-5

# [1] DL 3 GDS 15"
# [2] GDS 13"     
# [3] GDS 16"     
# [4] GDS 18"     
# [5] GDS 22 T"   
# [6] GDS 9"

soil_site<-"C2" # choose from "C1","C2","C3","C3_northernest one","T1","T2","T2a","T3"
copper_site<-"C2" # choose from "C1","C2","C3","C3_northernest one","T1","T2","T2a","T3"

# read in weatherhawk data
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
weather_obs<-read.csv('Field Data/weatherhawk_10min.csv')
weather_obs$TIMESTAMP<-as.POSIXct(weather_obs$TIMESTAMP,format="%Y-%m-%d %H:%M:%S",tz=tzone)

################################### burrow temperatures #####################################################################

burrow.folders<-list.dirs(burrow.folder)[-1]

#m<-5 # choose burrow folder
# for(burrow_site in 1:6){
# burrow.files1<-list.files(burrow.folders[burrow_site])
# burrow.files1<-burrow.files1[grep(burrow.files1,pattern = ".txt")]
# burrow.files1<-burrow.files1[-grep(burrow.files1,pattern = "backup")] # remove backups
# if(burrow_site==1){
#  burrow.files<-burrow.files1
# }else{
#   burrow.files<-c(burrow.files,burrow.files1)
# }
# }
################################### burrow surface #####################################################################

# read in all burrow surface data
  for(burrow_site in 1:6){
burrow.files<-list.files(burrow.folders[burrow_site])
burrow.files<-burrow.files[grep(burrow.files,pattern = ".txt")]
burrow.files<-burrow.files[-grep(burrow.files,pattern = "backup")] # remove backups

    burrow.files.surf<-burrow.files[grep(burrow.files,pattern = "surf")] # get surface files

    for(j in 1:length(burrow.files.surf)){

  burrow.data.surf<-read.csv(paste(burrow.folders[burrow_site],"/",burrow.files.surf[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE)[,1:2] #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data.surf)<-c('date_time','temperature') #give the columns names
  burrow.title<-burrow.files.surf[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.surf$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.surf$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.surf$date_time<-as.POSIXct(burrow.data.surf$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1 & burrow_site==1){
  burrow.surf<-burrow.data.surf
  }else{
  burrow.surf<-rbind(burrow.surf,burrow.data.surf)
  }
}

################################### burrow mid #####################################################################

burrow.files.mid<-burrow.files[grep(burrow.files,pattern = "mid")] # get mid files

# read in all burrow mid data
for(j in 1:length(burrow.files.mid)){
  burrow.data.mid<-read.csv(paste(burrow.folders[burrow_site],"/",burrow.files.mid[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data.mid)<-c('date_time','temperature',"humidity") #give the columns names
  burrow.title<-burrow.files.mid[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.mid$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.mid$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.mid$date_time<-as.POSIXct(burrow.data.mid$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1 & burrow_site==1){
  burrow.mid<-burrow.data.mid
  }else{
  burrow.mid<-rbind(burrow.mid,burrow.data.mid)
  }
}

################################### burrow deep #####################################################################

burrow.files.deep<-burrow.files[grep(burrow.files,pattern = "deep")] # get deep files

# read in all burrow deep data
for(j in 1:length(burrow.files.deep)){
  burrow.data.deep<-read.csv(paste(burrow.folders[burrow_site],"/",burrow.files.deep[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data.deep)<-c('date_time','temperature',"hudeepity") #give the columns names
  burrow.title<-burrow.files.deep[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.deep$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.deep$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.deep$date_time<-as.POSIXct(burrow.data.deep$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1 & burrow_site==1){
  burrow.deep<-burrow.data.deep
  }else{
  burrow.deep<-rbind(burrow.deep,burrow.data.deep)
  }
}
} # end loop through all burrow sites

burrow.surf<-burrow.surf[order(burrow.surf$date_time),] 
burrow.mid<-burrow.mid[order(burrow.mid$date_time),] 
burrow.deep<-burrow.deep[order(burrow.deep$date_time),] 

################################### soil temperatures #####################################################################

soil.files<-list.files(soil.folder)
soil.files<-soil.files[grep(soil.files,pattern = ".txt")]
soil.files<-soil.files[-grep(soil.files,pattern = "backup")] # remove backups

sites<-c("C1","C2","C3","C3_northernest one","T1","T2","T2a","T3")
dates<-c("01-Jan-15","09-Dec-14","10-Dec-14","10-Nov-13","15-May-14","21-Apr-14","22-Apr-14","23-Apr-14")
depths<-c("_surface","_5cm","_15cm","_30cm","_50cm","_1m")

# read in and plot results per depth for a given site and date

soil.files.subset<-soil.files[grep(soil.files,pattern = soil_site)] # specify site

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

################################### adult copper model full shade #####################################################################

adult.folders<-list.dirs(adult.folder)[-1]
adult.folders.fullshade<-adult.folders[grep(adult.folders,pattern = "full shade")]
adult.files.fullshade<-list.files(adult.folders.fullshade)
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = ".txt")]
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = copper_site)]

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
   adult.fullshades<-rbind(adult.fullshades,adult.fullshade)
  }
 }
adult.fullshades<-adult.fullshades[order(adult.fullshades$date_time),] 

  
################################### adult copper model full sun #####################################################################

adult.folders.fullsun<-adult.folders[grep(adult.folders,pattern = "full sun")]
if(length(adult.folders.fullsun)>1){
  adult.folders.fullsun<-adult.folders.fullsun[1]
}
adult.files.fullsun<-list.files(adult.folders.fullsun)
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = ".txt")]
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = copper_site)]

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
   adult.fullsuns<-rbind(adult.fullsuns,adult.fullsun)
  }
 }
adult.fullsuns<-adult.fullsuns[order(adult.fullsuns$date_time),] 
             
################################### adult copper model part shade #####################################################################

adult.folders.partshade<-adult.folders[grep(adult.folders,pattern = "part shade")]
if(length(adult.folders.partshade)>1){
  adult.folders.partshade<-adult.folders.partshade[1]
}
adult.files.partshade<-list.files(adult.folders.partshade)
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = ".txt")]
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = copper_site)]

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
   adult.partshades<-rbind(adult.partshades,adult.partshade)
  }
 }
adult.partshades<-adult.partshades[order(adult.partshades$date_time),] 

################################### plot lizard data against obs #####################################################################

roundhalfhour <- function( x ) { # function that rounds time to the nearest half hour
 round<-as.POSIXct(round(as.numeric(x+30*60)/(60*30))*60*30,origin="1970-01-01")
 round[length(round)]<-round[length(round)]-60*1
 round
} 

# read in microclimate model output for obtaining solar data
metout<-read.csv('microclimate/metout.csv')
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-seq(ISOdate(2012,1,1,tz=tzone)-3600*12, ISOdate(2015,1,1,tz=tzone)-3600*13, by="hours")
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
metout$dates<-dates+60*30
pred_solar<-metout[,c(20,14)]
pred_n<-seq(1,nrow(pred_solar))
dates2<-seq(ISOdate(2012,1,1,tz=tzone)-3600*12, ISOdate(2015,1,1,tz=tzone)-3600*13, by=30*60)+30*60
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
sol2<-approxfun(y=pred_solar$SOLR,x=pred_n)
pred_solar2<-cbind(as.data.frame(as.POSIXct(dates2,tz=tzone)),as.data.frame(sol2(seq(1,length(pred_n),0.5))))
colnames(pred_solar2)<-c("dates","solar") 

days<-seq(as.POSIXct(timestart,tz=tzone), as.POSIXct(timefinish,tz=tzone), "days")
y1<-15 # y axis lower limit
y2<-50 # y axis upper limit
m<-0

pdf(file=paste("time budget/",GDS.title,"_timebudget.pdf",sep=""),paper="a4r",width=11,height=8)
  
for(i in 1:length(days)){
  sub.GDS.data<-subset(GDS.data,as.Date(GDS.data$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
  if(nrow(sub.GDS.data)>0){
    m<-m+1
    if(nrow(subset(weather_obs,as.Date(weather_obs$TIMESTAMP,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone)))>0){
    sub.weather<-subset(weather_obs,as.Date(weather_obs$TIMESTAMP,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    }
    sub.solar<-subset(pred_solar2,format(pred_solar2$dates,"%Y-%m-%d")==as.Date(days[i],tz=tzone))
    sub.burrow.deep<-subset(burrow.deep,as.Date(burrow.deep$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.burrow.surf<-subset(burrow.surf,as.Date(burrow.surf$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.burrow.mid<-subset(burrow.mid,as.Date(burrow.mid$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.soil.surface<-subset(soil.surfaces,as.Date(soil.surfaces$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.soil.5cm<-subset(soil.5cms,as.Date(soil.5cms$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.soil.15cm<-subset(soil.15cms,as.Date(soil.15cms$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.soil.30cm<-subset(soil.30cms,as.Date(soil.30cms$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.soil.50cm<-subset(soil.50cms,as.Date(soil.50cms$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.soil.1m<-subset(soil.1ms,as.Date(soil.1ms$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.adult.partshade<-subset(adult.partshades,as.Date(adult.partshades$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.adult.fullshade<-subset(adult.fullshades,as.Date(adult.fullshades$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))
    sub.adult.fullsun<-subset(adult.fullsuns,as.Date(adult.fullsuns$date_time,format="%Y-%m-%d",tz=tzone)==as.Date(days[i],tz=tzone))

    
    h.GDS<-aggregate(sub.GDS.data$temperature,by=list(format(roundhalfhour(sub.GDS.data$date_time), "%H:%M")), FUN=mean)
    h.solar<-aggregate(sub.weather$Solar_Avg,by=list(format(roundhalfhour(sub.weather$TIMESTAMP), "%H:%M")), FUN=mean)
    h.solar2<-aggregate(sub.solar$solar,by=list(format(roundhalfhour(sub.solar$dates), "%H:%M")), FUN=mean)
    h.solar2[48,1]<-h.solar[48,1]
    h.solar2<-rbind(h.solar2,c("23:59",0))
    h.GDS<-rbind(c(h.solar[1,1],h.GDS[1,2]),h.GDS)
    h.GDS$x<-as.numeric(h.GDS$x)
    h.burrow.surf<-aggregate(sub.burrow.surf[,1:2],by=list(format(roundhalfhour(sub.burrow.surf$date_time), "%H:%M")), FUN=mean)
    h.burrow.surf<-h.burrow.surf[order(h.burrow.surf$date_time),]
    h.burrow.mid<-aggregate(sub.burrow.mid[,1:2],by=list(format(roundhalfhour(sub.burrow.mid$date_time), "%H:%M")), FUN=mean)
    h.burrow.mid<-h.burrow.mid[order(h.burrow.mid$date_time),]
    h.burrow.deep<-aggregate(sub.burrow.deep[,1:2],by=list(format(roundhalfhour(sub.burrow.deep$date_time), "%H:%M")), FUN=mean)
    h.burrow.deep<-h.burrow.deep[order(h.burrow.deep$date_time),]
    h.soil.surface<-aggregate(sub.soil.surface$temperature,by=list(format(roundhalfhour(sub.soil.surface$date_time), "%H:%M")), FUN=mean)
    h.soil.5cm<-aggregate(sub.soil.5cm$temperature,by=list(format(roundhalfhour(sub.soil.5cm$date_time), "%H:%M")), FUN=mean)
    h.soil.15cm<-aggregate(sub.soil.15cm$temperature,by=list(format(roundhalfhour(sub.soil.15cm$date_time), "%H:%M")), FUN=mean)
    h.soil.30cm<-aggregate(sub.soil.30cm$temperature,by=list(format(roundhalfhour(sub.soil.30cm$date_time), "%H:%M")), FUN=mean)
    h.soil.50cm<-aggregate(sub.soil.50cm$temperature,by=list(format(roundhalfhour(sub.soil.50cm$date_time), "%H:%M")), FUN=mean)
    h.soil.1m<-aggregate(sub.soil.1m$temperature,by=list(format(roundhalfhour(sub.soil.1m$date_time), "%H:%M")), FUN=mean)
    h.adult.partshade<-aggregate(sub.adult.partshade$temperature,by=list(format(roundhalfhour(sub.adult.partshade$date_time), "%H:%M")), FUN=mean)
    h.adult.fullshade<-aggregate(sub.adult.fullshade$temperature,by=list(format(roundhalfhour(sub.adult.fullshade$date_time), "%H:%M")), FUN=mean)
    h.adult.fullsun<-aggregate(sub.adult.fullsun$temperature,by=list(format(roundhalfhour(sub.adult.fullsun$date_time), "%H:%M")), FUN=mean)
    h.data.merge<-merge(h.solar2,h.solar,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.GDS,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.burrow.surf[,c(1,3)],all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.burrow.mid[,c(1,3)],all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.burrow.deep[,c(1,3)],all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.soil.surface,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.soil.5cm,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.soil.15cm,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.soil.30cm,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.soil.50cm,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.soil.1m,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.adult.fullsun,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.adult.partshade,all.x=TRUE,by="Group.1")
    h.data.merge<-merge(h.data.merge,h.adult.fullshade,all.x=TRUE,by="Group.1")
    h.data<-as.data.frame(cbind(seq(0.5,24,0.5),h.data.merge[2:49,2:16]))

    #h.data<-as.data.frame(cbind(seq(0,23.5,0.5),h.solar[,2],h.solar2[,2],h.GDS[,2],h.burrow.surf[,3],h.burrow.mid[,3],h.burrow.deep[,3],h.soil.surface[,2],h.soil.5cm[,2],h.soil.15cm[,2],h.soil.30cm[,2],h.soil.50cm[,2],h.soil.1m[,2],h.adult.fullsun[,2],h.adult.partshade[,2],h.adult.fullshade[,2]))
    colnames(h.data)<-c("hour","pred_solar","solar","Tb","b.surf","b.mid","b.deep","s.1cm","s.5cm","s.15cm","s.30cm","s.50cm","s.1m","c.sun","c.part","c.shd")
    mins<-abs(as.data.frame(h.data[,4]-h.data[,c(5,9:16)])) # just surface burrow, plus soil from 5 cm down (and copper models)
    #mins<-abs(as.data.frame(h.data[,3]-h.data[,c(4:6,13:15)])) # just burrow (and copper models)
    #mins<-abs(as.data.frame(h.data[,3]-h.data[,7:15])) # just soil (and copper models)
    #mins<-abs(as.data.frame(h.data[,3]-h.data[,4:15])) # burrow plus soil (and copper models)

    location<-names(mins)[apply( mins, 1, which.min)]
    h.data$loc<-location
    
    if(m==1){
      h.data.all<-cbind(days[i],h.data)
    }else{
      h.data.all<-rbind(h.data.all,cbind(days[i],h.data))
    }
    
    with(sub.GDS.data,plot(temperature~date_time,type='l',lwd=2,col='black',ylim=c(y1,y2),main=GDS.title,xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
    axis.POSIXct(side = 1, x = GDS.data$date_time,
      at = seq(as.POSIXct(timestart,tz=tzone), as.POSIXct(timefinish,tz=tzone), "hours"), format = "%d %b %H",
      las = 2)
     with(soil.5cms,points(temperature~date_time,type='l',lwd=1,col='dark grey')) # plot the data
     with(soil.15cms,points(temperature~date_time,type='l',lty=2,lwd=2,col='dark grey')) # plot the data  
     with(soil.30cms,points(temperature~date_time,type='l',lwd=3,col='green')) # plot the data  
     with(soil.50cms,points(temperature~date_time,type='l',lwd=4,col='dark grey')) # plot the data  
     with(soil.1ms,points(temperature~date_time,type='l',lwd=5,col='dark grey')) # plot the data  
  
    points(weather_obs$Solar_Avg/75+y1~weather_obs$TIMESTAMP,type='h',col='gold')
    points(pred_solar2$solar/75+y1~pred_solar2$dates,type='h',col='red')
    with(sub.GDS.data,points(temperature~date_time,type='l',lwd=2,col='black')) # plot the data
    with(h.burrow.surf,points(temperature~date_time,type='l',col='red')) # plot the data
    #with(h.burrow.mid,points(temperature~date_time,type='l',col='orange')) # plot the data
    #with(h.burrow.deep,points(temperature~date_time,type='l',col='blue')) # plot the data

    with(adult.partshades,points(temperature~date_time,type='l',lty=2,col='orange')) # plot the data
    with(adult.fullsuns,points(temperature~date_time,type='l',lty=2,col='red')) # plot the data
    with(adult.fullshades,points(temperature~date_time,type='l',lty=2,col='blue')) # plot the data
#     with(soil.surfaces,points(temperature~date_time,type='l',lty=1,col='dark grey')) # plot the data
      points(weather_obs$RAIN+y1-1~weather_obs$TIMESTAMP,type='h',col='blue')
#     points(weather_obs$AirTemp_C_Min~weather_obs$TIMESTAMP,type='l',col='blue')
#     points(weather_obs$AirTemp_C_Max~weather_obs$TIMESTAMP,type='l',col='red')
#      points(weather_obs$RH_Avg~weather_obs$TIMESTAMP,type='l',col='light blue')
#     points(weather_obs$WindSpeed_ms_Avg*5+y1~weather_obs$TIMESTAMP,type='l',col='red')
grid(NA, NULL) # grid only in x-direction
abline( v=seq(as.POSIXct(days[i],tz=tzone), as.POSIXct(days[i]+3600*23+3600,tz=tzone), 3600/2), col="dark gray", lty=3)
text(seq(as.POSIXct(days[i],tz=tzone), as.POSIXct(days[i]+3600*23+3600,tz=tzone), 3600/2)+3600/4, 45, h.data$loc,srt=90)
  }
  cat(paste("day ",i," of ",length(days)," done \n",sep=""))
}
dev.off()
write.csv(h.data.all,paste("time budget/",GDS.title,"_timebudget.csv",sep=""))
return(GDS.title)
}