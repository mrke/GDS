################################### soil temperatures #####################################################################

soil.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Soil Profiles/"
soil.files<-list.files(soil.folder)
soil.files<-soil.files[grep(soil.files,pattern = ".txt")]
soil.files<-soil.files[-grep(soil.files,pattern = "backup")] # remove backups

sites<-c("C1","T1","C2","T2","C3","T3")
dates<-c("01-Jan-15","09-Dec-14","10-Dec-14","10-Nov-13","15-May-14","21-Apr-14","22-Apr-14","23-Apr-14")
depths<-c("_surface","_5cm","_15cm","_30cm","_50cm","_1m")

par(mfrow = c(3,2)) # set up for 6 plots in 2 columns

# read in and plot results per depth for a given site and date
site<-"C2"
for(k in 1:length(sites)){
site<-sites[k]
if(site!="T3"){
soil.files.subset<-soil.files[grep(soil.files,pattern = site)] # specify site
}else{
soil.files.subset<-soil.files[grep(soil.files,pattern = "T3|T2a")] # specify site
}  
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

  with(soil.surfaces,plot(temperature~date_time,type='l',col='red',ylim=c(-10,70),xlim=c(as.numeric(as.POSIXct("2013-09-01",origin="1970-01-01")),as.numeric(as.POSIXct("2014-10-01",origin="1970-01-01"))),main=site,xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
axis.POSIXct(side = 1, x = soil.surfaces$date_time,
             at = seq(ISOdate(2013,08,011), ISOdate(2014,11,01), "months"), format = "%b %y",
             las = 2)
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

  with(soil.5cms,points(temperature~date_time,type='l',col='orange')) # plot the data

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

  with(soil.15cms,points(temperature~date_time,type='l',col='brown')) # plot the data

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

  with(soil.30cms,points(temperature~date_time,type='l',col='light blue')) # plot the data

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

  with(soil.50cms,points(temperature~date_time,type='l',col='blue')) # plot the data

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

  with(soil.1ms,points(temperature~date_time,type='l',col='black')) # plot the data
  
grid(NA, NULL) # grid only in y-direction

} # end loop through all sites