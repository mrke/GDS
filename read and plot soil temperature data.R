# this script reads in soil temperature data, converts it to the right format, and plots it

# load the packages needed for this session
library(stringr)

#soil.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Soil Profiles/"
soil.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/Soil profiles/"
soil.files<-list.files(soil.folder)
soil.files<-soil.files[grep(soil.files,pattern = ".txt")]
soil.files<-soil.files[-grep(soil.files,pattern = "backup")] # remove backups

sites<-c("C1","C2","C3","C3_northernest one","T1","T2","T2a","T3")
dates<-c("01-Jan-15","09-Dec-14","10-Dec-14","10-Nov-13","15-May-14","21-Apr-14","22-Apr-14","23-Apr-14")
depths<-c("_surface","_5cm","_15cm","_30cm","_50cm","_1m")

# read in and plot results per depth for a given site and date

soil.files.subset<-soil.files[grep(soil.files,pattern = "C1")] # specify site
soil.files.subset<-soil.files[grep(soil.files.subset,pattern = "10-Dec-14")] # specify date

for(i in 1:length(soil.files.subset)){
  soil.files.subset2<-soil.files.subset[grep(soil.files.subset,pattern = depths[i])]
  data<-read.csv(paste(soil.folder,soil.files.subset2,sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(data)<-c('date_time','temperature') #give the columns names
  title<-soil.files.subset2
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  data$temperature<-type.convert(sub("\\p{So}C", "", data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  data$date_time<-as.POSIXct(data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   with(data,plot(temperature~date_time,type='l',main=title,ylim=c(-10,70),col=i)) # plot the data
  }else{
   with(data,points(temperature~date_time,type='l',main=title,ylim=c(-10,70),col=i)) # plot the data
  }
}

# read and plot all data

for(i in 1:length(soil.files)){
  data<-read.csv(paste(soil.folder,soil.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(data)<-c('date_time','temperature') #give the columns names
  title<-soil.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  data$temperature<-type.convert(sub("\\p{So}C", "", data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  data$date_time<-as.POSIXct(data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(data,plot(temperature~date_time,type='l',main=title,ylim=c(-10,60))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}