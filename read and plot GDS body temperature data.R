# this script reads in juvenile copper model data, converts it to the right format, and plots it

# load the packages needed for this session
library(stringr)

GDS.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/GDS body temps/"

GDS.files<-list.files(GDS.folder)
GDS.files<-GDS.files[grep(GDS.files,pattern = ".txt")]
GDS.files<-GDS.files[-grep(GDS.files,pattern = "backup")] # remove backups

# read and plot all data

for(i in 1:length(GDS.files)){
  data<-read.csv(paste(GDS.folder,GDS.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(data)<-c('date_time','temperature') #give the columns names
  title<-GDS.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  data$temperature<-type.convert(sub("\\p{So}C", "", data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  data$date_time<-as.POSIXct(data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(data,plot(temperature~date_time,type='l',main=title,ylim=c(-10,80))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}