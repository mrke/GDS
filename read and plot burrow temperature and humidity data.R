# this script reads in burrow temperature data, converts it to the right format, and plots it

# load the packages needed for this session
library(stringr)

burrow.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/burrow data/"
burrow.folders<-list.dirs(burrow.folder)[-1]

m<-3 # choose burrow folder
burrow.files<-list.files(burrow.folders[m])
burrow.files<-burrow.files[grep(burrow.files,pattern = ".txt")]
burrow.files<-burrow.files[-grep(burrow.files,pattern = "backup")] # remove backups

depths<-c("surf","mid","deep")

# read and plot all data

for(i in 1:length(burrow.files)){
  data<-read.csv(paste(burrow.folders[m],"/",burrow.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(data)<-c('date_time','temperature',"humidity") #give the columns names
  title<-burrow.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  data$temperature<-type.convert(sub("\\p{So}C", "", data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  data$date_time<-as.POSIXct(data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(data,plot(temperature~date_time,type='l',main=title,ylim=c(-10,60))) # plot the data
  with(data,plot(humidity~date_time,type='l',main=title,ylim=c(-10,110))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}