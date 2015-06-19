# script to plot GDS Tbs against available temperatures

# load the packages needed for this session
library(stringr)

GDS.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/GDS body temps/"

GDS.files<-list.files(GDS.folder)
GDS.files<-GDS.files[grep(GDS.files,pattern = ".txt")]
GDS.files<-GDS.files[-grep(GDS.files,pattern = "backup")] # remove backups

# read and plot all data for first skink
i<-1
  GDS.data<-read.csv(paste(GDS.folder,GDS.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(GDS.data)<-c('date_time','temperature') #give the columns names
  title<-GDS.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  GDS.data$temperature<-type.convert(sub("\\p{So}C", "", GDS.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  GDS.data$date_time<-as.POSIXct(GDS.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(GDS.data,plot(temperature~date_time,type='l',main=title,ylim=c(10,40))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files

burrow.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/burrow data/"
burrow.folders<-list.dirs(burrow.folder)[-1]

m<-5 # choose burrow folder
burrow.files<-list.files(burrow.folders[m])
burrow.files<-burrow.files[grep(burrow.files,pattern = ".txt")]
burrow.files<-burrow.files[-grep(burrow.files,pattern = "backup")] # remove backups

burrow.files.sub<-burrow.files[grep(burrow.files,pattern = "10-Dec-13")] # find correct date

depths<-c("surf","mid","deep")

# read and plot all data
for(j in 1:length(burrow.files.sub)){
  burrow.data<-read.csv(paste(burrow.folders[m],"/",burrow.files.sub[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data)<-c('date_time','temperature',"humidity") #give the columns names
  title<-burrow.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  burrow.data$temperature<-type.convert(sub("\\p{So}C", "", burrow.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data$date_time<-as.POSIXct(burrow.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(burrow.data,points(temperature~date_time,type='l',main=title,ylim=c(-10,60),col=(j+1))) # plot the data
  #with(data,plot(humidity~date_time,type='l',main=title,ylim=c(-10,110))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}

burrow.files.sub<-burrow.files[grep(burrow.files,pattern = "30-Oct-13")] # find correct date

depths<-c("surf","mid","deep")

# read and plot all data
for(j in 1:length(burrow.files.sub)){
  burrow.data<-read.csv(paste(burrow.folders[m],"/",burrow.files.sub[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data)<-c('date_time','temperature',"humidity") #give the columns names
  title<-burrow.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  burrow.data$temperature<-type.convert(sub("\\p{So}C", "", burrow.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data$date_time<-as.POSIXct(burrow.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(burrow.data,points(temperature~date_time,type='l',main=title,ylim=c(-10,60),col=(j+1))) # plot the data
  #with(data,plot(humidity~date_time,type='l',main=title,ylim=c(-10,110))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}

burrow.files.sub<-burrow.files[grep(burrow.files,pattern = "09-Mar-14")] # find correct date

depths<-c("surf","mid","deep")

# read and plot all data
for(j in 1:length(burrow.files.sub)){
  burrow.data<-read.csv(paste(burrow.folders[m],"/",burrow.files.sub[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data)<-c('date_time','temperature',"humidity") #give the columns names
  title<-burrow.files.sub[j]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  burrow.data$temperature<-type.convert(sub("\\p{So}C", "", burrow.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data$date_time<-as.POSIXct(burrow.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(burrow.data,points(temperature~date_time,type='l',main=title,ylim=c(-10,60),col=(j+1))) # plot the data
  #with(data,plot(humidity~date_time,type='l',main=title,ylim=c(-10,110))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}


###################### second skink #######################################

# read and plot all data for first skink
i<-2
  GDS.data<-read.csv(paste(GDS.folder,GDS.files[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(GDS.data)<-c('date_time','temperature') #give the columns names
  title<-GDS.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  GDS.data$temperature<-type.convert(sub("\\p{So}C", "", GDS.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  GDS.data$date_time<-as.POSIXct(GDS.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(GDS.data,plot(temperature~date_time,type='l',main=title,ylim=c(10,40))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files

burrow.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/burrow data/"
burrow.folders<-list.dirs(burrow.folder)[-1]

m<-2 # choose burrow folder
burrow.files<-list.files(burrow.folders[m])
burrow.files<-burrow.files[grep(burrow.files,pattern = ".txt")]
burrow.files<-burrow.files[-grep(burrow.files,pattern = "backup")] # remove backups

burrow.files.sub<-burrow.files[grep(burrow.files,pattern = "10-Dec-13")] # find correct date

depths<-c("surf","mid","deep")

# read and plot all data
for(j in 1:length(burrow.files.sub)){
  burrow.data<-read.csv(paste(burrow.folders[m],"/",burrow.files.sub[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data)<-c('date_time','temperature',"humidity") #give the columns names
  title<-burrow.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  burrow.data$temperature<-type.convert(sub("\\p{So}C", "", burrow.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data$date_time<-as.POSIXct(burrow.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(GDS.data,plot(temperature~date_time,type='l',main=title,ylim=c(10,40))) # plot the data
  with(burrow.data,points(temperature~date_time,type='l',main=title,ylim=c(-10,60),col=(j+1))) # plot the data
  #with(data,plot(humidity~date_time,type='l',main=title,ylim=c(-10,110))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}

burrow.files.sub<-burrow.files[grep(burrow.files,pattern = "30-Oct-13")] # find correct date

depths<-c("surf","mid","deep")

# read and plot all data
for(j in 1:length(burrow.files.sub)){
  burrow.data<-read.csv(paste(burrow.folders[m],"/",burrow.files.sub[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data)<-c('date_time','temperature',"humidity") #give the columns names
  title<-burrow.files[i]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  burrow.data$temperature<-type.convert(sub("\\p{So}C", "", burrow.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data$date_time<-as.POSIXct(burrow.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(GDS.data,plot(temperature~date_time,type='l',main=title,ylim=c(10,40))) # plot the data
  with(burrow.data,points(temperature~date_time,type='l',main=title,ylim=c(-10,60),col=(j+1))) # plot the data
  #with(data,plot(humidity~date_time,type='l',main=title,ylim=c(-10,110))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}

burrow.files.sub<-burrow.files[grep(burrow.files,pattern = "09-Mar-14")] # find correct date

depths<-c("surf","mid","deep")

# read and plot all data
for(j in 1:length(burrow.files.sub)){
  burrow.data<-read.csv(paste(burrow.folders[m],"/",burrow.files.sub[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(burrow.data)<-c('date_time','temperature',"humidity") #give the columns names
  title<-burrow.files.sub[j]
  title<-str_replace_all(title,'.txt','') #get rid of ".txt"

  burrow.data$temperature<-type.convert(sub("\\p{So}C", "", burrow.data$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data$date_time<-as.POSIXct(burrow.data$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  filename<-paste(title,".pdf",sep="")
  #pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out adn the 'dev.off()' line below if you want to see them in R
  with(GDS.data,plot(temperature~date_time,type='l',main=title,ylim=c(10,40))) # plot the data
  with(burrow.data,points(temperature~date_time,type='l',main=title,ylim=c(-10,60),col=(j+1))) # plot the data
  #with(data,plot(humidity~date_time,type='l',main=title,ylim=c(-10,110))) # plot the data
  #dev.off() # uncomment me and the pdf line above if you want the plots to go to pdf files
}














