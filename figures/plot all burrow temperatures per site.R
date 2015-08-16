################################### burrow temperatures #####################################################################

burrow.folder<-"c:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/burrow data/"
burrow.folders<-list.dirs(burrow.folder)[-1]

par(mfrow = c(3,2)) # set up for 5 plots in 2 columns

m<-5 # choose burrow folder
for(m in 1:5){
burrow.files<-list.files(burrow.folders[m])
burrow.files<-burrow.files[grep(burrow.files,pattern = ".txt")]
burrow.files<-burrow.files[-grep(burrow.files,pattern = "backup")] # remove backups

################################### burrow surface #####################################################################

burrow.files.surf<-burrow.files[grep(burrow.files,pattern = "surf")] # get surface files

# read in all burrow surface data
for(j in 1:length(burrow.files.surf)){
  burrow.data.surf<-read.csv(paste(burrow.folders[m],"/",burrow.files.surf[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  if(ncol(burrow.data.surf)==3){
   colnames(burrow.data.surf)<-c('date_time','temperature',"humidity") #give the columns names
  }else{
   colnames(burrow.data.surf)<-c('date_time','temperature') #give the columns names
  }
  burrow.title<-burrow.files.surf[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"
  burrow.title<-str_replace_all(burrow.title,'surface','') #get rid of "surface"
  burrow.title<-str_replace_all(burrow.title,'30-Oct-13','') #get rid of "date"

  burrow.data.surf$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.surf$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.surf$date_time<-as.POSIXct(burrow.data.surf$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1){
  burrow.surf<-burrow.data.surf[,1:2]
  }else{
  burrow.surf<-rbind(burrow.surf,burrow.data.surf[,1:2])
  }
}
burrow.surf<-burrow.surf[order(burrow.surf$date_time),] 

with(burrow.surf,plot(temperature~date_time,type='l',col='red',ylim=c(-10,70),xlim=c(as.numeric(as.POSIXct("2013-10-01",origin="1970-01-01")),as.numeric(as.POSIXct("2014-12-01",origin="1970-01-01"))),main=burrow.title,xlab="",ylab="temperature (deg C)",xaxt = "n")) # plot the data
axis.POSIXct(side = 1, x = soil.surfaces$date_time,
             at = seq(ISOdate(2013,07,011), ISOdate(2015,01,01), "months"), format = "%b %y",
             las = 2)
  
################################### burrow mid #####################################################################

burrow.files.mid<-burrow.files[grep(burrow.files,pattern = "mid")] # get mid files

# read in all burrow mid data
for(j in 1:length(burrow.files.mid)){
  burrow.data.mid<-read.csv(paste(burrow.folders[m],"/",burrow.files.mid[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  if(ncol(burrow.data.mid)==3){
   colnames(burrow.data.mid)<-c('date_time','temperature',"humidity") #give the columns names
  }else{
   colnames(burrow.data.mid)<-c('date_time','temperature') #give the columns names
  }  
  burrow.title<-burrow.files.mid[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.mid$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.mid$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.mid$date_time<-as.POSIXct(burrow.data.mid$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1){
  burrow.mid<-burrow.data.mid[,1:2]
  }else{
  burrow.mid<-rbind(burrow.mid,burrow.data.mid[,1:2])
  }
}
burrow.mid<-burrow.mid[order(burrow.mid$date_time),] 

  with(burrow.mid,points(temperature~date_time,type='l',col='orange')) # plot the data

################################### burrow deep #####################################################################

burrow.files.deep<-burrow.files[grep(burrow.files,pattern = "deep")] # get deep files

# read in all burrow deep data
for(j in 1:length(burrow.files.deep)){
  burrow.data.deep<-read.csv(paste(burrow.folders[m],"/",burrow.files.deep[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  if(ncol(burrow.data.deep)==3){
   colnames(burrow.data.deep)<-c('date_time','temperature',"humidity") #give the columns names
  }else{
   colnames(burrow.data.deep)<-c('date_time','temperature') #give the columns names
  } 
  burrow.title<-burrow.files.deep[j]
  burrow.title<-str_replace_all(burrow.title,'.txt','') #get rid of ".txt"

  burrow.data.deep$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.deep$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  burrow.data.deep$date_time<-as.POSIXct(burrow.data.deep$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(j==1){
  burrow.deep<-burrow.data.deep[,1:2]
  }else{
  burrow.deep<-rbind(burrow.deep,burrow.data.deep[,1:2])
  }
}
burrow.deep<-burrow.deep[order(burrow.deep$date_time),] 

  with(burrow.deep,points(temperature~date_time,type='l',col='blue')) # plot the data
grid(NA, NULL) # grid only in y-direction

}