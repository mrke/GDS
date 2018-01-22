library(NicheMapR) # load the NicheMapR package to get the function for calculating vapour density from relative humidity and air temp
# to install NicheMapR, follow instructions at https://camelunimelb.wordpress.com/resources/ 
library(zoo)
library(stringr)
st.err <- function(x) { # a function specified for the standard error, i.e. the standard error divided by the square root of the sample size
  sd(x)/sqrt(length(x))
}

# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"
#dropbox="c:/Users/Danae/My documents/gds/"
#gds<-"C:/Users/Danae/My documents/gds/"

################################### burrow temperatures #####################################################################

burrow.folder<-paste0(dropbox,"raw data/burrow data")
burrow.folders<-list.dirs(burrow.folder)[-1]
burrows<-strsplit(burrow.folders, "/")
burrows<-as.character(sapply(burrows, "[[", length(burrows[[1]]))) # get burrow names)

# loop through sites and put all data for a given depth in one table, with site name as an extra column
for(m in 1:6){ # loop through sites
  burrow.files<-list.files(burrow.folders[m]) # get list of all files in folder
  burrow.files<-burrow.files[grep(burrow.files,pattern = ".txt")] # get just the .txt files
  burrow.files<-burrow.files[-grep(burrow.files,pattern = "backup")] # remove backups (notice -grep)
  if(m==4){
   burrow.files<-burrow.files[-grep(burrow.files,pattern = "ground_dont", fixed=TRUE)] # remove data from logger that ended up on surface
  }
  ################################### burrow surface #####################################################################
  
  burrow.files.surf<-burrow.files[grep(burrow.files,pattern = "surf")] # get list of just surface files
  
  # read in all burrow surface data
  for(j in 1:length(burrow.files.surf)){ # start loop through surface files
    burrow.data.surf<-read.csv(paste(burrow.folders[m],"/",burrow.files.surf[j],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
    if(ncol(burrow.data.surf)==3){ # we have humidity data
      colnames(burrow.data.surf)<-c('date_time','temperature',"humidity") #give the columns names
    }else{ # we just have temperature data
      colnames(burrow.data.surf)<-c('date_time','temperature') #give the columns names
    }
    burrow.data.surf$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.surf$temperature, perl = TRUE)) # trick to get rid of degree C symbol
    burrow.data.surf$date_time<-as.POSIXct(burrow.data.surf$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
    if(ncol(burrow.data.surf)==3){ # we have humidity data so also compute vapour density
      burrow.data.surf$vd<-WETAIR(db = burrow.data.surf$temperature, rh = burrow.data.surf$humidity)$vd
    }else{ # we have just temperature data so add blank columns for humidity and vapour pressure
      burrow.data.surf$humidity=""
      burrow.data.surf$vd=""
    }
    if(j==1){ # first surface file for this site, just bind the burrow name as a new column, and make sure strings don't turn into factors
      burrow.surf<-cbind(burrow.data.surf,burrows[m],stringsAsFactors=FALSE)
    }else{ # subsequent surface file for this site - row-bind the last results to this set
      burrow.surf<-rbind(burrow.surf,cbind(burrow.data.surf,burrows[m],stringsAsFactors=FALSE))
    }
  }
  
  if(m==1){ # first site
    all.burrow.surf=burrow.surf
  }else{ # subsequent site - row-bind the last site's results to this set
    all.burrow.surf=rbind(all.burrow.surf,burrow.surf)
  }
  
  
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
    burrow.data.mid$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.mid$temperature, perl = TRUE)) # trick to get rid of degree C symbol
    burrow.data.mid$date_time<-as.POSIXct(burrow.data.mid$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
    if(ncol(burrow.data.mid)==3){
      burrow.data.mid$vd<-WETAIR(db = burrow.data.mid$temperature, rh = burrow.data.mid$humidity)$vd
    }else{
      burrow.data.mid$humidity=""
      burrow.data.mid$vd=""
    }
    if(j==1){
      burrow.mid<-cbind(burrow.data.mid,burrows[m],stringsAsFactors=FALSE)
    }else{
      burrow.mid<-rbind(burrow.mid,cbind(burrow.data.mid,burrows[m],stringsAsFactors=FALSE))
    }
  }
  
  if(m==1){
    all.burrow.mid=burrow.mid
  }else{
    all.burrow.mid=rbind(all.burrow.mid,burrow.mid)
  }
  
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
    burrow.data.deep$temperature<-type.convert(sub("\\p{So}C", "", burrow.data.deep$temperature, perl = TRUE)) # trick to get rid of degree C symbol
    burrow.data.deep$date_time<-as.POSIXct(burrow.data.deep$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
    if(ncol(burrow.data.deep)==3){
      burrow.data.deep$vd<-WETAIR(db = burrow.data.deep$temperature, rh = burrow.data.deep$humidity)$vd
    }else{
      burrow.data.deep$humidity=""
      burrow.data.deep$vd=""
    }
    if(j==1){
      burrow.deep<-cbind(burrow.data.deep,burrows[m],stringsAsFactors=FALSE)
    }else{
      burrow.deep<-rbind(burrow.deep,cbind(burrow.data.deep,burrows[m],stringsAsFactors=FALSE))
    }
  }
  
  if(m==1){
    all.burrow.deep=burrow.deep
  }else{
    all.burrow.deep=rbind(all.burrow.deep,burrow.deep)
  }
  
} # end of loop through all sites for all depths

# convert text to numbers
all.burrow.surf[,3:4]<-as.numeric(unlist(all.burrow.surf[,3:4]))
all.burrow.mid[,3:4]<-as.numeric(unlist(all.burrow.mid[,3:4]))
all.burrow.deep[,3:4]<-as.numeric(unlist(all.burrow.deep[,3:4]))
# put in date order
all.burrow.surf<-all.burrow.surf[order(all.burrow.surf$date_time),] 
all.burrow.mid<-all.burrow.mid[order(all.burrow.mid$date_time),] 
all.burrow.deep<-all.burrow.deep[order(all.burrow.deep$date_time),] 
# remove negative humidities
all.burrow.surf$humidity[all.burrow.surf$humidity<0]<-0
all.burrow.mid$humidity[all.burrow.mid$humidity<0]<-0
all.burrow.deep$humidity[all.burrow.deep$humidity<0]<-0
# add column names
colnames(all.burrow.surf)<-c("date_time","temperature","humidity","vd","site")
colnames(all.burrow.mid)<-c("date_time","temperature","humidity","vd","site")
colnames(all.burrow.deep)<-c("date_time","temperature","humidity","vd","site")

burrow.remove<-c("25/09/2013","26/09/2013","30/10/2013","11/12/2013","06/03/2014","07/03/2014","08/03/2014","09/03/2014", "10/03/2014"," 02/06/2014","17/08/2014","10/11/2014", "27/08/2014","04/10/2014","27/09/2013","28/09/2013","29/09/2013","30/09/2013","01/10/2013","02/10/2013","03/10/2013", "04/10/2013","03/06/2014","16/08/2014","10/12/2013","02/06/2014","17/08/2014","10/11/2014","08/03/2014","07/03/2014","06/03/2014","05/03/2014")

all.burrow.surf<-subset(all.burrow.surf, !(as.character(all.burrow.surf$date_time, format="%d/%m/%Y")%in% burrow.remove))
all.burrow.mid<-subset(all.burrow.mid, !(as.character(all.burrow.mid$date_time, format="%d/%m/%Y")%in% burrow.remove))
all.burrow.deep<-subset(all.burrow.deep, !(as.character(all.burrow.deep$date_time, format="%d/%m/%Y")%in% burrow.remove))


################################## aggregation of data per day and per site ################################################################

# aggregate to daily max over all sites ('by' argument has just date specified)
# (this aggregate command gets the max per group, the group being specified in the 'by' argument
# which, in the first case, is the 'date_time' column of the 'all.burrow.surf' table
# formatted to just have the date and not the time, and so forth through the mid and deep)
max.surf=aggregate(all.burrow.surf, by=list(format(all.burrow.surf$date_time,"%d-%m-%Y")), FUN = max) 
max.mid=aggregate(all.burrow.mid, by=list(format(all.burrow.mid$date_time,"%d-%m-%Y")), FUN = max)
max.deep=aggregate(all.burrow.deep, by=list(format(all.burrow.deep$date_time,"%d-%m-%Y")), FUN = max)
max.surf<-max.surf[order(max.surf$date_time),] # put in date order (otherwise plots look bad)
max.mid<-max.mid[order(max.mid$date_time),] # put in date order (otherwise plots look bad)
max.deep<-max.deep[order(max.deep$date_time),] # put in date order (otherwise plots look bad)

# aggregate to daily max per site ('by' argument has both date and site specified) - note specifing
# only columns 2-4 to aggregate - i.e. the temperature, humidity and vapour pressure
max.surf.site=aggregate(all.burrow.surf[,2:4], by=list(format(all.burrow.surf$date_time,"%d-%m-%Y"),all.burrow.surf$site), FUN = max)
max.mid.site=aggregate(all.burrow.mid[,2:4], by=list(format(all.burrow.mid$date_time,"%d-%m-%Y"),all.burrow.mid$site), FUN = max)
max.deep.site=aggregate(all.burrow.deep[,2:4], by=list(format(all.burrow.deep$date_time,"%d-%m-%Y"),all.burrow.deep$site), FUN = max)





############################################thermal stress#############################################################


#thermal stress burrow mid_format date_time
all.burrow.midf=read.csv(paste0(dropbox,"csv summaries/all.burrow.mid.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.mid$date_time = as.POSIXct(all.burrow.mid$date_time, format = "%Y-%m-%d %H:%M:%S")

#theraml stress_burrow mid 
warm=1.8 #warming effect
max.mid=aggregate(all.burrow.mid, by=list(format(all.burrow.mid$date_time,"%d-%m-%Y %H")), FUN = max)
max.mid$deg_thresh=max.mid$temperature-39.6+warm # vol max temp
max.mid$deg_thresh[max.mid$deg_thresh<0]<-0
max.mid2=aggregate(max.mid$deg_thresh, by=list(format(max.mid$date_time,"%d-%m-%Y")), FUN = sum)


#theraml stress_burrow surface/entrance 
warm=2.8 #warming effect
max.surf=aggregate(all.burrow.surf, by=list(format(all.burrow.surf$date_time,"%d-%m-%Y %H")), FUN = min)
max.surf$deg_thresh=max.surf$temperature-39.6+warm # vol max temp
max.surf$deg_thresh[max.surf$deg_thresh<0]<-0
max.surf2=aggregate(max.surf$deg_thresh, by=list(format(max.surf$date_time,"%d-%m-%Y")), FUN = sum)

write.csv(max.surf, paste0(dropbox,"/csv summaries/threshold/burrow entrance_threshold_2070.csv"))



#theraml stress_foraging at burrow surface/entrance

#thermal stress burrow entrance/surface_format date_time
all.burrow.surf=read.csv(paste0(dropbox,"csv summaries/all.burrow.surf.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.surf$date_time = as.POSIXct(all.burrow.surf$date_time, format = "%Y-%m-%d %H:%M:%S")


warm=2.8 #warming effect
max.surf=aggregate(all.burrow.surf, by=list(format(all.burrow.surf$date_time,"%d-%m-%Y %H")), FUN = min)
max.surf$deg_thresh=max.surf$temperature-37.6+warm #foraging max temp
max.surf$deg_thresh[max.surf$deg_thresh<0]<-0
max.surf2=aggregate(max.surf$deg_thresh, by=list(format(max.surf$date_time,"%d-%m-%Y")), FUN = sum)

write.csv(max.surf, paste0(dropbox,"/csv summaries/threshold/burrow entrance_threshold_2070f.csv"))
#write.csv(max.surf2, paste0(dropbox,"/csv summaries/threshold/max_surf2_threshold_20702f.csv"))


##theraml stress_foraging fullsun

#thermal stress surface full sun_format date_time
all.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/all.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.fullsuns$date_time = as.POSIXct(all.adult.fullsuns$date_time, format = "%Y-%m-%d %H:%M:%S")

warm=2.8 #warming effect
max.full.sun=aggregate(all.adult.fullsuns, by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y %H")), FUN = min)
max.full.sun$deg_thresh=max.full.sun$temperature-37.6+warm
max.full.sun$deg_thresh[max.full.sun$deg_thresh<0]<-0
max.full.sun2=aggregate(max.full.sun$deg_thresh, by=list(format(max.full.sun$date_time,"%d-%m-%Y")), FUN = sum)

write.csv(max.full.sun, paste0(dropbox,"/csv summaries/threshold/max_fullsun_threshold_2070f.csv"))
#write.csv(max.full.sun2, paste0(dropbox,"/csv summaries/threshold/max_fullsun_threshold2_2016f.csv"))


#thermal stress surface full shade_format date_time
all.adult.fullshades=read.csv(paste0(dropbox,"csv summaries/all.adult.fullshades.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.fullshades$date_time = as.POSIXct(all.adult.fullshades$date_time, format = "%Y-%m-%d %H:%M:%S")

warm=0   #warming effect
max.full.shade=aggregate(all.adult.fullshades, by=list(format(all.adult.fullshades$date_time,"%d-%m-%Y %H")), FUN = min)
max.full.shade$deg_thresh=max.full.shade$temperature-37.6+warm
max.full.shade$deg_thresh[max.full.shade$deg_thresh<0]<-0
max.full.shade2=aggregate(max.full.shade$deg_thresh, by=list(format(max.full.shade$date_time,"%d-%m-%Y")), FUN = sum)

write.csv(max.full.shade, paste0(dropbox,"/csv summaries/threshold/max_fullshade_threshold_2016f.csv"))
#write.csv(max.full.sun2, paste0(dropbox,"/csv summaries/threshold/max_fullshade_threshold2_2050f.csv"))


#thermal stress nocturnal foraging temp fullsun format date_time

all.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/all.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.fullsuns$date_time = as.POSIXct(all.adult.fullsuns$date_time, format = "%Y-%m-%d %H:%M:%S")

warm=2.8 #warming effect
max.full.sun=aggregate(all.adult.fullsuns, by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y %H")), FUN = min)
max.full.sun$deg_thresh=max.full.sun$temperature-37.6+warm
max.full.sun$deg_thresh[max.full.sun$deg_thresh<0]<-0
max.full.sun2=aggregate(max.full.sun$deg_thresh, by=list(format(max.full.sun$date_time,"%d-%m-%Y")), FUN = sum)

write.csv(max.full.sun, paste0(dropbox,"/csv summaries/threshold/max_fullsun_threshold_2070f_night2.csv"))
#write.csv(max.full.sun2, paste0(dropbox,"/csv summaries/threshold/max_fullsun_threshold2_2070f_night.csv"))


#potential activity #
all.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/all.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.fullsuns$date_time = as.POSIXct(all.adult.fullsuns$date_time, format = "%Y-%m-%d %H:%M:%S")

warm=2.8 #warming effect
max.full.sun=aggregate(all.adult.fullsuns, by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y %H")), FUN = min)
max.full.sun$deg_thresh=max.full.sun$temperature-37.6+warm
max.full.sun$deg_thresh[max.full.sun$deg_thresh<0]<-0
max.full.sun2=aggregate(max.full.sun$deg_thresh, by=list(format(max.full.sun$date_time,"%d-%m-%Y")), FUN = sum)

write.csv(max.full.sun, paste0(dropbox,"/csv summaries/threshold/max_fullsun_threshold_2070f_night2.csv"))
#write.csv(max.full.sun2, paste0(dropbox,"/csv summaries/threshold/max_fullsun_threshold2_2070f_night.csv"))





######### get mean max over site########
# note only columns 3-5 to aggregate- i.e. the temperature, humidity and vapour pressure
mean.max.surf=aggregate(max.surf.site[,3:5], by=list(max.surf.site$Group.1), FUN = mean)
mean.max.mid=aggregate(max.mid.site[,3:5], by=list(max.mid.site$Group.1), FUN = mean)
mean.max.deep=aggregate(max.deep.site[,3:5], by=list(max.deep.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.max.surf=aggregate(max.surf.site[,3:5], by=list(max.surf.site$Group.1), FUN = st.err)
se.max.mid=aggregate(max.mid.site[,3:5], by=list(max.mid.site$Group.1), FUN = st.err)
se.max.deep=aggregate(max.deep.site[,3:5], by=list(max.deep.site$Group.1), FUN = st.err)
sd.max.surf=aggregate(max.surf.site[,3:5], by=list(max.surf.site$Group.1), FUN = sd)
sd.max.mid=aggregate(max.mid.site[,3:5], by=list(max.mid.site$Group.1), FUN = sd)
sd.max.deep=aggregate(max.deep.site[,3:5], by=list(max.deep.site$Group.1), FUN = sd)
# add standard error columns to the mean.max tables so they are all together
mean.max.surf=cbind(mean.max.surf, se.max.surf[,2:4], sd.max.surf[,2:4])
mean.max.mid=cbind(mean.max.mid, se.max.mid[,2:4], sd.max.mid[,2:4])
mean.max.deep=cbind(mean.max.deep, se.max.deep[,2:4], sd.max.deep[,2:4])

# label columns
names<-c("date","mean.max.temperature","mean.max.humidity","mean.max.vd","se.max.temperature","se.max.humidity","se.max.vd","sd.max.temperature","sd.max.humidity","sd.max.vd")
colnames(mean.max.surf)<-names
colnames(mean.max.mid)<-names
colnames(mean.max.deep)<-names
# format dates as proper date format
mean.max.surf$date<-as.POSIXct(mean.max.surf$date,format="%d-%m-%Y")
mean.max.mid$date<-as.POSIXct(mean.max.mid$date,format="%d-%m-%Y")
mean.max.deep$date<-as.POSIXct(mean.max.deep$date,format="%d-%m-%Y")
# put in date order to make plots look nice
mean.max.surf<-mean.max.surf[order(mean.max.surf$date),] 
mean.max.mid<-mean.max.mid[order(mean.max.mid$date),] 
mean.max.deep<-mean.max.deep[order(mean.max.deep$date),] 

###################################### now aggregate for minimum values #############################################################

# aggregate to daily min
# aggregate to daily min over all sites
min.surf=aggregate(all.burrow.surf, by=list(format(all.burrow.surf$date_time,"%d-%m-%Y")), FUN = min)
min.mid=aggregate(all.burrow.mid, by=list(format(all.burrow.mid$date_time,"%d-%m-%Y")), FUN = min)
min.deep=aggregate(all.burrow.deep, by=list(format(all.burrow.deep$date_time,"%d-%m-%Y")), FUN = min)
min.surf<-min.surf[order(min.surf$date_time),] 
min.mid<-min.mid[order(min.mid$date_time),] 
min.deep<-min.deep[order(min.deep$date_time),]

# aggregate to daily min per site
min.surf.site=aggregate(all.burrow.surf[,2:4], by=list(format(all.burrow.surf$date_time,"%d-%m-%Y"),all.burrow.surf$site), FUN = min)
min.mid.site=aggregate(all.burrow.mid[,2:4], by=list(format(all.burrow.mid$date_time,"%d-%m-%Y"),all.burrow.mid$site), FUN = min)
min.deep.site=aggregate(all.burrow.deep[,2:4], by=list(format(all.burrow.deep$date_time,"%d-%m-%Y"),all.burrow.deep$site), FUN = min)

# get mean min over site
mean.min.surf=aggregate(min.surf.site[,3:5], by=list(min.surf.site$Group.1), FUN = mean)
mean.min.mid=aggregate(min.mid.site[,3:5], by=list(min.mid.site$Group.1), FUN = mean)
mean.min.deep=aggregate(min.deep.site[,3:5], by=list(min.deep.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.min.surf=aggregate(min.surf.site[,3:5], by=list(min.surf.site$Group.1), FUN = st.err)
se.min.mid=aggregate(min.mid.site[,3:5], by=list(min.mid.site$Group.1), FUN = st.err)
se.min.deep=aggregate(min.deep.site[,3:5], by=list(min.deep.site$Group.1), FUN = st.err)
sd.min.surf=aggregate(min.surf.site[,3:5], by=list(min.surf.site$Group.1), FUN = sd)
sd.min.mid=aggregate(min.mid.site[,3:5], by=list(min.mid.site$Group.1), FUN = sd)
sd.min.deep=aggregate(min.deep.site[,3:5], by=list(min.deep.site$Group.1), FUN = sd)
mean.min.surf=cbind(mean.min.surf, se.min.surf[,2:4], sd.min.surf[,2:4])
mean.min.mid=cbind(mean.min.mid, se.min.mid[,2:4], sd.min.mid[,2:4])
mean.min.deep=cbind(mean.min.deep, se.min.deep[,2:4], sd.min.deep[,2:4])

names<-c("date","mean.min.temperature","mean.min.humidity","mean.min.vd","se.min.temperature","se.min.humidity","se.min.vd","sd.min.temperature","sd.min.humidity","sd.min.vd")
colnames(mean.min.surf)<-names
colnames(mean.min.mid)<-names
colnames(mean.min.deep)<-names

mean.min.surf$date<-as.POSIXct(mean.min.surf$date,format="%d-%m-%Y")
mean.min.mid$date<-as.POSIXct(mean.min.mid$date,format="%d-%m-%Y")
mean.min.deep$date<-as.POSIXct(mean.min.deep$date,format="%d-%m-%Y")

mean.min.surf<-mean.min.surf[order(mean.min.surf$date),] 
mean.min.mid<-mean.min.mid[order(mean.min.mid$date),] 
mean.min.deep<-mean.min.deep[order(mean.min.deep$date),] 

#################################### now aggregate for mean values ###############################################################

# aggregate to daily mean
# aggregate to daily mean over all sites
mean.surf=aggregate(all.burrow.surf, by=list(format(all.burrow.surf$date_time,"%d-%m-%Y")), FUN = mean)
mean.mid=aggregate(all.burrow.mid, by=list(format(all.burrow.mid$date_time,"%d-%m-%Y")), FUN = mean)
mean.deep=aggregate(all.burrow.deep, by=list(format(all.burrow.deep$date_time,"%d-%m-%Y")), FUN = mean)
mean.surf<-mean.surf[order(mean.surf$date_time),] 
mean.mid<-mean.mid[order(mean.mid$date_time),] 
mean.deep<-mean.deep[order(mean.deep$date_time),]

# aggregate to daily mean per site
mean.surf.site=aggregate(all.burrow.surf[,2:4], by=list(format(all.burrow.surf$date_time,"%d-%m-%Y"),all.burrow.surf$site), FUN = mean)
mean.mid.site=aggregate(all.burrow.mid[,2:4], by=list(format(all.burrow.mid$date_time,"%d-%m-%Y"),all.burrow.mid$site), FUN = mean)
mean.deep.site=aggregate(all.burrow.deep[,2:4], by=list(format(all.burrow.deep$date_time,"%d-%m-%Y"),all.burrow.deep$site), FUN = mean)

# get mean mean over site
mean.mean.surf=aggregate(mean.surf.site[,3:5], by=list(mean.surf.site$Group.1), FUN = mean)
mean.mean.mid=aggregate(mean.mid.site[,3:5], by=list(mean.mid.site$Group.1), FUN = mean)
mean.mean.deep=aggregate(mean.deep.site[,3:5], by=list(mean.deep.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.mean.surf=aggregate(mean.surf.site[,3:5], by=list(mean.surf.site$Group.1), FUN = st.err)
se.mean.mid=aggregate(mean.mid.site[,3:5], by=list(mean.mid.site$Group.1), FUN = st.err)
se.mean.deep=aggregate(mean.deep.site[,3:5], by=list(mean.deep.site$Group.1), FUN = st.err)
sd.mean.surf=aggregate(mean.surf.site[,3:5], by=list(mean.surf.site$Group.1), FUN = sd)
sd.mean.mid=aggregate(mean.mid.site[,3:5], by=list(mean.mid.site$Group.1), FUN = sd)
sd.mean.deep=aggregate(mean.deep.site[,3:5], by=list(mean.deep.site$Group.1), FUN = sd)
mean.mean.surf=cbind(mean.mean.surf, se.mean.surf[,2:4], sd.mean.surf[,2:4])
mean.mean.mid=cbind(mean.mean.mid, se.mean.mid[,2:4], sd.mean.mid[,2:4])
mean.mean.deep=cbind(mean.mean.deep, se.mean.deep[,2:4], sd.mean.deep[,2:4])

names<-c("date","mean.mean.temperature","mean.mean.humidity","mean.mean.vd","se.mean.temperature","se.mean.humidity","se.mean.vd","sd.mean.temperature","sd.mean.humidity","sd.mean.vd")
colnames(mean.mean.surf)<-names
colnames(mean.mean.mid)<-names
colnames(mean.mean.deep)<-names

mean.mean.surf$date<-as.POSIXct(mean.mean.surf$date,format="%d-%m-%Y")
mean.mean.mid$date<-as.POSIXct(mean.mean.mid$date,format="%d-%m-%Y")
mean.mean.deep$date<-as.POSIXct(mean.mean.deep$date,format="%d-%m-%Y")

mean.mean.surf<-mean.mean.surf[order(mean.mean.surf$date),] 
mean.mean.mid<-mean.mean.mid[order(mean.mean.mid$date),] 
mean.mean.deep<-mean.mean.deep[order(mean.mean.deep$date),] 

################################### copper models #####################################################################

adult.folder<-paste0(dropbox,"raw data/adult models/")
adult.folders<-list.dirs(adult.folder)[-1]

sites<-c("C1","T1","C2","T2","C3","T3")
for(m in 1:length(sites)){
################################### adult copper model full sun #####################################################################

adult.folders.fullsun<-adult.folders[grep(adult.folders,pattern = "full sun")]
if(length(adult.folders.fullsun)>1){
  adult.folders.fullsun<-adult.folders.fullsun[1]
}
adult.files.fullsun<-list.files(adult.folders.fullsun)
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = ".txt")]
adult.files.fullsun<-adult.files.fullsun[grep(adult.files.fullsun,pattern = sites[m])]

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
   adult.fullsuns<-cbind(adult.fullsun,sites[m],stringsAsFactors=FALSE)
  }else{
   adult.fullsuns<-rbind(adult.fullsuns,cbind(adult.fullsun,sites[m],stringsAsFactors=FALSE))
  }
 }
adult.fullsun<-adult.fullsun[order(adult.fullsun$date_time),] 

  if(m==1){ # first site
    all.adult.fullsuns=adult.fullsuns
  }else{ # subsequent site - row-bind the last site's results to this set
    all.adult.fullsuns=rbind(all.adult.fullsuns,adult.fullsuns)
  }

################################### adult copper model part shade #####################################################################

adult.folders.partshade<-adult.folders[grep(adult.folders,pattern = "part shade")]
if(length(adult.folders.partshade)>1){
  adult.folders.partshade<-adult.folders.partshade[1]
}
adult.files.partshade<-list.files(adult.folders.partshade)
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = ".txt")]
adult.files.partshade<-adult.files.partshade[grep(adult.files.partshade,pattern = sites[m])]

# read and plot all data

for(i in 1:length(adult.files.partshade)){
  adult.partshade<-read.csv(paste(adult.folders.partshade,"/",adult.files.partshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(adult.partshade)<-c('date_time','temperature') #give the columns names
  adult.title<-adult.files.partshade[i]
  adult.title<-str_replace_all(adult.title,'.txt','') #get rid of ".txt"

  adult.partshade$temperature<-type.convert(sub("\\p{So}C", "", adult.partshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  adult.partshade$date_time<-as.POSIXct(adult.partshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   adult.partshades<-cbind(adult.partshade,sites[m],stringsAsFactors=FALSE)
  }else{
   adult.partshades<-rbind(adult.partshades,cbind(adult.partshade,sites[m],stringsAsFactors=FALSE))
  }
 }
adult.partshade<-adult.partshade[order(adult.partshade$date_time),] 

  if(m==1){ # first site
    all.adult.partshades=adult.partshades
  }else{ # subsequent site - row-bind the last site's results to this set
    all.adult.partshades=rbind(all.adult.partshades,adult.partshades)
  }

################################### adult copper model full shade #####################################################################

adult.folders.fullshade<-adult.folders[grep(adult.folders,pattern = "full shade")]
adult.files.fullshade<-list.files(adult.folders.fullshade)
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = ".txt")]
adult.files.fullshade<-adult.files.fullshade[grep(adult.files.fullshade,pattern = sites[m])]

# read and plot all data

for(i in 1:length(adult.files.fullshade)){
  adult.fullshade<-read.csv(paste(adult.folders.fullshade,"/",adult.files.fullshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(adult.fullshade)<-c('date_time','temperature') #give the columns names
  adult.title<-adult.files.fullshade[i]
  adult.title<-str_replace_all(adult.title,'.txt','') #get rid of ".txt"

  adult.fullshade$temperature<-type.convert(sub("\\p{So}C", "", adult.fullshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  adult.fullshade$date_time<-as.POSIXct(adult.fullshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   adult.fullshades<-cbind(adult.fullshade,sites[m],stringsAsFactors=FALSE)
  }else{
   adult.fullshades<-rbind(adult.fullshades,cbind(adult.fullshade,sites[m],stringsAsFactors=FALSE))
  }
 }
adult.fullshade<-adult.fullshade[order(adult.fullshade$date_time),] 

  if(m==1){ # first site
    all.adult.fullshades=adult.fullshades
  }else{ # subsequent site - row-bind the last site's results to this set
    all.adult.fullshades=rbind(all.adult.fullshades,adult.fullshades)
  }

################################### juvenile copper model full shade #####################################################################

juvenile.folder<-paste0(dropbox,"raw data/Juvenile models/")
juvenile.folders<-list.dirs(juvenile.folder)[-1]
juvenile.folders.fullshade<-juvenile.folders[grep(juvenile.folders,pattern = "full shade")]
juvenile.files.fullshade<-list.files(juvenile.folders.fullshade)
juvenile.files.fullshade<-juvenile.files.fullshade[grep(juvenile.files.fullshade,pattern = ".txt")]
juvenile.files.fullshade<-juvenile.files.fullshade[grep(juvenile.files.fullshade,pattern = sites[m])]

# read and plot all data

for(i in 1:length(juvenile.files.fullshade)){
  juv.fullshade<-read.csv(paste(juvenile.folders.fullshade,"/",juvenile.files.fullshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(juv.fullshade)<-c('date_time','temperature') #give the columns names
  juv.title<-juvenile.files.fullshade[i]
  juv.title<-str_replace_all(juv.title,'.txt','') #get rid of ".txt"

  juv.fullshade$temperature<-type.convert(sub("\\p{So}C", "", juv.fullshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  juv.fullshade$date_time<-as.POSIXct(juv.fullshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   juv.fullshades<-cbind(juv.fullshade,sites[m],stringsAsFactors=FALSE)
  }else{
   juv.fullshades<-rbind(juv.fullshades,cbind(juv.fullshade,sites[m],stringsAsFactors=FALSE))
  }
 }
juv.fullshade<-juv.fullshade[order(juv.fullshade$date_time),] 

  if(m==1){ # first site
    all.juv.fullshades=juv.fullshades
  }else{ # subsequent site - row-bind the last site's results to this set
    all.juv.fullshades=rbind(all.juv.fullshades,juv.fullshades)
  }

################################### juvenile copper model full sun #####################################################################

juvenile.folders.fullsun<-juvenile.folders[grep(juvenile.folders,pattern = "full sun")]
if(length(juvenile.folders.fullsun)>1){
  juvenile.folders.fullsun<-juvenile.folders.fullsun[1]
}
juvenile.files.fullsun<-list.files(juvenile.folders.fullsun)
juvenile.files.fullsun<-juvenile.files.fullsun[grep(juvenile.files.fullsun,pattern = ".txt")]
juvenile.files.fullsun<-juvenile.files.fullsun[grep(juvenile.files.fullsun,pattern = sites[m])]

# read and plot all data

for(i in 1:length(juvenile.files.fullsun)){
  juv.fullsun<-read.csv(paste(juvenile.folders.fullsun,"/",juvenile.files.fullsun[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(juv.fullsun)<-c('date_time','temperature') #give the columns names
  juv.title<-juvenile.files.fullsun[i]
  juv.title<-str_replace_all(juv.title,'.txt','') #get rid of ".txt"

  juv.fullsun$temperature<-type.convert(sub("\\p{So}C", "", juv.fullsun$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  juv.fullsun$date_time<-as.POSIXct(juv.fullsun$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   juv.fullsuns<-cbind(juv.fullsun,sites[m],stringsAsFactors=FALSE)
  }else{
   juv.fullsuns<-rbind(juv.fullsuns,cbind(juv.fullsun,sites[m],stringsAsFactors=FALSE))
  }
 }
juv.fullsun<-juv.fullsun[order(juv.fullsun$date_time),] 

  if(m==1){ # first site
    all.juv.fullsuns=juv.fullsuns
  }else{ # subsequent site - row-bind the last site's results to this set
    all.juv.fullsuns=rbind(all.juv.fullsuns,juv.fullsuns)
  }

################################### juvenile copper model part shade #####################################################################

juvenile.folders.partshade<-juvenile.folders[grep(juvenile.folders,pattern = "part shade")]
if(length(juvenile.folders.partshade)>1){
  juvenile.folders.partshade<-juvenile.folders.partshade[1]
}
juvenile.files.partshade<-list.files(juvenile.folders.partshade)
juvenile.files.partshade<-juvenile.files.partshade[grep(juvenile.files.partshade,pattern = ".txt")]
juvenile.files.partshade<-juvenile.files.partshade[grep(juvenile.files.partshade,pattern = sites[m])]

# read and plot all data

for(i in 1:length(juvenile.files.partshade)){
  juv.partshade<-read.csv(paste(juvenile.folders.partshade,"/",juvenile.files.partshade[i],sep=""),head=FALSE,skip=2,stringsAsFactors=FALSE) #read the file, skip the first two lines and specify that there isn't a header
  colnames(juv.partshade)<-c('date_time','temperature') #give the columns names
  juv.title<-juvenile.files.partshade[i]
  juv.title<-str_replace_all(juv.title,'.txt','') #get rid of ".txt"

  juv.partshade$temperature<-type.convert(sub("\\p{So}C", "", juv.partshade$temperature, perl = TRUE)) # trick to get rid of degree C symbol
  juv.partshade$date_time<-as.POSIXct(juv.partshade$date_time,tz="Etc/GMT-10",format="%d/%m/%Y %H:%M:%S") # format date colum
  if(i==1){
   juv.partshades<-cbind(juv.partshade,sites[m],stringsAsFactors=FALSE)
  }else{
   juv.partshades<-rbind(juv.partshades,cbind(juv.partshade,sites[m],stringsAsFactors=FALSE))
  }
 }
juv.partshade<-juv.partshade[order(juv.partshade$date_time),]

  if(m==1){ # first site
    all.juv.partshades=juv.partshades
  }else{ # subsequent site - row-bind the last site's results to this set
    all.juv.partshades=rbind(all.juv.partshades,juv.partshades)
  }

} # end loop through all sites


# add column names
colnames(all.adult.fullsuns)<-c("date_time","temperature","site")
colnames(all.adult.fullshades)<-c("date_time","temperature","site")
colnames(all.adult.partshades)<-c("date_time","temperature","site")
colnames(all.juv.fullsuns)<-c("date_time","temperature","site")
colnames(all.juv.fullshades)<-c("date_time","temperature","site")
colnames(all.juv.partshades)<-c("date_time","temperature","site")

all.adult.fullsuns<-all.adult.fullsuns[order(all.adult.fullsuns$date_time),] 
all.adult.fullshades<-all.adult.fullshades[order(all.adult.fullshades$date_time),] 
all.adult.partshades<-all.adult.partshades[order(all.adult.partshades$date_time),] 
all.juv.fullsuns<-all.juv.fullsuns[order(all.juv.fullsuns$date_time),] 
all.juv.fullshades<-all.juv.fullshades[order(all.juv.fullshades$date_time),] 
all.juv.partshades<-all.juv.partshades[order(all.juv.partshades$date_time),] 

# remove data from days where loggers were downloaded
copper.remove.C1<-c("01/09/2013", "11/11/2013", "28/01/2014", "22/04/2014", "10/07/2014", "3/10/2014")
copper.remove.C2<-c("01/09/2013", "11/11/2013", "28/01/2014", "22/04/2014", "10/07/2014", "3/10/2014", "12/11/2013", "25/01/2014", "20/01/2014")
copper.remove.C3<-c("01/09/2013", "11/11/2013", "28/01/2014", "22/04/2014", "10/07/2014", "3/10/2014", "11/11/2013", "25/01/2014", "20/04/2014")
copper.remove.T1<-c("01/09/2013", "11/11/2013", "08/01/2014", "21/04/2014", "08/07/2014","01/10/2014", "12/11/2013", "28/01/2014")
copper.remove.T2<-c("01/09/2013", "11/11/2013", "08/01/2014", "21/04/2014", "08/07/2014","01/10/2014", "28/01/2014")
copper.remove.T3<-c("01/09/2013", "11/11/2013", "08/01/2014", "21/04/2014", "08/07/2014","01/10/2014", "12/11/2013", "28/01/2014")
copper.remove.C3.fullsun<-c("02/09/2013", "25/10/2013")

all.adult.fullsuns<-subset(all.adult.fullsuns, !(as.character(all.adult.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C1 & all.adult.fullsuns$site=="C1"))
all.adult.fullsuns<-subset(all.adult.fullsuns, !(as.character(all.adult.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C2 & all.adult.fullsuns$site=="C2"))
all.adult.fullsuns<-subset(all.adult.fullsuns, !(as.character(all.adult.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C3 & all.adult.fullsuns$site=="C3"))
all.adult.fullsuns<-subset(all.adult.fullsuns, !(as.character(all.adult.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.T1 & all.adult.fullsuns$site=="T1"))
all.adult.fullsuns<-subset(all.adult.fullsuns, !(as.character(all.adult.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.T2 & all.adult.fullsuns$site=="T2"))
all.adult.fullsuns<-subset(all.adult.fullsuns, !(as.character(all.adult.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.T3 & all.adult.fullsuns$site=="T3"))
all.adult.fullsuns<-subset(all.adult.fullsuns, !(as.character(all.adult.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C3.fullsun & all.adult.fullsuns$site=="C3"))

all.adult.fullshades<-subset(all.adult.fullshades, !(as.character(all.adult.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.C1 & all.adult.fullshades$site=="C1"))
all.adult.fullshades<-subset(all.adult.fullshades, !(as.character(all.adult.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.C2 & all.adult.fullshades$site=="C2"))
all.adult.fullshades<-subset(all.adult.fullshades, !(as.character(all.adult.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.C3 & all.adult.fullshades$site=="C3"))
all.adult.fullshades<-subset(all.adult.fullshades, !(as.character(all.adult.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.T1 & all.adult.fullshades$site=="T1"))
all.adult.fullshades<-subset(all.adult.fullshades, !(as.character(all.adult.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.T2 & all.adult.fullshades$site=="T2"))
all.adult.fullshades<-subset(all.adult.fullshades, !(as.character(all.adult.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.T3 & all.adult.fullshades$site=="T3"))

all.adult.partshades<-subset(all.adult.partshades, !(as.character(all.adult.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.C1 & all.adult.partshades$site=="C1"))
all.adult.partshades<-subset(all.adult.partshades, !(as.character(all.adult.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.C2 & all.adult.partshades$site=="C2"))
all.adult.partshades<-subset(all.adult.partshades, !(as.character(all.adult.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.C3 & all.adult.partshades$site=="C3"))
all.adult.partshades<-subset(all.adult.partshades, !(as.character(all.adult.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.T1 & all.adult.partshades$site=="T1"))
all.adult.partshades<-subset(all.adult.partshades, !(as.character(all.adult.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.T2 & all.adult.partshades$site=="T2"))
all.adult.partshades<-subset(all.adult.partshades, !(as.character(all.adult.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.T3 & all.adult.partshades$site=="T3"))

all.juv.fullsuns<-subset(all.juv.fullsuns, !(as.character(all.juv.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C1 & all.juv.fullsuns$site=="C1"))
all.juv.fullsuns<-subset(all.juv.fullsuns, !(as.character(all.juv.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C2 & all.juv.fullsuns$site=="C2"))
all.juv.fullsuns<-subset(all.juv.fullsuns, !(as.character(all.juv.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C3 & all.juv.fullsuns$site=="C3"))
all.juv.fullsuns<-subset(all.juv.fullsuns, !(as.character(all.juv.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.T1 & all.juv.fullsuns$site=="T1"))
all.juv.fullsuns<-subset(all.juv.fullsuns, !(as.character(all.juv.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.T2 & all.juv.fullsuns$site=="T2"))
all.juv.fullsuns<-subset(all.juv.fullsuns, !(as.character(all.juv.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.T3 & all.juv.fullsuns$site=="T3"))
all.juv.fullsuns<-subset(all.juv.fullsuns, !(as.character(all.juv.fullsuns$date_time, format="%d/%m/%Y")%in% copper.remove.C3.fullsun & all.juv.fullsuns$site=="C3"))

all.juv.fullshades<-subset(all.juv.fullshades, !(as.character(all.juv.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.C1 & all.juv.fullshades$site=="C1"))
all.juv.fullshades<-subset(all.juv.fullshades, !(as.character(all.juv.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.C2 & all.juv.fullshades$site=="C2"))
all.juv.fullshades<-subset(all.juv.fullshades, !(as.character(all.juv.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.C3 & all.juv.fullshades$site=="C3"))
all.juv.fullshades<-subset(all.juv.fullshades, !(as.character(all.juv.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.T1 & all.juv.fullshades$site=="T1"))
all.juv.fullshades<-subset(all.juv.fullshades, !(as.character(all.juv.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.T2 & all.juv.fullshades$site=="T2"))
all.juv.fullshades<-subset(all.juv.fullshades, !(as.character(all.juv.fullshades$date_time, format="%d/%m/%Y")%in% copper.remove.T3 & all.juv.fullshades$site=="T3"))

all.juv.partshades<-subset(all.juv.partshades, !(as.character(all.juv.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.C1 & all.juv.partshades$site=="C1"))
all.juv.partshades<-subset(all.juv.partshades, !(as.character(all.juv.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.C2 & all.juv.partshades$site=="C2"))
all.juv.partshades<-subset(all.juv.partshades, !(as.character(all.juv.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.C3 & all.juv.partshades$site=="C3"))
all.juv.partshades<-subset(all.juv.partshades, !(as.character(all.juv.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.T1 & all.juv.partshades$site=="T1"))
all.juv.partshades<-subset(all.juv.partshades, !(as.character(all.juv.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.T2 & all.juv.partshades$site=="T2"))
all.juv.partshades<-subset(all.juv.partshades, !(as.character(all.juv.partshades$date_time, format="%d/%m/%Y")%in% copper.remove.T3 & all.juv.partshades$site=="T3"))

################################# aggregation of data per day and per site ################################################################

# aggregate to daily max over all sites ('by' argument has just date specified)
# (this aggregate command gets the max per group, the group being specified in the 'by' argument
# which, in the first case, is the 'date_time' column of the 'all.burrow.surf' table
# formatted to just have the date and not the time, and so forth through the mid and deep)
max.adult.fullsuns=aggregate(all.adult.fullsuns, by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y")), FUN = max) 
max.adult.fullshades=aggregate(all.adult.fullshades, by=list(format(all.adult.fullshades$date_time,"%d-%m-%Y")), FUN = max) 
max.adult.partshades=aggregate(all.adult.partshades, by=list(format(all.adult.partshades$date_time,"%d-%m-%Y")), FUN = max) 
max.juv.fullsuns=aggregate(all.juv.fullsuns, by=list(format(all.juv.fullsuns$date_time,"%d-%m-%Y")), FUN = max) 
max.juv.fullshades=aggregate(all.juv.fullshades, by=list(format(all.juv.fullshades$date_time,"%d-%m-%Y")), FUN = max) 
max.juv.partshades=aggregate(all.juv.partshades, by=list(format(all.juv.partshades$date_time,"%d-%m-%Y")), FUN = max) 

max.adult.fullsuns<-max.adult.fullsuns[order(max.adult.fullsuns$date_time),] # put in date order (otherwise plots look bad)
max.adult.fullshades<-max.adult.fullshades[order(max.adult.fullshades$date_time),] # put in date order (otherwise plots look bad)
max.adult.partshades<-max.adult.partshades[order(max.adult.partshades$date_time),] # put in date order (otherwise plots look bad)
max.juv.fullsuns<-max.juv.fullsuns[order(max.juv.fullsuns$date_time),] # put in date order (otherwise plots look bad)
max.juv.fullshades<-max.juv.fullshades[order(max.juv.fullshades$date_time),] # put in date order (otherwise plots look bad)
max.juv.partshades<-max.juv.partshades[order(max.juv.partshades$date_time),] # put in date order (otherwise plots look bad)

# aggregate to daily max per site ('by' argument has both date and site specified) - note specifing
max.adult.fullsun.site=aggregate(all.adult.fullsuns[,2], by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y"),all.adult.fullsuns$site), FUN = max)
max.adult.fullshade.site=aggregate(all.adult.fullshades[,2], by=list(format(all.adult.fullshades$date_time,"%d-%m-%Y"),all.adult.fullshades$site), FUN = max)
max.adult.partshade.site=aggregate(all.adult.partshades[,2], by=list(format(all.adult.partshades$date_time,"%d-%m-%Y"),all.adult.partshades$site), FUN = max)
max.juv.fullsun.site=aggregate(all.juv.fullsuns[,2], by=list(format(all.juv.fullsuns$date_time,"%d-%m-%Y"),all.juv.fullsuns$site), FUN = max)
max.juv.fullshade.site=aggregate(all.juv.fullshades[,2], by=list(format(all.juv.fullshades$date_time,"%d-%m-%Y"),all.juv.fullshades$site), FUN = max)
max.juv.partshade.site=aggregate(all.juv.partshades[,2], by=list(format(all.juv.partshades$date_time,"%d-%m-%Y"),all.juv.partshades$site), FUN = max)

# get mean max over site
mean.max.adult.fullsun=aggregate(max.adult.fullsun.site[,3], by=list(max.adult.fullsun.site$Group.1), FUN = mean)
mean.max.adult.fullshade=aggregate(max.adult.fullshade.site[,3], by=list(max.adult.fullshade.site$Group.1), FUN = mean)
mean.max.adult.partshade=aggregate(max.adult.partshade.site[,3], by=list(max.adult.partshade.site$Group.1), FUN = mean)
mean.max.juv.fullsun=aggregate(max.juv.fullsun.site[,3], by=list(max.juv.fullsun.site$Group.1), FUN = mean)
mean.max.juv.fullshade=aggregate(max.juv.fullshade.site[,3], by=list(max.juv.fullshade.site$Group.1), FUN = mean)
mean.max.juv.partshade=aggregate(max.juv.partshade.site[,3], by=list(max.juv.partshade.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.max.adult.fullsun=aggregate(max.adult.fullsun.site[,3], by=list(max.adult.fullsun.site$Group.1), FUN = st.err)
se.max.adult.fullshade=aggregate(max.adult.fullshade.site[,3], by=list(max.adult.fullshade.site$Group.1), FUN = st.err)
se.max.adult.partshade=aggregate(max.adult.partshade.site[,3], by=list(max.adult.partshade.site$Group.1), FUN = st.err)
se.max.juv.fullsun=aggregate(max.juv.fullsun.site[,3], by=list(max.juv.fullsun.site$Group.1), FUN = st.err)
se.max.juv.fullshade=aggregate(max.juv.fullshade.site[,3], by=list(max.juv.fullshade.site$Group.1), FUN = st.err)
se.max.juv.partshade=aggregate(max.juv.partshade.site[,3], by=list(max.juv.partshade.site$Group.1), FUN = st.err)

sd.max.adult.fullsun=aggregate(max.adult.fullsun.site[,3], by=list(max.adult.fullsun.site$Group.1), FUN = sd)
sd.max.adult.fullshade=aggregate(max.adult.fullshade.site[,3], by=list(max.adult.fullshade.site$Group.1), FUN = sd)
sd.max.adult.partshade=aggregate(max.adult.partshade.site[,3], by=list(max.adult.partshade.site$Group.1), FUN = sd)
sd.max.juv.fullsun=aggregate(max.juv.fullsun.site[,3], by=list(max.juv.fullsun.site$Group.1), FUN = st.err)
sd.max.juv.fullshade=aggregate(max.juv.fullshade.site[,3], by=list(max.juv.fullshade.site$Group.1), FUN = sd)
sd.max.juv.partshade=aggregate(max.juv.partshade.site[,3], by=list(max.juv.partshade.site$Group.1), FUN = sd)

# add standard error columns to the mean.max tables so they are all together
mean.max.adult.fullsun=cbind(mean.max.adult.fullsun, se.max.adult.fullsun[,2], sd.max.adult.fullsun[,2])
mean.max.adult.fullshade=cbind(mean.max.adult.fullshade, se.max.adult.fullshade[,2], sd.max.adult.fullshade[,2])
mean.max.adult.partshade=cbind(mean.max.adult.partshade, se.max.adult.partshade[,2], sd.max.adult.partshade[,2])
mean.max.juv.fullsun=cbind(mean.max.juv.fullsun, se.max.juv.fullsun[,2], sd.max.juv.fullsun[,2])
mean.max.juv.fullshade=cbind(mean.max.juv.fullshade, se.max.juv.fullshade[,2], sd.max.juv.fullshade[,2])
mean.max.juv.partshade=cbind(mean.max.juv.partshade, se.max.juv.partshade[,2], sd.max.juv.partshade[,2])
# label columns
names<-c("date","mean.max.temperature","se.max.temperature","sd.max.temperature")
colnames(mean.max.adult.fullsun)<-names
colnames(mean.max.adult.fullshade)<-names
colnames(mean.max.adult.partshade)<-names
colnames(mean.max.juv.fullsun)<-names
colnames(mean.max.juv.fullshade)<-names
colnames(mean.max.juv.partshade)<-names

# format dates as proper date format
mean.max.adult.fullsun$date<-as.POSIXct(mean.max.adult.fullsun$date,format="%d-%m-%Y")
mean.max.adult.fullshade$date<-as.POSIXct(mean.max.adult.fullshade$date,format="%d-%m-%Y")
mean.max.adult.partshade$date<-as.POSIXct(mean.max.adult.partshade$date,format="%d-%m-%Y")
mean.max.juv.fullsun$date<-as.POSIXct(mean.max.juv.fullsun$date,format="%d-%m-%Y")
mean.max.juv.fullshade$date<-as.POSIXct(mean.max.juv.fullshade$date,format="%d-%m-%Y")
mean.max.juv.partshade$date<-as.POSIXct(mean.max.juv.partshade$date,format="%d-%m-%Y")

# put in date order to make plots look nice
mean.max.adult.fullsun<-mean.max.adult.fullsun[order(mean.max.adult.fullsun$date),] 
mean.max.adult.fullshade<-mean.max.adult.fullshade[order(mean.max.adult.fullshade$date),] 
mean.max.adult.partshade<-mean.max.adult.partshade[order(mean.max.adult.partshade$date),] 
mean.max.juv.fullsun<-mean.max.juv.fullsun[order(mean.max.juv.fullsun$date),] 
mean.max.juv.fullshade<-mean.max.juv.fullshade[order(mean.max.juv.fullshade$date),] 
mean.max.juv.partshade<-mean.max.juv.partshade[order(mean.max.juv.partshade$date),] 

###################################### now aggregate for minimum values #############################################################

# aggregate to daily min over all sites ('by' argument has just date specified)
# (this aggregate command gets the min per group, the group being specified in the 'by' argument
# which, in the first case, is the 'date_time' column of the 'all.burrow.surf' table
# formatted to just have the date and not the time, and so forth through the mid and deep)
min.adult.fullsuns=aggregate(all.adult.fullsuns, by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y")), FUN = min) 
min.adult.fullshades=aggregate(all.adult.fullshades, by=list(format(all.adult.fullshades$date_time,"%d-%m-%Y")), FUN = min) 
min.adult.partshades=aggregate(all.adult.partshades, by=list(format(all.adult.partshades$date_time,"%d-%m-%Y")), FUN = min) 
min.juv.fullsuns=aggregate(all.juv.fullsuns, by=list(format(all.juv.fullsuns$date_time,"%d-%m-%Y")), FUN = min) 
min.juv.fullshades=aggregate(all.juv.fullshades, by=list(format(all.juv.fullshades$date_time,"%d-%m-%Y")), FUN = min) 
min.juv.partshades=aggregate(all.juv.partshades, by=list(format(all.juv.partshades$date_time,"%d-%m-%Y")), FUN = min) 

min.adult.fullsuns<-min.adult.fullsuns[order(min.adult.fullsuns$date_time),] # put in date order (otherwise plots look bad)
min.adult.fullshades<-min.adult.fullshades[order(min.adult.fullshades$date_time),] # put in date order (otherwise plots look bad)
min.adult.partshades<-min.adult.partshades[order(min.adult.partshades$date_time),] # put in date order (otherwise plots look bad)
min.juv.fullsuns<-min.juv.fullsuns[order(min.juv.fullsuns$date_time),] # put in date order (otherwise plots look bad)
min.juv.fullshades<-min.juv.fullshades[order(min.juv.fullshades$date_time),] # put in date order (otherwise plots look bad)
min.juv.partshades<-min.juv.partshades[order(min.juv.partshades$date_time),] # put in date order (otherwise plots look bad)

# aggregate to daily min per site ('by' argument has both date and site specified) - note specifing
min.adult.fullsun.site=aggregate(all.adult.fullsuns[,2], by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y"),all.adult.fullsuns$site), FUN = min)
min.adult.fullshade.site=aggregate(all.adult.fullshades[,2], by=list(format(all.adult.fullshades$date_time,"%d-%m-%Y"),all.adult.fullshades$site), FUN = min)
min.adult.partshade.site=aggregate(all.adult.partshades[,2], by=list(format(all.adult.partshades$date_time,"%d-%m-%Y"),all.adult.partshades$site), FUN = min)
min.juv.fullsun.site=aggregate(all.juv.fullsuns[,2], by=list(format(all.juv.fullsuns$date_time,"%d-%m-%Y"),all.juv.fullsuns$site), FUN = min)
min.juv.fullshade.site=aggregate(all.juv.fullshades[,2], by=list(format(all.juv.fullshades$date_time,"%d-%m-%Y"),all.juv.fullshades$site), FUN = min)
min.juv.partshade.site=aggregate(all.juv.partshades[,2], by=list(format(all.juv.partshades$date_time,"%d-%m-%Y"),all.juv.partshades$site), FUN = min)

# get mean min over site
mean.min.adult.fullsun=aggregate(min.adult.fullsun.site[,3], by=list(min.adult.fullsun.site$Group.1), FUN = mean)
mean.min.adult.fullshade=aggregate(min.adult.fullshade.site[,3], by=list(min.adult.fullshade.site$Group.1), FUN = mean)
mean.min.adult.partshade=aggregate(min.adult.partshade.site[,3], by=list(min.adult.partshade.site$Group.1), FUN = mean)
mean.min.juv.fullsun=aggregate(min.juv.fullsun.site[,3], by=list(min.juv.fullsun.site$Group.1), FUN = mean)
mean.min.juv.fullshade=aggregate(min.juv.fullshade.site[,3], by=list(min.juv.fullshade.site$Group.1), FUN = mean)
mean.min.juv.partshade=aggregate(min.juv.partshade.site[,3], by=list(min.juv.partshade.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.min.adult.fullsun=aggregate(min.adult.fullsun.site[,3], by=list(min.adult.fullsun.site$Group.1), FUN = st.err)
se.min.adult.fullshade=aggregate(min.adult.fullshade.site[,3], by=list(min.adult.fullshade.site$Group.1), FUN = st.err)
se.min.adult.partshade=aggregate(min.adult.partshade.site[,3], by=list(min.adult.partshade.site$Group.1), FUN = st.err)
se.min.juv.fullsun=aggregate(min.juv.fullsun.site[,3], by=list(min.juv.fullsun.site$Group.1), FUN = st.err)
se.min.juv.fullshade=aggregate(min.juv.fullshade.site[,3], by=list(min.juv.fullshade.site$Group.1), FUN = st.err)
se.min.juv.partshade=aggregate(min.juv.partshade.site[,3], by=list(min.juv.partshade.site$Group.1), FUN = st.err)

sd.min.adult.fullsun=aggregate(min.adult.fullsun.site[,3], by=list(min.adult.fullsun.site$Group.1), FUN = sd)
sd.min.adult.fullshade=aggregate(min.adult.fullshade.site[,3], by=list(min.adult.fullshade.site$Group.1), FUN = sd)
sd.min.adult.partshade=aggregate(min.adult.partshade.site[,3], by=list(min.adult.partshade.site$Group.1), FUN = sd)
sd.min.juv.fullsun=aggregate(min.juv.fullsun.site[,3], by=list(min.juv.fullsun.site$Group.1), FUN = sd)
sd.min.juv.fullshade=aggregate(min.juv.fullshade.site[,3], by=list(min.juv.fullshade.site$Group.1), FUN = sd)
sd.min.juv.partshade=aggregate(min.juv.partshade.site[,3], by=list(min.juv.partshade.site$Group.1), FUN = sd)

# add standard error columns to the mean.min tables so they are all together
mean.min.adult.fullsun=cbind(mean.min.adult.fullsun, se.min.adult.fullsun[,2], sd.min.adult.fullsun[,2])
mean.min.adult.fullshade=cbind(mean.min.adult.fullshade, se.min.adult.fullshade[,2], sd.min.adult.fullshade[,2])
mean.min.adult.partshade=cbind(mean.min.adult.partshade, se.min.adult.partshade[,2], sd.min.adult.partshade[,2])
mean.min.juv.fullsun=cbind(mean.min.juv.fullsun, se.min.juv.fullsun[,2], sd.min.juv.fullsun[,2])
mean.min.juv.fullshade=cbind(mean.min.juv.fullshade, se.min.juv.fullshade[,2], sd.min.juv.fullshade[,2])
mean.min.juv.partshade=cbind(mean.min.juv.partshade, se.min.juv.partshade[,2], sd.min.juv.partshade[,2])
# label columns
names<-c("date","mean.min.temperature","se.min.temperature","sd.min.temperature")
colnames(mean.min.adult.fullsun)<-names
colnames(mean.min.adult.fullshade)<-names
colnames(mean.min.adult.partshade)<-names
colnames(mean.min.juv.fullsun)<-names
colnames(mean.min.juv.fullshade)<-names
colnames(mean.min.juv.partshade)<-names

# format dates as proper date format
mean.min.adult.fullsun$date<-as.POSIXct(mean.min.adult.fullsun$date,format="%d-%m-%Y")
mean.min.adult.fullshade$date<-as.POSIXct(mean.min.adult.fullshade$date,format="%d-%m-%Y")
mean.min.adult.partshade$date<-as.POSIXct(mean.min.adult.partshade$date,format="%d-%m-%Y")
mean.min.juv.fullsun$date<-as.POSIXct(mean.min.juv.fullsun$date,format="%d-%m-%Y")
mean.min.juv.fullshade$date<-as.POSIXct(mean.min.juv.fullshade$date,format="%d-%m-%Y")
mean.min.juv.partshade$date<-as.POSIXct(mean.min.juv.partshade$date,format="%d-%m-%Y")

# put in date order to make plots look nice
mean.min.adult.fullsun<-mean.min.adult.fullsun[order(mean.min.adult.fullsun$date),] 
mean.min.adult.fullshade<-mean.min.adult.fullshade[order(mean.min.adult.fullshade$date),] 
mean.min.adult.partshade<-mean.min.adult.partshade[order(mean.min.adult.partshade$date),] 
mean.min.juv.fullsun<-mean.min.juv.fullsun[order(mean.min.juv.fullsun$date),] 
mean.min.juv.fullshade<-mean.min.juv.fullshade[order(mean.min.juv.fullshade$date),] 
mean.min.juv.partshade<-mean.min.juv.partshade[order(mean.min.juv.partshade$date),] 

#################################### now aggregate for mean values ###############################################################

# aggregate to daily mean over all sites ('by' argument has just date specified)
# (this aggregate command gets the mean per group, the group being specified in the 'by' argument
# which, in the first case, is the 'date_time' column of the 'all.burrow.surf' table
# formatted to just have the date and not the time, and so forth through the mid and deep)
mean.adult.fullsuns=aggregate(all.adult.fullsuns[,2], by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y")), FUN = mean) 
mean.adult.fullshades=aggregate(all.adult.fullshades[,2], by=list(format(all.adult.fullshades$date_time,"%d-%m-%Y")), FUN = mean) 
mean.adult.partshades=aggregate(all.adult.partshades[,2], by=list(format(all.adult.partshades$date_time,"%d-%m-%Y")), FUN = mean) 
mean.juv.fullsuns=aggregate(all.juv.fullsuns[,2], by=list(format(all.juv.fullsuns$date_time,"%d-%m-%Y")), FUN = mean) 
mean.juv.fullshades=aggregate(all.juv.fullshades[,2], by=list(format(all.juv.fullshades$date_time,"%d-%m-%Y")), FUN = mean) 
mean.juv.partshades=aggregate(all.juv.partshades[,2], by=list(format(all.juv.partshades$date_time,"%d-%m-%Y")), FUN = mean) 

colnames(mean.adult.fullsuns)<-c("date_time","temperature")
colnames(mean.adult.fullshades)<-c("date_time","temperature")
colnames(mean.adult.partshades)<-c("date_time","temperature")
colnames(mean.juv.fullsuns)<-c("date_time","temperature")
colnames(mean.juv.fullshades)<-c("date_time","temperature")
colnames(mean.juv.partshades)<-c("date_time","temperature")

mean.adult.fullsuns$date_time<-as.POSIXct(mean.adult.fullsuns$date_time,format="%d-%m-%Y")
mean.adult.fullshades$date_time<-as.POSIXct(mean.adult.fullshades$date_time,format="%d-%m-%Y")
mean.adult.partshades$date_time<-as.POSIXct(mean.adult.partshades$date_time,format="%d-%m-%Y")
mean.juv.fullsuns$date_time<-as.POSIXct(mean.juv.fullsuns$date_time,format="%d-%m-%Y")
mean.juv.fullshades$date_time<-as.POSIXct(mean.juv.fullshades$date_time,format="%d-%m-%Y")
mean.juv.partshades$date_time<-as.POSIXct(mean.juv.partshades$date_time,format="%d-%m-%Y")

mean.adult.fullsuns<-mean.adult.fullsuns[order(mean.adult.fullsuns$date_time),] # put in date order (otherwise plots look bad)
mean.adult.fullshades<-mean.adult.fullshades[order(mean.adult.fullshades$date_time),] # put in date order (otherwise plots look bad)
mean.adult.partshades<-mean.adult.partshades[order(mean.adult.partshades$date_time),] # put in date order (otherwise plots look bad)
mean.juv.fullsuns<-mean.juv.fullsuns[order(mean.juv.fullsuns$date_time),] # put in date order (otherwise plots look bad)
mean.juv.fullshades<-mean.juv.fullshades[order(mean.juv.fullshades$date_time),] # put in date order (otherwise plots look bad)
mean.juv.partshades<-mean.juv.partshades[order(mean.juv.partshades$date_time),] # put in date order (otherwise plots look bad)

# aggregate to daily mean per site ('by' argument has both date and site specified) - note specifing
mean.adult.fullsun.site=aggregate(all.adult.fullsuns[,2], by=list(format(all.adult.fullsuns$date_time,"%d-%m-%Y"),all.adult.fullsuns$site), FUN = mean)
mean.adult.fullshade.site=aggregate(all.adult.fullshades[,2], by=list(format(all.adult.fullshades$date_time,"%d-%m-%Y"),all.adult.fullshades$site), FUN = mean)
mean.adult.partshade.site=aggregate(all.adult.partshades[,2], by=list(format(all.adult.partshades$date_time,"%d-%m-%Y"),all.adult.partshades$site), FUN = mean)
mean.juv.fullsun.site=aggregate(all.juv.fullsuns[,2], by=list(format(all.juv.fullsuns$date_time,"%d-%m-%Y"),all.juv.fullsuns$site), FUN = mean)
mean.juv.fullshade.site=aggregate(all.juv.fullshades[,2], by=list(format(all.juv.fullshades$date_time,"%d-%m-%Y"),all.juv.fullshades$site), FUN = mean)
mean.juv.partshade.site=aggregate(all.juv.partshades[,2], by=list(format(all.juv.partshades$date_time,"%d-%m-%Y"),all.juv.partshades$site), FUN = mean)

# get mean mean over site
mean.mean.adult.fullsun=aggregate(mean.adult.fullsun.site[,3], by=list(mean.adult.fullsun.site$Group.1), FUN = mean)
mean.mean.adult.fullshade=aggregate(mean.adult.fullshade.site[,3], by=list(mean.adult.fullshade.site$Group.1), FUN = mean)
mean.mean.adult.partshade=aggregate(mean.adult.partshade.site[,3], by=list(mean.adult.partshade.site$Group.1), FUN = mean)
mean.mean.juv.fullsun=aggregate(mean.juv.fullsun.site[,3], by=list(mean.juv.fullsun.site$Group.1), FUN = mean)
mean.mean.juv.fullshade=aggregate(mean.juv.fullshade.site[,3], by=list(mean.juv.fullshade.site$Group.1), FUN = mean)
mean.mean.juv.partshade=aggregate(mean.juv.partshade.site[,3], by=list(mean.juv.partshade.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.mean.adult.fullsun=aggregate(mean.adult.fullsun.site[,3], by=list(mean.adult.fullsun.site$Group.1), FUN = st.err)
se.mean.adult.fullshade=aggregate(mean.adult.fullshade.site[,3], by=list(mean.adult.fullshade.site$Group.1), FUN = st.err)
se.mean.adult.partshade=aggregate(mean.adult.partshade.site[,3], by=list(mean.adult.partshade.site$Group.1), FUN = st.err)
se.mean.juv.fullsun=aggregate(mean.juv.fullsun.site[,3], by=list(mean.juv.fullsun.site$Group.1), FUN = st.err)
se.mean.juv.fullshade=aggregate(mean.juv.fullshade.site[,3], by=list(mean.juv.fullshade.site$Group.1), FUN = st.err)
se.mean.juv.partshade=aggregate(mean.juv.partshade.site[,3], by=list(mean.juv.partshade.site$Group.1), FUN = st.err)

sd.mean.adult.fullsun=aggregate(mean.adult.fullsun.site[,3], by=list(mean.adult.fullsun.site$Group.1), FUN = sd)
sd.mean.adult.fullshade=aggregate(mean.adult.fullshade.site[,3], by=list(mean.adult.fullshade.site$Group.1), FUN = sd)
sd.mean.adult.partshade=aggregate(mean.adult.partshade.site[,3], by=list(mean.adult.partshade.site$Group.1), FUN = sd)
sd.mean.juv.fullsun=aggregate(mean.juv.fullsun.site[,3], by=list(mean.juv.fullsun.site$Group.1), FUN = sd)
sd.mean.juv.fullshade=aggregate(mean.juv.fullshade.site[,3], by=list(mean.juv.fullshade.site$Group.1), FUN = sd)
sd.mean.juv.partshade=aggregate(mean.juv.partshade.site[,3], by=list(mean.juv.partshade.site$Group.1), FUN = sd)

# add standard error columns to the mean.mean tables so they are all together
mean.mean.adult.fullsun=cbind(mean.mean.adult.fullsun, se.mean.adult.fullsun[,2], sd.mean.adult.fullsun[,2])
mean.mean.adult.fullshade=cbind(mean.mean.adult.fullshade, se.mean.adult.fullshade[,2], sd.mean.adult.fullshade[,2])
mean.mean.adult.partshade=cbind(mean.mean.adult.partshade, se.mean.adult.partshade[,2], sd.mean.adult.partshade[,2])
mean.mean.juv.fullsun=cbind(mean.mean.juv.fullsun, se.mean.juv.fullsun[,2], sd.mean.juv.fullsun[,2])
mean.mean.juv.fullshade=cbind(mean.mean.juv.fullshade, se.mean.juv.fullshade[,2], sd.mean.juv.fullshade[,2])
mean.mean.juv.partshade=cbind(mean.mean.juv.partshade, se.mean.juv.partshade[,2], sd.mean.juv.partshade[,2])
# label columns
names<-c("date","mean.mean.temperature","se.mean.temperature","sd.mean.temperature")
colnames(mean.mean.adult.fullsun)<-names
colnames(mean.mean.adult.fullshade)<-names
colnames(mean.mean.adult.partshade)<-names
colnames(mean.mean.juv.fullsun)<-names
colnames(mean.mean.juv.fullshade)<-names
colnames(mean.mean.juv.partshade)<-names

# format dates as proper date format
mean.mean.adult.fullsun$date<-as.POSIXct(mean.mean.adult.fullsun$date,format="%d-%m-%Y")
mean.mean.adult.fullshade$date<-as.POSIXct(mean.mean.adult.fullshade$date,format="%d-%m-%Y")
mean.mean.adult.partshade$date<-as.POSIXct(mean.mean.adult.partshade$date,format="%d-%m-%Y")
mean.mean.juv.fullsun$date<-as.POSIXct(mean.mean.juv.fullsun$date,format="%d-%m-%Y")
mean.mean.juv.fullshade$date<-as.POSIXct(mean.mean.juv.fullshade$date,format="%d-%m-%Y")
mean.mean.juv.partshade$date<-as.POSIXct(mean.mean.juv.partshade$date,format="%d-%m-%Y")

# put in date order to make plots look nice
mean.mean.adult.fullsun<-mean.mean.adult.fullsun[order(mean.mean.adult.fullsun$date),] 
mean.mean.adult.fullshade<-mean.mean.adult.fullshade[order(mean.mean.adult.fullshade$date),] 
mean.mean.adult.partshade<-mean.mean.adult.partshade[order(mean.mean.adult.partshade$date),] 
mean.mean.juv.fullsun<-mean.mean.juv.fullsun[order(mean.mean.juv.fullsun$date),] 
mean.mean.juv.fullshade<-mean.mean.juv.fullshade[order(mean.mean.juv.fullshade$date),] 
mean.mean.juv.partshade<-mean.mean.juv.partshade[order(mean.mean.juv.partshade$date),] 

################################### soil temperatures #####################################################################

soil.folder<-paste0(dropbox,"raw data/Soil Profiles/")
soil.files<-list.files(soil.folder)
soil.files<-soil.files[grep(soil.files,pattern = ".txt")]
soil.files<-soil.files[-grep(soil.files,pattern = "backup")] # remove backups

sites<-c("C1","T1","C2","T2","C3","T3")
dates<-c("01-Jan-15","09-Dec-14","10-Dec-14","10-Nov-13","15-May-14","21-Apr-14","22-Apr-14","23-Apr-14")
depths<-c("_surface","_5cm","_15cm","_30cm","_50cm","_1m")

# note 11-13th Nov 2013 and 20th-24th April 2014 are data logger download events, remove these from the plots

#par(mfrow = c(3,2)) # set up for 6 plots in 2 columns

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
      soil.surfaces<-cbind(soil.surface,sites[k],stringsAsFactors=FALSE)
    }else{
      soil.surfaces<-rbind(soil.surfaces,cbind(soil.surface,sites[k],stringsAsFactors=FALSE))
    }
  }
  soil.surfaces<-soil.surfaces[order(soil.surfaces$date_time),] 
  # get rid of values in November April when loggers were downloaded
  soil.surfaces<-subset(soil.surfaces,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
  soil.surfaces<-subset(soil.surfaces,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
  if(k==5){
    soil.surfaces<-subset(soil.surfaces,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
  }
  if(k==1){
    all.soil.surfaces=soil.surfaces
  }else{
    all.soil.surfaces=rbind(all.soil.surfaces,soil.surfaces)
  }
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
      soil.5cms<-cbind(soil.5cm,sites[k],stringsAsFactors=FALSE)
    }else{
      soil.5cms<-rbind(soil.5cms,cbind(soil.5cm,sites[k],stringsAsFactors=FALSE))
    }
  }
  soil.5cms<-soil.5cms[order(soil.5cms$date_time),] 
  # get rid of values in November April when loggers were downloaded
  soil.5cms<-subset(soil.5cms,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
  soil.5cms<-subset(soil.5cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
  if(k==5){
    soil.5cms<-subset(soil.5cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
  }
  if(k==1){
    all.soil.5cms=soil.5cms
  }else{
    all.soil.5cms=rbind(all.soil.5cms,soil.5cms)
  }
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
      soil.15cms<-cbind(soil.15cm,sites[k],stringsAsFactors=FALSE)
    }else{
      soil.15cms<-rbind(soil.15cms,cbind(soil.15cm,sites[k],stringsAsFactors=FALSE))
    }
  }
  soil.15cms<-soil.15cms[order(soil.15cms$date_time),] 
  # get rid of values in November April when loggers were downloaded
  soil.15cms<-subset(soil.15cms,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
  soil.15cms<-subset(soil.15cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
  if(k==5){
    soil.15cms<-subset(soil.15cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
  }
  if(k==1){
    all.soil.15cms=soil.15cms
  }else{
    all.soil.15cms=rbind(all.soil.15cms,soil.15cms)
  }
  
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
      soil.30cms<-cbind(soil.30cm,sites[k],stringsAsFactors=FALSE)
    }else{
      soil.30cms<-rbind(soil.30cms,cbind(soil.30cm,sites[k],stringsAsFactors=FALSE))
    }
  }
  soil.30cms<-soil.30cms[order(soil.30cms$date_time),] 
  # get rid of values in November April when loggers were downloaded
  soil.30cms<-subset(soil.30cms,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
  soil.30cms<-subset(soil.30cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
  if(k==5){
    soil.30cms<-subset(soil.30cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
  }
  if(k==1){
    all.soil.30cms=soil.30cms
  }else{
    all.soil.30cms=rbind(all.soil.30cms,soil.30cms)
  }
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
      soil.50cms<-cbind(soil.50cm,sites[k],stringsAsFactors=FALSE)
    }else{
      soil.50cms<-rbind(soil.50cms,cbind(soil.50cm,sites[k],stringsAsFactors=FALSE))
    }
  }
  soil.50cms<-soil.50cms[order(soil.50cms$date_time),] 
  # get rid of values in November April when loggers were downloaded
  soil.50cms<-subset(soil.50cms,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
  soil.50cms<-subset(soil.50cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
  if(k==5){
    soil.50cms<-subset(soil.50cms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
  }
  soil.50cms<-soil.50cms[order(soil.50cms$date_time),] 
  if(k==1){
    all.soil.50cms=soil.50cms
  }else{
    all.soil.50cms=rbind(all.soil.50cms,soil.50cms)
  }
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
      soil.1ms<-cbind(soil.1m,sites[k],stringsAsFactors=FALSE)
    }else{
      soil.1ms<-rbind(soil.1ms,cbind(soil.1m,sites[k],stringsAsFactors=FALSE))
    }
  }
  soil.1ms<-soil.1ms[order(soil.1ms$date_time),] 
  # get rid of values in November April when loggers were downloaded
  soil.1ms<-subset(soil.1ms,as.numeric(date_time)<as.numeric(as.POSIXct("2013-11-10",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2013-11-13",origin="1970-01-01")))
  soil.1ms<-subset(soil.1ms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-04-25",origin="1970-01-01")))
  if(k==5){
    soil.1ms<-subset(soil.1ms,as.numeric(date_time)<as.numeric(as.POSIXct("2014-04-20",origin="1970-01-01")) | as.numeric(date_time)>as.numeric(as.POSIXct("2014-05-01",origin="1970-01-01")))
  }
  if(k==1){
    all.soil.1ms=soil.1ms
  }else{
    all.soil.1ms=rbind(all.soil.1ms,soil.1ms)
  }
  
  
} # end loop through all sites

all.soil.surfaces<-all.soil.surfaces[order(all.soil.surfaces$date_time),] 
all.soil.5cms<-all.soil.5cms[order(all.soil.5cms$date_time),] 
all.soil.15cms<-all.soil.15cms[order(all.soil.15cms$date_time),] 
all.soil.30cms<-all.soil.30cms[order(all.soil.30cms$date_time),] 
all.soil.50cms<-all.soil.50cms[order(all.soil.50cms$date_time),] 
all.soil.1ms<-all.soil.1ms[order(all.soil.1ms$date_time),] 

# code to remove data on days loggers were retrieved and downloaded

col.names<-c("date_time","temperature","site")
colnames(all.soil.surfaces)<-col.names
colnames(all.soil.5cms)<-col.names
colnames(all.soil.15cms)<-col.names
colnames(all.soil.30cms)<-col.names
colnames(all.soil.50cms)<-col.names
colnames(all.soil.1ms)<-col.names

burrow.remove.C1<-c("18/08/2013","11/11/2013","22/04/2014","01/05/2014","10/10/2014")
burrow.remove.C2<-c("18/08/2013","12/11/2013","22/04/2014","23/04/2014","11/10/2014")
burrow.remove.C3<-c("16/08/2013","11/11/2013","22/04/2014","01/05/2014","10/10/2014")
burrow.remove.T1<-c("18/08/2013","12/11/2013","21/04/2014","09/10/2014")
burrow.remove.T2<-c("18/08/2013","10/11/2013","21/04/2014","09/10/2014")
burrow.remove.T3<-c("19/08/2013","12/11/2013","21/04/2014","09/10/2014")

all.soil.surfaces<-subset(all.soil.surfaces, !(as.character(all.soil.surfaces$date_time, format="%d/%m/%Y")%in% burrow.remove.C1 & all.soil.surfaces$site=="C1"))
all.soil.surfaces<-subset(all.soil.surfaces, !(as.character(all.soil.surfaces$date_time, format="%d/%m/%Y")%in% burrow.remove.C2 & all.soil.surfaces$site=="C2"))
all.soil.surfaces<-subset(all.soil.surfaces, !(as.character(all.soil.surfaces$date_time, format="%d/%m/%Y")%in% burrow.remove.C3 & all.soil.surfaces$site=="C3"))
all.soil.surfaces<-subset(all.soil.surfaces, !(as.character(all.soil.surfaces$date_time, format="%d/%m/%Y")%in% burrow.remove.T1 & all.soil.surfaces$site=="T1"))
all.soil.surfaces<-subset(all.soil.surfaces, !(as.character(all.soil.surfaces$date_time, format="%d/%m/%Y")%in% burrow.remove.T2 & all.soil.surfaces$site=="T2"))
all.soil.surfaces<-subset(all.soil.surfaces, !(as.character(all.soil.surfaces$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.surfaces$site=="T2a"))
all.soil.surfaces<-subset(all.soil.surfaces, !(as.character(all.soil.surfaces$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.surfaces$site=="T3"))

all.soil.5cms<-subset(all.soil.5cms, !(as.character(all.soil.5cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C1 & all.soil.5cms$site=="C1"))
all.soil.5cms<-subset(all.soil.5cms, !(as.character(all.soil.5cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C2 & all.soil.5cms$site=="C2"))
all.soil.5cms<-subset(all.soil.5cms, !(as.character(all.soil.5cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C3 & all.soil.5cms$site=="C3"))
all.soil.5cms<-subset(all.soil.5cms, !(as.character(all.soil.5cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T1 & all.soil.5cms$site=="T1"))
all.soil.5cms<-subset(all.soil.5cms, !(as.character(all.soil.5cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T2 & all.soil.5cms$site=="T2"))
all.soil.5cms<-subset(all.soil.5cms, !(as.character(all.soil.5cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.5cms$site=="T2a"))
all.soil.5cms<-subset(all.soil.5cms, !(as.character(all.soil.5cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.5cms$site=="T3"))

all.soil.15cms<-subset(all.soil.15cms, !(as.character(all.soil.15cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C1 & all.soil.15cms$site=="C1"))
all.soil.15cms<-subset(all.soil.15cms, !(as.character(all.soil.15cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C2 & all.soil.15cms$site=="C2"))
all.soil.15cms<-subset(all.soil.15cms, !(as.character(all.soil.15cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C3 & all.soil.15cms$site=="C3"))
all.soil.15cms<-subset(all.soil.15cms, !(as.character(all.soil.15cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T1 & all.soil.15cms$site=="T1"))
all.soil.15cms<-subset(all.soil.15cms, !(as.character(all.soil.15cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T2 & all.soil.15cms$site=="T2"))
all.soil.15cms<-subset(all.soil.15cms, !(as.character(all.soil.15cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.15cms$site=="T2a"))
all.soil.15cms<-subset(all.soil.15cms, !(as.character(all.soil.15cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.15cms$site=="T3"))

all.soil.30cms<-subset(all.soil.30cms, !(as.character(all.soil.30cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C1 & all.soil.30cms$site=="C1"))
all.soil.30cms<-subset(all.soil.30cms, !(as.character(all.soil.30cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C2 & all.soil.30cms$site=="C2"))
all.soil.30cms<-subset(all.soil.30cms, !(as.character(all.soil.30cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C3 & all.soil.30cms$site=="C3"))
all.soil.30cms<-subset(all.soil.30cms, !(as.character(all.soil.30cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T1 & all.soil.30cms$site=="T1"))
all.soil.30cms<-subset(all.soil.30cms, !(as.character(all.soil.30cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T2 & all.soil.30cms$site=="T2"))
all.soil.30cms<-subset(all.soil.30cms, !(as.character(all.soil.30cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.30cms$site=="T2a"))
all.soil.30cms<-subset(all.soil.30cms, !(as.character(all.soil.30cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.30cms$site=="T3"))

all.soil.50cms<-subset(all.soil.50cms, !(as.character(all.soil.50cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C1 & all.soil.50cms$site=="C1"))
all.soil.50cms<-subset(all.soil.50cms, !(as.character(all.soil.50cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C2 & all.soil.50cms$site=="C2"))
all.soil.50cms<-subset(all.soil.50cms, !(as.character(all.soil.50cms$date_time, format="%d/%m/%Y")%in% burrow.remove.C3 & all.soil.50cms$site=="C3"))
all.soil.50cms<-subset(all.soil.50cms, !(as.character(all.soil.50cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T1 & all.soil.50cms$site=="T1"))
all.soil.50cms<-subset(all.soil.50cms, !(as.character(all.soil.50cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T2 & all.soil.50cms$site=="T2"))
all.soil.50cms<-subset(all.soil.50cms, !(as.character(all.soil.50cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.50cms$site=="T2a"))
all.soil.50cms<-subset(all.soil.50cms, !(as.character(all.soil.50cms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.50cms$site=="T3"))

all.soil.1ms<-subset(all.soil.1ms, !(as.character(all.soil.1ms$date_time, format="%d/%m/%Y")%in% burrow.remove.C1 & all.soil.1ms$site=="C1"))
all.soil.1ms<-subset(all.soil.1ms, !(as.character(all.soil.1ms$date_time, format="%d/%m/%Y")%in% burrow.remove.C2 & all.soil.1ms$site=="C2"))
all.soil.1ms<-subset(all.soil.1ms, !(as.character(all.soil.1ms$date_time, format="%d/%m/%Y")%in% burrow.remove.C3 & all.soil.1ms$site=="C3"))
all.soil.1ms<-subset(all.soil.1ms, !(as.character(all.soil.1ms$date_time, format="%d/%m/%Y")%in% burrow.remove.T1 & all.soil.1ms$site=="T1"))
all.soil.1ms<-subset(all.soil.1ms, !(as.character(all.soil.1ms$date_time, format="%d/%m/%Y")%in% burrow.remove.T2 & all.soil.1ms$site=="T2"))
all.soil.1ms<-subset(all.soil.1ms, !(as.character(all.soil.1ms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.1ms$site=="T2a"))
all.soil.1ms<-subset(all.soil.1ms, !(as.character(all.soil.1ms$date_time, format="%d/%m/%Y")%in% burrow.remove.T3 & all.soil.1ms$site=="T3"))
# end of code to remove data on days loggers were retrieved and downloaded

# summaries of maximum temperature

# aggregate to daily max over all sites
max.surface=aggregate(all.soil.surfaces, by=list(format(all.soil.surfaces$date_time,"%d-%m-%Y")), FUN = max)
max.5cms=aggregate(all.soil.5cms, by=list(format(all.soil.5cms$date_time,"%d-%m-%Y")), FUN = max)
max.15cms=aggregate(all.soil.15cms, by=list(format(all.soil.15cms$date_time,"%d-%m-%Y")), FUN = max)
max.30cms=aggregate(all.soil.30cms, by=list(format(all.soil.30cms$date_time,"%d-%m-%Y")), FUN = max)
max.50cms=aggregate(all.soil.50cms, by=list(format(all.soil.50cms$date_time,"%d-%m-%Y")), FUN = max)
max.1ms=aggregate(all.soil.1ms, by=list(format(all.soil.1ms$date_time,"%d-%m-%Y")), FUN = max)
max.surface<-max.surface[order(max.surface$date_time),] 
max.5cms<-max.5cms[order(max.5cms$date_time),] 
max.15cms<-max.15cms[order(max.15cms$date_time),] 
max.30cms<-max.30cms[order(max.30cms$date_time),] 
max.50cms<-max.50cms[order(max.50cms$date_time),] 
max.1ms<-max.1ms[order(max.1ms$date_time),] 

# aggregate to daily max per site
max.surface.site=aggregate(all.soil.surfaces[,2], by=list(format(all.soil.surfaces$date_time,"%d-%m-%Y"),all.soil.surfaces$site), FUN = max)
max.5cms.site=aggregate(all.soil.5cms[,2], by=list(format(all.soil.5cms$date_time,"%d-%m-%Y"),all.soil.5cms$site), FUN = max)
max.15cms.site=aggregate(all.soil.15cms[,2], by=list(format(all.soil.15cms$date_time,"%d-%m-%Y"),all.soil.15cms$site), FUN = max)
max.30cms.site=aggregate(all.soil.30cms[,2], by=list(format(all.soil.30cms$date_time,"%d-%m-%Y"),all.soil.30cms$site), FUN = max)
max.50cms.site=aggregate(all.soil.50cms[,2], by=list(format(all.soil.50cms$date_time,"%d-%m-%Y"),all.soil.50cms$site), FUN = max)
max.1ms.site=aggregate(all.soil.1ms[,2], by=list(format(all.soil.1ms$date_time,"%d-%m-%Y"),all.soil.1ms$site), FUN = max)

# get mean max over site
mean.max.surface=aggregate(max.surface.site[,3], by=list(max.surface.site$Group.1), FUN = mean)
mean.max.5cms=aggregate(max.5cms.site[,3], by=list(max.5cms.site$Group.1), FUN = mean)
mean.max.15cms=aggregate(max.15cms.site[,3], by=list(max.15cms.site$Group.1), FUN = mean)
mean.max.30cms=aggregate(max.30cms.site[,3], by=list(max.30cms.site$Group.1), FUN = mean)
mean.max.50cms=aggregate(max.50cms.site[,3], by=list(max.50cms.site$Group.1), FUN = mean)
mean.max.1ms=aggregate(max.1ms.site[,3], by=list(max.1ms.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.max.surface=aggregate(max.surface.site[,3], by=list(max.surface.site$Group.1), FUN = st.err)
se.max.5cms=aggregate(max.5cms.site[,3], by=list(max.5cms.site$Group.1), FUN = st.err)
se.max.15cms=aggregate(max.15cms.site[,3], by=list(max.15cms.site$Group.1), FUN = st.err)
se.max.30cms=aggregate(max.30cms.site[,3], by=list(max.30cms.site$Group.1), FUN = st.err)
se.max.50cms=aggregate(max.50cms.site[,3], by=list(max.50cms.site$Group.1), FUN = st.err)
se.max.1ms=aggregate(max.1ms.site[,3], by=list(max.1ms.site$Group.1), FUN = st.err)

sd.max.surface=aggregate(max.surface.site[,3], by=list(max.surface.site$Group.1), FUN = sd)
sd.max.5cms=aggregate(max.5cms.site[,3], by=list(max.5cms.site$Group.1), FUN = sd)
sd.max.15cms=aggregate(max.15cms.site[,3], by=list(max.15cms.site$Group.1), FUN = sd)
sd.max.30cms=aggregate(max.30cms.site[,3], by=list(max.30cms.site$Group.1), FUN = sd)
sd.max.50cms=aggregate(max.50cms.site[,3], by=list(max.50cms.site$Group.1), FUN = sd)
sd.max.1ms=aggregate(max.1ms.site[,3], by=list(max.1ms.site$Group.1), FUN = sd)

# combine means and standard errors
mean.max.surface=cbind(mean.max.surface, se.max.surface[,2], sd.max.surface[,2])
mean.max.5cms=cbind(mean.max.5cms, se.max.5cms[,2], sd.max.5cms[,2])
mean.max.15cms=cbind(mean.max.15cms, se.max.15cms[,2], sd.max.15cms[,2])
mean.max.30cms=cbind(mean.max.30cms, se.max.30cms[,2], sd.max.30cms[,2])
mean.max.50cms=cbind(mean.max.50cms, se.max.50cms[,2], sd.max.50cms[,2])
mean.max.1ms=cbind(mean.max.1ms, se.max.1ms[,2], sd.max.1ms[,2])
# label columns
names<-c("date","mean.max.temperature","se.max.temperature","sd.max.temperature")
colnames(mean.max.surface)<-names
colnames(mean.max.5cms)<-names
colnames(mean.max.15cms)<-names
colnames(mean.max.30cms)<-names
colnames(mean.max.50cms)<-names
colnames(mean.max.1ms)<-names
# format dates
mean.max.surface$date<-as.POSIXct(mean.max.surface$date,format="%d-%m-%Y")
mean.max.5cms$date<-as.POSIXct(mean.max.5cms$date,format="%d-%m-%Y")
mean.max.15cms$date<-as.POSIXct(mean.max.15cms$date,format="%d-%m-%Y")
mean.max.30cms$date<-as.POSIXct(mean.max.30cms$date,format="%d-%m-%Y")
mean.max.50cms$date<-as.POSIXct(mean.max.50cms$date,format="%d-%m-%Y")
mean.max.1ms$date<-as.POSIXct(mean.max.1ms$date,format="%d-%m-%Y")
# put in date order
mean.max.surface<-mean.max.surface[order(mean.max.surface$date),] 
mean.max.5cms<-mean.max.5cms[order(mean.max.5cms$date),] 
mean.max.15cms<-mean.max.15cms[order(mean.max.15cms$date),] 
mean.max.30cms<-mean.max.30cms[order(mean.max.30cms$date),] 
mean.max.50cms<-mean.max.50cms[order(mean.max.50cms$date),] 
mean.max.1ms<-mean.max.1ms[order(mean.max.1ms$date),] 

# summaries of minimum temperatures

# aggregate to daily min over all sites
min.surface=aggregate(all.soil.surfaces, by=list(format(all.soil.surfaces$date_time,"%d-%m-%Y")), FUN = min)
min.5cms=aggregate(all.soil.5cms, by=list(format(all.soil.5cms$date_time,"%d-%m-%Y")), FUN = min)
min.15cms=aggregate(all.soil.15cms, by=list(format(all.soil.15cms$date_time,"%d-%m-%Y")), FUN = min)
min.30cms=aggregate(all.soil.30cms, by=list(format(all.soil.30cms$date_time,"%d-%m-%Y")), FUN = min)
min.50cms=aggregate(all.soil.50cms, by=list(format(all.soil.50cms$date_time,"%d-%m-%Y")), FUN = min)
min.1ms=aggregate(all.soil.1ms, by=list(format(all.soil.1ms$date_time,"%d-%m-%Y")), FUN = min)
min.surface<-min.surface[order(min.surface$date_time),] 
min.5cms<-min.5cms[order(min.5cms$date_time),] 
min.15cms<-min.15cms[order(min.15cms$date_time),] 
min.30cms<-min.30cms[order(min.30cms$date_time),] 
min.50cms<-min.50cms[order(min.50cms$date_time),] 
min.1ms<-min.1ms[order(min.1ms$date_time),] 

# aggregate to daily min per site
min.surface.site=aggregate(all.soil.surfaces[,2], by=list(format(all.soil.surfaces$date_time,"%d-%m-%Y"),all.soil.surfaces$site), FUN = min)
min.5cms.site=aggregate(all.soil.5cms[,2], by=list(format(all.soil.5cms$date_time,"%d-%m-%Y"),all.soil.5cms$site), FUN = min)
min.15cms.site=aggregate(all.soil.15cms[,2], by=list(format(all.soil.15cms$date_time,"%d-%m-%Y"),all.soil.15cms$site), FUN = min)
min.30cms.site=aggregate(all.soil.30cms[,2], by=list(format(all.soil.30cms$date_time,"%d-%m-%Y"),all.soil.30cms$site), FUN = min)
min.50cms.site=aggregate(all.soil.50cms[,2], by=list(format(all.soil.50cms$date_time,"%d-%m-%Y"),all.soil.50cms$site), FUN = min)
min.1ms.site=aggregate(all.soil.1ms[,2], by=list(format(all.soil.1ms$date_time,"%d-%m-%Y"),all.soil.1ms$site), FUN = min)

# get mean min over site
mean.min.surface=aggregate(min.surface.site[,3], by=list(min.surface.site$Group.1), FUN = mean)
mean.min.5cms=aggregate(min.5cms.site[,3], by=list(min.5cms.site$Group.1), FUN = mean)
mean.min.15cms=aggregate(min.15cms.site[,3], by=list(min.15cms.site$Group.1), FUN = mean)
mean.min.30cms=aggregate(min.30cms.site[,3], by=list(min.30cms.site$Group.1), FUN = mean)
mean.min.50cms=aggregate(min.50cms.site[,3], by=list(min.50cms.site$Group.1), FUN = mean)
mean.min.1ms=aggregate(min.1ms.site[,3], by=list(min.1ms.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.min.surface=aggregate(min.surface.site[,3], by=list(min.surface.site$Group.1), FUN = st.err)
se.min.5cms=aggregate(min.5cms.site[,3], by=list(min.5cms.site$Group.1), FUN = st.err)
se.min.15cms=aggregate(min.15cms.site[,3], by=list(min.15cms.site$Group.1), FUN = st.err)
se.min.30cms=aggregate(min.30cms.site[,3], by=list(min.30cms.site$Group.1), FUN = st.err)
se.min.50cms=aggregate(min.50cms.site[,3], by=list(min.50cms.site$Group.1), FUN = st.err)
se.min.1ms=aggregate(min.1ms.site[,3], by=list(min.1ms.site$Group.1), FUN = st.err)

sd.min.surface=aggregate(min.surface.site[,3], by=list(min.surface.site$Group.1), FUN = sd)
sd.min.5cms=aggregate(min.5cms.site[,3], by=list(min.5cms.site$Group.1), FUN = sd)
sd.min.15cms=aggregate(min.15cms.site[,3], by=list(min.15cms.site$Group.1), FUN = sd)
sd.min.30cms=aggregate(min.30cms.site[,3], by=list(min.30cms.site$Group.1), FUN = sd)
sd.min.50cms=aggregate(min.50cms.site[,3], by=list(min.50cms.site$Group.1), FUN = sd)
sd.min.1ms=aggregate(min.1ms.site[,3], by=list(min.1ms.site$Group.1), FUN = sd)

mean.min.surface=cbind(mean.min.surface, se.min.surface[,2], sd.min.surface[,2])
mean.min.5cms=cbind(mean.min.5cms, se.min.5cms[,2], sd.min.5cms[,2])
mean.min.15cms=cbind(mean.min.15cms, se.min.15cms[,2], sd.min.15cms[,2])
mean.min.30cms=cbind(mean.min.30cms, se.min.30cms[,2], sd.min.30cms[,2])
mean.min.50cms=cbind(mean.min.50cms, se.min.50cms[,2], sd.min.50cms[,2])
mean.min.1ms=cbind(mean.min.1ms, se.min.1ms[,2], sd.min.1ms[,2])

names<-c("date","mean.min.temperature","se.min.temperature","sd.min.temperature")
colnames(mean.min.surface)<-names
colnames(mean.min.5cms)<-names
colnames(mean.min.15cms)<-names
colnames(mean.min.30cms)<-names
colnames(mean.min.50cms)<-names
colnames(mean.min.1ms)<-names

mean.min.surface$date<-as.POSIXct(mean.min.surface$date,format="%d-%m-%Y")
mean.min.5cms$date<-as.POSIXct(mean.min.5cms$date,format="%d-%m-%Y")
mean.min.15cms$date<-as.POSIXct(mean.min.15cms$date,format="%d-%m-%Y")
mean.min.30cms$date<-as.POSIXct(mean.min.30cms$date,format="%d-%m-%Y")
mean.min.50cms$date<-as.POSIXct(mean.min.50cms$date,format="%d-%m-%Y")
mean.min.1ms$date<-as.POSIXct(mean.min.1ms$date,format="%d-%m-%Y")

mean.min.surface<-mean.min.surface[order(mean.min.surface$date),] 
mean.min.5cms<-mean.min.5cms[order(mean.min.5cms$date),] 
mean.min.15cms<-mean.min.15cms[order(mean.min.15cms$date),] 
mean.min.30cms<-mean.min.30cms[order(mean.min.30cms$date),] 
mean.min.50cms<-mean.min.50cms[order(mean.min.50cms$date),] 
mean.min.1ms<-mean.min.1ms[order(mean.min.1ms$date),] 

# summaries of mean temperatures

# aggregate to daily mean over all sites
mean.surface=aggregate(all.soil.surfaces, by=list(format(all.soil.surfaces$date_time,"%d-%m-%Y")), FUN = mean)
mean.5cms=aggregate(all.soil.5cms, by=list(format(all.soil.5cms$date_time,"%d-%m-%Y")), FUN = mean)
mean.15cms=aggregate(all.soil.15cms, by=list(format(all.soil.15cms$date_time,"%d-%m-%Y")), FUN = mean)
mean.30cms=aggregate(all.soil.30cms, by=list(format(all.soil.30cms$date_time,"%d-%m-%Y")), FUN = mean)
mean.50cms=aggregate(all.soil.50cms, by=list(format(all.soil.50cms$date_time,"%d-%m-%Y")), FUN = mean)
mean.1ms=aggregate(all.soil.1ms, by=list(format(all.soil.1ms$date_time,"%d-%m-%Y")), FUN = mean)
mean.surface<-mean.surface[order(mean.surface$date_time),] 
mean.5cms<-mean.5cms[order(mean.5cms$date_time),] 
mean.15cms<-mean.15cms[order(mean.15cms$date_time),] 
mean.30cms<-mean.30cms[order(mean.30cms$date_time),] 
mean.50cms<-mean.50cms[order(mean.50cms$date_time),] 
mean.1ms<-mean.1ms[order(mean.1ms$date_time),] 

# aggregate to daily mean per site
mean.surface.site=aggregate(all.soil.surfaces[,2], by=list(format(all.soil.surfaces$date_time,"%d-%m-%Y"),all.soil.surfaces$site), FUN = mean)
mean.5cms.site=aggregate(all.soil.5cms[,2], by=list(format(all.soil.5cms$date_time,"%d-%m-%Y"),all.soil.5cms$site), FUN = mean)
mean.15cms.site=aggregate(all.soil.15cms[,2], by=list(format(all.soil.15cms$date_time,"%d-%m-%Y"),all.soil.15cms$site), FUN = mean)
mean.30cms.site=aggregate(all.soil.30cms[,2], by=list(format(all.soil.30cms$date_time,"%d-%m-%Y"),all.soil.30cms$site), FUN = mean)
mean.50cms.site=aggregate(all.soil.50cms[,2], by=list(format(all.soil.50cms$date_time,"%d-%m-%Y"),all.soil.50cms$site), FUN = mean)
mean.1ms.site=aggregate(all.soil.1ms[,2], by=list(format(all.soil.1ms$date_time,"%d-%m-%Y"),all.soil.1ms$site), FUN = mean)

# get mean mean over site
mean.mean.surface=aggregate(mean.surface.site[,3], by=list(mean.surface.site$Group.1), FUN = mean)
mean.mean.5cms=aggregate(mean.5cms.site[,3], by=list(mean.5cms.site$Group.1), FUN = mean)
mean.mean.15cms=aggregate(mean.15cms.site[,3], by=list(mean.15cms.site$Group.1), FUN = mean)
mean.mean.30cms=aggregate(mean.30cms.site[,3], by=list(mean.30cms.site$Group.1), FUN = mean)
mean.mean.50cms=aggregate(mean.50cms.site[,3], by=list(mean.50cms.site$Group.1), FUN = mean)
mean.mean.1ms=aggregate(mean.1ms.site[,3], by=list(mean.1ms.site$Group.1), FUN = mean)

# get standard error and standard deviation of the mean over site
se.mean.surface=aggregate(mean.surface.site[,3], by=list(mean.surface.site$Group.1), FUN = st.err)
se.mean.5cms=aggregate(mean.5cms.site[,3], by=list(mean.5cms.site$Group.1), FUN = st.err)
se.mean.15cms=aggregate(mean.15cms.site[,3], by=list(mean.15cms.site$Group.1), FUN = st.err)
se.mean.30cms=aggregate(mean.30cms.site[,3], by=list(mean.30cms.site$Group.1), FUN = st.err)
se.mean.50cms=aggregate(mean.50cms.site[,3], by=list(mean.50cms.site$Group.1), FUN = st.err)
se.mean.1ms=aggregate(mean.1ms.site[,3], by=list(mean.1ms.site$Group.1), FUN = st.err)

sd.mean.surface=aggregate(mean.surface.site[,3], by=list(mean.surface.site$Group.1), FUN = sd)
sd.mean.5cms=aggregate(mean.5cms.site[,3], by=list(mean.5cms.site$Group.1), FUN = sd)
sd.mean.15cms=aggregate(mean.15cms.site[,3], by=list(mean.15cms.site$Group.1), FUN = sd)
sd.mean.30cms=aggregate(mean.30cms.site[,3], by=list(mean.30cms.site$Group.1), FUN = sd)
sd.mean.50cms=aggregate(mean.50cms.site[,3], by=list(mean.50cms.site$Group.1), FUN = sd)
sd.mean.1ms=aggregate(mean.1ms.site[,3], by=list(mean.1ms.site$Group.1), FUN = sd)

mean.mean.surface=cbind(mean.mean.surface, se.mean.surface[,2], sd.mean.surface[,2])
mean.mean.5cms=cbind(mean.mean.5cms, se.mean.5cms[,2], sd.mean.5cms[,2])
mean.mean.15cms=cbind(mean.mean.15cms, se.mean.15cms[,2], sd.mean.15cms[,2])
mean.mean.30cms=cbind(mean.mean.30cms, se.mean.30cms[,2], sd.mean.30cms[,2])
mean.mean.50cms=cbind(mean.mean.50cms, se.mean.50cms[,2], sd.mean.50cms[,2])
mean.mean.1ms=cbind(mean.mean.1ms, se.mean.1ms[,2], sd.mean.1ms[,2])

names<-c("date","mean.mean.temperature","se.mean.temperature","sd.mean.temperature")
colnames(mean.mean.surface)<-names
colnames(mean.mean.5cms)<-names
colnames(mean.mean.15cms)<-names
colnames(mean.mean.30cms)<-names
colnames(mean.mean.50cms)<-names
colnames(mean.mean.1ms)<-names

mean.mean.surface$date<-as.POSIXct(mean.mean.surface$date,format="%d-%m-%Y")
mean.mean.5cms$date<-as.POSIXct(mean.mean.5cms$date,format="%d-%m-%Y")
mean.mean.15cms$date<-as.POSIXct(mean.mean.15cms$date,format="%d-%m-%Y")
mean.mean.30cms$date<-as.POSIXct(mean.mean.30cms$date,format="%d-%m-%Y")
mean.mean.50cms$date<-as.POSIXct(mean.mean.50cms$date,format="%d-%m-%Y")
mean.mean.1ms$date<-as.POSIXct(mean.mean.1ms$date,format="%d-%m-%Y")

mean.mean.surface<-mean.mean.surface[order(mean.mean.surface$date),] 
mean.mean.5cms<-mean.mean.5cms[order(mean.mean.5cms$date),] 
mean.mean.15cms<-mean.mean.15cms[order(mean.mean.15cms$date),] 
mean.mean.30cms<-mean.mean.30cms[order(mean.mean.30cms$date),] 
mean.mean.50cms<-mean.mean.50cms[order(mean.mean.50cms$date),] 
mean.mean.1ms<-mean.mean.1ms[order(mean.mean.1ms$date),] 

###################### write all collated/aggregated data to csv files ###################

write.csv(all.burrow.surf,paste0(dropbox,"csv summaries/all.burrow.surf.csv"))
write.csv(all.burrow.mid,paste0(dropbox,"csv summaries/all.burrow.mid.csv"))
write.csv(all.burrow.deep,paste0(dropbox,"csv summaries/all.burrow.deep.csv"))

write.csv(max.surf,paste0(dropbox,"csv summaries/max.surf.csv"))
write.csv(max.mid,paste0(dropbox,"csv summaries/max.mid.csv"))
write.csv(max.deep,paste0(dropbox,"csv summaries/max.deep.csv"))

write.csv(min.surf,paste0(dropbox,"csv summaries/min.surf.csv"))
write.csv(min.mid,paste0(dropbox,"csv summaries/min.mid.csv"))
write.csv(min.deep,paste0(dropbox,"csv summaries/min.deep.csv"))

write.csv(mean.surf,paste0(dropbox,"csv summaries/mean.surf.csv"))
write.csv(mean.mid,paste0(dropbox,"csv summaries/mean.mid.csv"))
write.csv(mean.deep,paste0(dropbox,"csv summaries/mean.deep.csv"))

write.csv(max.surf,paste0(dropbox,"csv summaries/max.surf.csv"))
write.csv(max.mid,paste0(dropbox,"csv summaries/max.mid.csv"))
write.csv(max.deep,paste0(dropbox,"csv summaries/max.deep.csv"))

write.csv(mean.max.surf,paste0(dropbox,"csv summaries/mean.max.surf.csv"))
write.csv(mean.max.mid,paste0(dropbox,"csv summaries/mean.max.mid.csv"))
write.csv(mean.max.deep,paste0(dropbox,"csv summaries/mean.max.deep.csv"))

write.csv(mean.min.surf,paste0(dropbox,"csv summaries/mean.min.surf.csv"))
write.csv(mean.min.mid,paste0(dropbox,"csv summaries/mean.min.mid.csv"))
write.csv(mean.min.deep,paste0(dropbox,"csv summaries/mean.min.deep.csv"))

write.csv(mean.mean.surf,paste0(dropbox,"csv summaries/mean.mean.surf.csv"))
write.csv(mean.mean.mid,paste0(dropbox,"csv summaries/mean.mean.mid.csv"))
write.csv(mean.mean.deep,paste0(dropbox,"csv summaries/mean.mean.deep.csv"))

write.csv(all.adult.fullsuns,paste0(dropbox,"csv summaries/all.adult.fullsuns.csv"))
write.csv(all.adult.partshades,paste0(dropbox,"csv summaries/all.adult.partshades.csv"))
write.csv(all.adult.fullshades,paste0(dropbox,"csv summaries/all.adult.fullshades.csv"))
write.csv(all.juv.fullsuns,paste0(dropbox,"csv summaries/all.juv.fullsuns.csv"))
write.csv(all.juv.partshades,paste0(dropbox,"csv summaries/all.juv.partshades.csv"))
write.csv(all.juv.fullshades,paste0(dropbox,"csv summaries/all.juv.fullshades.csv"))

write.csv(max.adult.fullsuns,paste0(dropbox,"csv summaries/max.adult.fullsuns.csv"))
write.csv(max.adult.partshades,paste0(dropbox,"csv summaries/max.adult.partshades.csv"))
write.csv(max.adult.fullshades,paste0(dropbox,"csv summaries/max.adult.fullshades.csv"))
write.csv(max.juv.fullsuns,paste0(dropbox,"csv summaries/max.juv.fullsuns.csv"))
write.csv(max.juv.partshades,paste0(dropbox,"csv summaries/max.juv.partshades.csv"))
write.csv(max.juv.fullshades,paste0(dropbox,"csv summaries/max.juv.fullshades.csv"))

write.csv(min.adult.fullsuns,paste0(dropbox,"csv summaries/min.adult.fullsuns.csv"))
write.csv(min.adult.partshades,paste0(dropbox,"csv summaries/min.adult.partshades.csv"))
write.csv(min.adult.fullshades,paste0(dropbox,"csv summaries/min.adult.fullshades.csv"))
write.csv(min.juv.fullsuns,paste0(dropbox,"csv summaries/min.juv.fullsuns.csv"))
write.csv(min.juv.partshades,paste0(dropbox,"csv summaries/min.juv.partshades.csv"))
write.csv(min.juv.fullshades,paste0(dropbox,"csv summaries/min.juv.fullshades.csv"))

write.csv(mean.adult.fullsuns,paste0(dropbox,"csv summaries/mean.adult.fullsuns.csv"))
write.csv(mean.adult.partshades,paste0(dropbox,"csv summaries/mean.adult.partshades.csv"))
write.csv(mean.adult.fullshades,paste0(dropbox,"csv summaries/mean.adult.fullshades.csv"))
write.csv(mean.juv.fullsuns,paste0(dropbox,"csv summaries/mean.juv.fullsuns.csv"))
write.csv(mean.juv.partshades,paste0(dropbox,"csv summaries/mean.juv.partshades.csv"))
write.csv(mean.juv.fullshades,paste0(dropbox,"csv summaries/mean.juv.fullshades.csv"))

write.csv(mean.max.adult.fullsun,paste0(dropbox,"csv summaries/mean.max.adult.fullsuns.csv"))
write.csv(mean.max.adult.partshade,paste0(dropbox,"csv summaries/mean.max.adult.partshades.csv"))
write.csv(mean.max.adult.fullshade,paste0(dropbox,"csv summaries/mean.max.adult.fullshades.csv"))
write.csv(mean.max.juv.fullsun,paste0(dropbox,"csv summaries/mean.max.juv.fullsuns.csv"))
write.csv(mean.max.juv.partshade,paste0(dropbox,"csv summaries/mean.max.juv.partshades.csv"))
write.csv(mean.max.juv.fullshade,paste0(dropbox,"csv summaries/mean.max.juv.fullshades.csv"))

write.csv(mean.min.adult.fullsun,paste0(dropbox,"csv summaries/mean.min.adult.fullsuns.csv"))
write.csv(mean.min.adult.partshade,paste0(dropbox,"csv summaries/mean.min.adult.partshades.csv"))
write.csv(mean.min.adult.fullshade,paste0(dropbox,"csv summaries/mean.min.adult.fullshades.csv"))
write.csv(mean.min.juv.fullsun,paste0(dropbox,"csv summaries/mean.min.juv.fullsuns.csv"))
write.csv(mean.min.juv.partshade,paste0(dropbox,"csv summaries/mean.min.juv.partshades.csv"))
write.csv(mean.min.juv.fullshade,paste0(dropbox,"csv summaries/mean.min.juv.fullshades.csv"))

write.csv(mean.mean.adult.fullsun,paste0(dropbox,"csv summaries/mean.mean.adult.fullsuns.csv"))
write.csv(mean.mean.adult.partshade,paste0(dropbox,"csv summaries/mean.mean.adult.partshades.csv"))
write.csv(mean.mean.adult.fullshade,paste0(dropbox,"csv summaries/mean.mean.adult.fullshades.csv"))
write.csv(mean.mean.juv.fullsun,paste0(dropbox,"csv summaries/mean.mean.juv.fullsuns.csv"))
write.csv(mean.mean.juv.partshade,paste0(dropbox,"csv summaries/mean.mean.juv.partshades.csv"))
write.csv(mean.mean.juv.fullshade,paste0(dropbox,"csv summaries/mean.mean.juv.fullshades.csv"))

write.csv(all.soil.surfaces,paste0(dropbox,"csv summaries/all.soil.surfaces.csv"))
write.csv(all.soil.5cms,paste0(dropbox,"csv summaries/all.soil.5cms.csv"))
write.csv(all.soil.15cms,paste0(dropbox,"csv summaries/all.soil.15cms.csv"))
write.csv(all.soil.30cms,paste0(dropbox,"csv summaries/all.soil.30cms.csv"))
write.csv(all.soil.50cms,paste0(dropbox,"csv summaries/all.soil.50cms.csv"))
write.csv(all.soil.1ms,paste0(dropbox,"csv summaries/all.soil.1ms.csv"))

write.csv(max.surface,paste0(dropbox,"csv summaries/max.surface.csv"))
write.csv(max.5cms,paste0(dropbox,"csv summaries/max.5cms.csv"))
write.csv(max.15cms,paste0(dropbox,"csv summaries/max.15cms.csv"))
write.csv(max.30cms,paste0(dropbox,"csv summaries/max.30cms.csv"))
write.csv(max.50cms,paste0(dropbox,"csv summaries/max.50cms.csv"))
write.csv(max.1ms,paste0(dropbox,"csv summaries/max.1ms.csv"))

write.csv(min.surface,paste0(dropbox,"csv summaries/min.surface.csv"))
write.csv(min.5cms,paste0(dropbox,"csv summaries/min.5cms.csv"))
write.csv(min.15cms,paste0(dropbox,"csv summaries/min.15cms.csv"))
write.csv(min.30cms,paste0(dropbox,"csv summaries/min.30cms.csv"))
write.csv(min.50cms,paste0(dropbox,"csv summaries/min.50cms.csv"))
write.csv(min.1ms,paste0(dropbox,"csv summaries/min.1ms.csv"))

write.csv(mean.surface,paste0(dropbox,"csv summaries/mean.surface.csv"))
write.csv(mean.5cms,paste0(dropbox,"csv summaries/mean.5cms.csv"))
write.csv(mean.15cms,paste0(dropbox,"csv summaries/mean.15cms.csv"))
write.csv(mean.30cms,paste0(dropbox,"csv summaries/mean.30cms.csv"))
write.csv(mean.50cms,paste0(dropbox,"csv summaries/mean.50cms.csv"))
write.csv(mean.1ms,paste0(dropbox,"csv summaries/mean.1ms.csv"))

write.csv(mean.max.surface,paste0(dropbox,"csv summaries/mean.max.surface.csv"))
write.csv(mean.max.5cms,paste0(dropbox,"csv summaries/mean.max.5cms.csv"))
write.csv(mean.max.15cms,paste0(dropbox,"csv summaries/mean.max.15cms.csv"))
write.csv(mean.max.30cms,paste0(dropbox,"csv summaries/mean.max.30cms.csv"))
write.csv(mean.max.50cms,paste0(dropbox,"csv summaries/mean.max.50cms.csv"))
write.csv(mean.max.1ms,paste0(dropbox,"csv summaries/mean.max.1ms.csv"))

write.csv(mean.min.surface,paste0(dropbox,"csv summaries/mean.min.surface.csv"))
write.csv(mean.min.5cms,paste0(dropbox,"csv summaries/mean.min.5cms.csv"))
write.csv(mean.min.15cms,paste0(dropbox,"csv summaries/mean.min.15cms.csv"))
write.csv(mean.min.30cms,paste0(dropbox,"csv summaries/mean.min.30cms.csv"))
write.csv(mean.min.50cms,paste0(dropbox,"csv summaries/mean.min.50cms.csv"))
write.csv(mean.min.1ms,paste0(dropbox,"csv summaries/mean.min.1ms.csv"))

write.csv(mean.mean.surface,paste0(dropbox,"csv summaries/mean.mean.surface.csv"))
write.csv(mean.mean.5cms,paste0(dropbox,"csv summaries/mean.mean.5cms.csv"))
write.csv(mean.mean.15cms,paste0(dropbox,"csv summaries/mean.mean.15cms.csv"))
write.csv(mean.mean.30cms,paste0(dropbox,"csv summaries/mean.mean.30cms.csv"))
write.csv(mean.mean.50cms,paste0(dropbox,"csv summaries/mean.mean.50cms.csv"))
write.csv(mean.mean.1ms,paste0(dropbox,"csv summaries/mean.mean.1ms.csv"))



