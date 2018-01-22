source("burrow chapter final scripts/bar.R")
library(plyr)
# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"

############################# read in copper model data and format dates ###################################################

# read in data
all.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/all.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.fullshades=read.csv(paste0(dropbox,"csv summaries/all.adult.fullshades.csv"),stringsAsFactors = FALSE)[,-1]

all.adult.fullsuns$date_time = as.POSIXct(all.adult.fullsuns$date_time, format = "%Y-%m-%d %H")
all.adult.fullshades$date_time = as.POSIXct(all.adult.fullshades$date_time, format = "%Y-%m-%d %H")

all.adult.fullsuns_hourly <- ddply(all.adult.fullsuns, .(date_time, timeday), summarize, temperature=mean(temperature))
all.adult.fullsuns_hourly <- all.adult.fullsuns_hourly[which(!duplicated(all.adult.fullsuns_hourly$date_time)),]

all.adult.fullshades_hourly <- ddply(all.adult.fullshades, .(date_time, timeday), summarize, temperature=mean(temperature))
all.adult.fullshades_hourly <- all.adult.fullshades_hourly[which(!duplicated(all.adult.fullshades_hourly$date_time)),]

colnames(all.adult.fullsuns_hourly)[3] <- "c.sun"
colnames(all.adult.fullshades_hourly)[3] <- "c.shd"

# read in data
all.burrow.surf=read.csv(paste0(dropbox,"csv summaries/all.burrow.surf.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.deep=read.csv(paste0(dropbox,"csv summaries/all.burrow.deep.csv"),stringsAsFactors = FALSE)[,-1]
colnames(all.burrow.surf)[6] <- "timeday"
colnames(all.burrow.deep)[6] <- "timeday"

all.burrow.surf$date_time = as.POSIXct(all.burrow.surf$date_time, format = "%Y-%m-%d %H")
all.burrow.deep$date_time = as.POSIXct(all.burrow.deep$date_time, format = "%Y-%m-%d %H")

all.burrow.surf_hourly <- ddply(all.burrow.surf, .(date_time, timeday), summarize, temperature=mean(temperature))
all.burrow.surf_hourly <- all.burrow.surf_hourly[which(!duplicated(all.burrow.surf_hourly$date_time)),]
all.burrow.deep_hourly <- ddply(all.burrow.deep, .(date_time, timeday), summarize, temperature=mean(temperature))
all.burrow.deep_hourly <- all.burrow.deep_hourly[which(!duplicated(all.burrow.deep_hourly$date_time)),]

colnames(all.burrow.surf_hourly)[3] <- "b.surf"
colnames(all.burrow.deep_hourly)[3] <- "b.deep"

warming <- c(0,2.8)

filename=paste("potential activity per month.pdf",sep="") 
pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R

# loop through warming scenarios
for(j in 1:2){
warm <- warming[j]

potential.act <- merge(all.adult.fullsuns_hourly, all.adult.fullshades_hourly, by = "date_time")
potential.act <- merge(potential.act, all.burrow.surf_hourly, by = "date_time")
potential.act <- merge(potential.act, all.burrow.deep_hourly, by = "date_time")
potential.act <- potential.act[, -c(4,6,8)]
#potential.act$timeday.x[potential.act$timeday.x == "dusk" | potential.act$timeday.x == "dawn"] <- "duskdawn"



# max and min Tb from lines 39 and 40 in 'activity figures.R'
maxTb <- 39.61
minTb <- 25.67

potential.act$c.sun[potential.act$c.sun+warm>maxTb]<-0 # zero too hot
potential.act$c.sun[potential.act$c.sun+warm<minTb]<-0 # zero too cold
potential.act$c.sun[potential.act$c.sun!=0]<-1 # 1 for just right
potential.act$c.shd[potential.act$c.sun==1]<-0 # zero, suitable in open
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm>maxTb]<-0 # zero, too hot in shade
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm<minTb]<-0 # zero, too cold in shade
potential.act$c.shd[potential.act$c.shd!=0]<-1 # 1 for just right in shade
potential.act$b.surf[potential.act$c.sun==1 | potential.act$c.shd==1]<-0 # zero, suitable on surface
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm>maxTb]<-0 # zero, too hot shallow
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm<minTb]<-0 # zero, too cold shallow
potential.act$b.surf[potential.act$b.surf!=0]<-1 # 1 for just right shallow
potential.act$b.deep[potential.act$c.sun==1 | potential.act$c.shd==1 | potential.act$b.surf==1]<-0 # zero suitable on surface or shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm>maxTb]<-0 # zero, too hot shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm<minTb]<-0 # zero, too cold shallow
potential.act$b.deep[potential.act$b.deep!=0]<-1 # 1 for just right shallow
potential.act$state <- "inactive.deep"
potential.act$state[potential.act$c.sun==1]<-"active.open"
potential.act$state[potential.act$c.shd==1]<-"active.shade"
potential.act$state[potential.act$b.surf==1]<-"active.shallow"

mons <- c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')

par(mfrow = c(3,4))
for(i in 1:12){
  potential.act.seas <- subset(potential.act, as.numeric(format(potential.act$date_time, "%m")) == i)
counts <- table(potential.act.seas$state, potential.act.seas$timeday)

if(nrow(counts)<4){
  if(!"active.shallow" %in% row.names(counts)){
cols <- c("black", "blue", "light blue")
order=c(3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,5]),][,-5]
}else{
cat('check')
}
}else{

cols <- c("black", "orange", "blue", "light blue")
order=c(4, 3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,5]),][,-5]
}


barplot(props, ylim = c(0,1.5), cex.lab = 1.3, cex = 1.3, cex.axis =1.3, tck=0.01, yaxs = "i", ylab = "proportion of time",
  xlab="", col=cols, main = mons[i] )
box()
title(main=paste0(warming[j], " degrees C warming"),outer=T, line = -2)

}
}
dev.off()