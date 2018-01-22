library(lattice)
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"
source(paste0(dropbox,"burrow chapter final scripts/bar.R"))
############################# read in copper model data and format dates ###################################################

# read in data
all.burrow.surf=read.csv(paste0(dropbox,"csv summaries/all.burrow.surf.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.mid=read.csv(paste0(dropbox,"csv summaries/all.burrow.mid.csv"),stringsAsFactors = FALSE)[,-1]
all.burrow.deep=read.csv(paste0(dropbox,"csv summaries/all.burrow.deep.csv"),stringsAsFactors = FALSE)[,-1]

all.burrow.surf$date_time = as.POSIXct(all.burrow.surf$date_time, format = "%Y-%m-%d")
all.burrow.mid$date_time = as.POSIXct(all.burrow.mid$date_time, format = "%Y-%m-%d")
all.burrow.deep$date_time = as.POSIXct(all.burrow.deep$date_time, format = "%Y-%m-%d")

all.burrow.surf$sitedate=paste0(format(all.burrow.surf$date,"%m"),all.burrow.surf$site)
all.burrow.mid$sitedate=paste0(format(all.burrow.mid$date,"%m"),all.burrow.mid$site)
all.burrow.deep$sitedate=paste0(format(all.burrow.deep$date,"%m"),all.burrow.deep$site)

# get monthly max values 
monthly.max.burrow.surf=aggregate(all.burrow.surf,by=list(all.burrow.surf$sitedate), FUN=max)
monthly.max.burrow.mid=aggregate(all.burrow.mid,by=list(all.burrow.mid$sitedate), FUN=max)
monthly.max.burrow.deep=aggregate(all.burrow.deep,by=list(all.burrow.deep$sitedate), FUN=max)

# combine to one dataset
monthly.max.burrow.surf$depth="surf"
monthly.max.burrow.mid$depth="mid"
monthly.max.burrow.deep$depth="deep"

monthly.max=rbind(monthly.max.burrow.surf,monthly.max.burrow.mid,monthly.max.burrow.deep)
monthly.max$month=format(monthly.max$date_time,"%m")

model = aov(humidity ~ depth + Error(month / depth), monthly.max)
summary(model)

max.summary = as.data.frame(aggregate(monthly.max$humidity, by=list(monthly.max$month, monthly.max$depth), FUN="mean"))
colnames(max.summary)=c("month", "depth", "humidity")

months=unique(max.summary$month)

par(mfrow = c(3,4))

for(i in 1:length(months)){
bar(dv = humidity, 
    factors = c(depth), 
    dataframe = subset(max.summary, max.summary$month==months[i]),
  main = months[i],
  ylim=c(0, 150))
}

max.table=cbind(seq(1,12),max.summary$humidity[1:12],max.summary$humidity[13:24],max.summary$humidity[25:36])
colnames(max.table)=c("month","deep", "mid", "surf")

# get monthly min values 
monthly.min.burrow.surf=aggregate(all.burrow.surf,by=list(all.burrow.surf$sitedate), FUN=min)
monthly.min.burrow.mid=aggregate(all.burrow.mid,by=list(all.burrow.mid$sitedate), FUN=min)
monthly.min.burrow.deep=aggregate(all.burrow.deep,by=list(all.burrow.deep$sitedate), FUN=min)

# combine to one dataset
monthly.min.burrow.surf$depth="surf"
monthly.min.burrow.mid$depth="mid"
monthly.min.burrow.deep$depth="deep"
monthly.min=rbind(monthly.min.burrow.surf,monthly.min.burrow.mid,monthly.min.burrow.deep)
monthly.min$month=format(monthly.min$date_time,"%m")

# run anovas
model = aov(humidity ~ depth + Error(month / (depth)), monthly.min)
summary(model)

min.summary = as.data.frame(aggregate(monthly.min$humidity, by=list(monthly.min$month, monthly.min$depth), FUN="mean"))
colnames(min.summary)=c("month", "depth", "humidity")

months=unique(min.summary$month)

par(mfrow = c(3,4))

for(i in 1:length(months)){
bar(dv = humidity, 
    factors = c(depth), 
    dataframe = subset(min.summary, min.summary$month==months[i]),
  main = months[i],
  ylim=c(0, 150))
}

# save group means
min.table=cbind(seq(1,12),min.summary$humidity[1:12],min.summary$humidity[13:24],min.summary$humidity[25:36])
colnames(min.table)=c("month","deep", "mid", "surf")
write.csv(min.table,paste0(dropbox,"burrow chapter final scripts/summary stats/burrow_humids_min.csv"))
write.csv(max.table,paste0(dropbox,"burrow chapter final scripts/summary stats/burrow_humids_max.csv"))
