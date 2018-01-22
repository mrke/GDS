library(lattice)
library(lme4) # for lmer
library(lmerTest) # to get p values for the lmer anova table
library(arm) # to get the deviance from the lmer model, function display()
source("burrow chapter final scripts/bar.R")
# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"

############################# read in copper model data and format dates ###################################################

# read in data
all.adult.fullsuns=read.csv(paste0(dropbox,"csv summaries/all.adult.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.partshades=read.csv(paste0(dropbox,"csv summaries/all.adult.partshades.csv"),stringsAsFactors = FALSE)[,-1]
all.adult.fullshades=read.csv(paste0(dropbox,"csv summaries/all.adult.fullshades.csv"),stringsAsFactors = FALSE)[,-1]
all.juv.fullsuns=read.csv(paste0(dropbox,"csv summaries/all.juv.fullsuns.csv"),stringsAsFactors = FALSE)[,-1]
all.juv.partshades=read.csv(paste0(dropbox,"csv summaries/all.juv.partshades.csv"),stringsAsFactors = FALSE)[,-1]
all.juv.fullshades=read.csv(paste0(dropbox,"csv summaries/all.juv.fullshades.csv"),stringsAsFactors = FALSE)[,-1]

all.adult.fullsuns$date_time = as.POSIXct(all.adult.fullsuns$date_time, format = "%Y-%m-%d")
all.adult.partshades$date_time = as.POSIXct(all.adult.partshades$date_time, format = "%Y-%m-%d")
all.adult.fullshades$date_time = as.POSIXct(all.adult.fullshades$date_time, format = "%Y-%m-%d")
all.juv.fullsuns$date_time = as.POSIXct(all.juv.fullsuns$date_time, format = "%Y-%m-%d")
all.juv.partshades$date_time = as.POSIXct(all.juv.partshades$date_time, format = "%Y-%m-%d")
all.juv.fullshades$date_time = as.POSIXct(all.juv.fullshades$date_time, format = "%Y-%m-%d")

all.adult.fullsuns$sitedate=paste0(format(all.adult.fullsuns$date,"%m"),all.adult.fullsuns$site)
all.adult.partshades$sitedate=paste0(format(all.adult.partshades$date,"%m"),all.adult.partshades$site)
all.adult.fullshades$sitedate=paste0(format(all.adult.fullshades$date,"%m"),all.adult.fullshades$site)
all.juv.fullsuns$sitedate=paste0(format(all.juv.fullsuns$date,"%m"),all.juv.fullsuns$site)
all.juv.partshades$sitedate=paste0(format(all.juv.partshades$date,"%m"),all.juv.partshades$site)
all.juv.fullshades$sitedate=paste0(format(all.juv.fullshades$date,"%m"),all.juv.fullshades$site)

# get monthly max values 
monthly.max.adult.fullsuns=aggregate(all.adult.fullsuns,by=list(all.adult.fullsuns$sitedate), FUN=max)
monthly.max.adult.partshades=aggregate(all.adult.partshades,by=list(all.adult.partshades$sitedate), FUN=max)
monthly.max.adult.fullshades=aggregate(all.adult.fullshades,by=list(all.adult.fullshades$sitedate), FUN=max)
monthly.max.juv.fullsuns=aggregate(all.juv.fullsuns,by=list(all.juv.fullsuns$sitedate), FUN=max)
monthly.max.juv.partshades=aggregate(all.juv.partshades,by=list(all.juv.partshades$sitedate), FUN=max)
monthly.max.juv.fullshades=aggregate(all.juv.fullshades,by=list(all.juv.fullshades$sitedate), FUN=max)

# combine to one dataset
monthly.max.adult.fullsuns$size="large"
monthly.max.adult.partshades$size="large"
monthly.max.adult.fullshades$size="large"
monthly.max.juv.fullsuns$size="small"
monthly.max.juv.partshades$size="small"
monthly.max.juv.fullshades$size="small"

monthly.max.adult.fullsuns$shade="minimum"
monthly.max.adult.partshades$shade="medium"
monthly.max.adult.fullshades$shade="maximum"
monthly.max.juv.fullsuns$shade="minimum"
monthly.max.juv.partshades$shade="medium"
monthly.max.juv.fullshades$shade="maximum"

monthly.max=rbind(monthly.max.adult.fullsuns,monthly.max.adult.partshades,monthly.max.adult.fullshades,monthly.max.juv.fullsuns,monthly.max.juv.partshades,monthly.max.juv.fullshades)
monthly.max$month=format(monthly.max$date_time,"%m")

########################################################################
# run anovas

# just shade, aov (month as repeated measure), balanced
model = aov(temperature ~ shade + Error(month / shade), data = subset(monthly.max, 
  (month=="01" | month== "02" | month== "03" | month== "04" | month== "07"| month== "10"| month== "11")))
summary(model)

# just shade, lmer (month as random factor), balanced
model <- lmer(temperature ~ shade  + (1 | month), data = subset(monthly.max, 
  (month=="01" | month== "02" | month== "03" | month== "04" | month== "07"| month== "10"| month== "11")))
anova(model)

# shade * size, aov (month as repeated measure), balanced
model = aov(temperature ~ shade * size + Error(month / (shade * size)), data = subset(monthly.max, 
  (month=="01" | month== "02" | month== "03" | month== "04" | month== "07"| month== "10"| month== "11")))
summary(model)

# shade * size, lmer (month as random factor), balanced
model <- lmer(temperature ~ shade * size  + (1 | shade:month) + (1 | size:month), data = subset(monthly.max, 
  (month=="01" | month== "02" | month== "03" | month== "04" | month== "07"| month== "10"| month== "11")))
anova(model)

# shade * size, lmer (month as random factor), unbalanced, all months
model <- lmer(temperature ~ shade * size  + (1 | shade:month) + (1 | size:month), data = monthly.max)
anova(model1)

# shade * size, lmer (month as random factor), unbalanced, all months
model.sub <- lmer(temperature ~ shade * size  + (1 | shade:month) + (1 | size:month), data = monthly.max)
anova(model1)

# shade * size, lmer (month and site as random factors), unbalanced, all months
model.full <- lmer(temperature ~ shade * size  + (1 | shade:month) + (1 | size:month) + (1 | site), data = monthly.max)
anova(model2)

dev1<-display(model.full) # get deviance of full model
dev2<-display(model.sub) # get deviance of sub model
dev=dev1$deviance-dev2$deviance # get difference in deviance
1 - pchisq(dev,1) # compute P-value and print it out

###########################################################################



model_sun = aov(temperature ~ size + Error(month / (size)), subset(monthly.max, (month=="01" | month== "02" | month== "03" | month== "04" | month== "07"| month== "10"| month== "11") & shade=="minimum"))
summary(model_sun)

model_part = aov(temperature ~ size + Error(month / (size)), subset(monthly.max,(month=="01" | month== "02" | month== "03" | month== "04" | month== "07"| month== "10"| month== "11") &  shade=="medium"))
summary(model_part)

model_shade = aov(temperature ~ size + Error(month / (size)), subset(monthly.max,(month=="01" | month== "02" | month== "03" | month== "04" | month== "07"| month== "10"| month== "11") &  shade=="maximum"))
summary(model_shade)

max.summary = as.data.frame(aggregate(monthly.max$temperature, by=list(monthly.max$month, monthly.max$shade, monthly.max$size), FUN="mean"))
colnames(max.summary)=c("month", "shade", "size", "temperature")

months=unique(max.summary$month)

par(mfrow = c(3,4))

for(i in 1:length(months)){
bar(dv = temperature, 
    factors = c(size, shade), 
    dataframe = subset(max.summary, max.summary$month==months[i]),
  main = months[i],
  ylim=c(0, 95))
}

# save group means
max.table=cbind(seq(1,12),max.summary$temperature[1:12],max.summary$temperature[13:24],max.summary$temperature[25:36],max.summary$temperature[37:48],max.summary$temperature[49:60],max.summary$temperature[61:72])
colnames(max.table)=c("month","max.large", "med.large", "min.large", "max.small", "med.small", "min.small")
write.csv(max.table,paste0(dropbox,"burrow chapter final scripts/summary stats/operative_temps_max.csv"))

# get monthly min values 
monthly.min.adult.fullsuns=aggregate(all.adult.fullsuns,by=list(all.adult.fullsuns$sitedate), FUN=min)
monthly.min.adult.partshades=aggregate(all.adult.partshades,by=list(all.adult.partshades$sitedate), FUN=min)
monthly.min.adult.fullshades=aggregate(all.adult.fullshades,by=list(all.adult.fullshades$sitedate), FUN=min)
monthly.min.juv.fullsuns=aggregate(all.juv.fullsuns,by=list(all.juv.fullsuns$sitedate), FUN=min)
monthly.min.juv.partshades=aggregate(all.juv.partshades,by=list(all.juv.partshades$sitedate), FUN=min)
monthly.min.juv.fullshades=aggregate(all.juv.fullshades,by=list(all.juv.fullshades$sitedate), FUN=min)

# combine to one dataset
monthly.min.adult.fullsuns$size="large"
monthly.min.adult.partshades$size="large"
monthly.min.adult.fullshades$size="large"
monthly.min.juv.fullsuns$size="small"
monthly.min.juv.partshades$size="small"
monthly.min.juv.fullshades$size="small"

monthly.min.adult.fullsuns$shade="minimum"
monthly.min.adult.partshades$shade="medium"
monthly.min.adult.fullshades$shade="maximum"
monthly.min.juv.fullsuns$shade="minimum"
monthly.min.juv.partshades$shade="medium"
monthly.min.juv.fullshades$shade="maximum"

# run anovas
monthly.min=rbind(monthly.min.adult.fullsuns,monthly.min.adult.partshades,monthly.min.adult.fullshades,monthly.min.juv.fullsuns,monthly.min.juv.partshades,monthly.min.juv.fullshades)
monthly.min$month=format(monthly.min$date_time,"%m")

model = aov(temperature ~ shade * size + Error(month / (shade * size)), monthly.min)
summary(model)

model_sun = aov(temperature ~ size + Error(month / (size)), subset(monthly.min, shade=="minimum"))
summary(model_sun)

model_part = aov(temperature ~ size + Error(month / (size)), subset(monthly.min, shade=="medium"))
summary(model_part)

model_shade = aov(temperature ~ size + Error(month / (size)), subset(monthly.min, shade=="maximum"))
summary(model_shade)

min.summary = as.data.frame(aggregate(monthly.min$temperature, by=list(monthly.min$month, monthly.min$shade, monthly.min$size), FUN="mean"))
colnames(min.summary)=c("month", "shade", "size", "temperature")

months=unique(min.summary$month)

for(i in 1:length(months)){
bar(dv = temperature, 
    factors = c(size, shade), 
    dataframe = subset(min.summary, min.summary$month==months[i]),
  main = months[i],
  ylim=c(-5, 25))
}

# save group means
min.table=cbind(seq(1,12),min.summary$temperature[1:12],min.summary$temperature[13:24],min.summary$temperature[25:36],min.summary$temperature[37:48],min.summary$temperature[49:60],min.summary$temperature[61:72])
colnames(min.table)=c("month","max.large", "med.large", "min.large", "max.small", "med.small", "min.small")
write.csv(min.table,paste0(dropbox,"burrow chapter final scripts/summary stats/operative_temps_min.csv"))
