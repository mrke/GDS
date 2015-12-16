# script to plot GDS Tbs against available temperatures

# lizard to do
lizard<-3
# time period to plot
timestart<-"2013-10-22 00:00:00"
timefinish<-"2013-11-19 00:00:00"

"C:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike"
GDS.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/GDS body temps/"
burrow.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/burrow data/"
soil.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/Soil profiles/"
adult.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/Adult models/"

#GDS.folder<-"C:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/GDS body temps/"
#burrow.folder<-"C:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/burrow data/"
#soil.folder<-"C:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Soil profiles/"
#adult.folder<-"C:/NicheMapR_Working/projects/GDS/Danae's datalogger data for Mike/Adult models/"

#  [1] "01-Jan-14 1044_data-logger_S_B22.txt " female S - 23/10 to 11/11 female  
#  [2] "01-Jan-14 1049_data-logger_A_B13.txt"  female A – 10/10 to 9/11 Female
#  [3] "01-Jan-14 1053_data-logger_X_B56.txt" male X – 22/10 to 19/11 Male 
#  [4] "01-Jan-14 1057_datalogger_K_B21.txt" female 10th Oct to 17th Oct
#  [5] "01-Jan-14 1100_data-logger_D_B44.txt" female D – 14/10 to 19/10 Female 
#  [6] "01-Jan-14 1103_datalogger_Q_B46.txt"  female 14th to 27th October 
#  [7] "01-Jan-14 1104_data-logger_B_B40.txt" female B – 10/10 to 6/11 great one Female  
#  [8] "01-Jan-14 1111_datalogger_B36_woma.txt" female 23/10 to 18/11
#  [9] "09-Dec-13 0941_data-logger_I.txt"   female 9/10 to 7/11    
# [10] "10-Dec-13 1421_data-logger_J.txt"   pregnant female 23/10 to 9/11    
# [11] "10-Dec-13 1424_data-logger_C.txt"    pregnant female 10/10 to 7/11  
# [12] "11-Dec-13 1152_data-logger_P_B32.txt" pregnant female 14/10 to 9/11 
# [13] "11-Dec-13 1217_data-logger_H B30.txt" pregnant female  25/10 to 16/11 
# [14] "13-Dec-13 0550_data-logger_Z_B10.txt" male 14th to 22th October
# [15] "13-Nov-13 0548_data-logger_Z_B47.txt" female Z – 19/10 to 11/11 cat killed night of 10th. Female 
# [16] "14-Dec-13 1133_data-loggers_G_B28.txt" female 10/10 to 6/11 
# [17] "15-Dec-13 0615_data-logger_M_B41.txt"  pregnant female, 14th to 18th October? 
# [18] "21-Apr-14 1202_B_Backup.txt" female B – 10/10 to 6/11??          
# [19] "30-Nov-13 1020_data-logger_L.txt" male 17/10 to 26/11     
# [20] "31-Dec-13 1741_data-logger_E_B45.txt" female  •  10/10 to 2/11
# [21] "31-Dec-13 1744_data-logger_T_B81.txt" pregant female, 26/10 to 17/12
tzone<-paste("Etc/GMT-10",sep="") # doing it this way ignores daylight savings!
timestarts<-c("2013-10-23 00:00:00","2013-10-10 00:00:00","2013-10-22 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-14 00:00:00","2013-10-10 00:00:00","2013-10-23 00:00:00","2013-10-09 00:00:00","2013-10-23 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-25 00:00:00","2013-10-14 00:00:00","2013-10-19 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-10 00:00:00","2013-10-17 00:00:00","2013-10-10 00:00:00","2013-10-26 00:00:00")
timefinishs<-c("2013-11-11 00:00:00","2013-11-09 00:00:00","2013-11-19 00:00:00","2013-10-17 00:00:00","2013-10-19 00:00:00","2013-10-27 00:00:00","2013-11-06 00:00:00","2013-11-18 00:00:00","2013-11-07 00:00:00","2013-11-09 00:00:00","2013-11-07 00:00:00","2013-11-19 00:00:00","2013-11-16 00:00:00","2013-10-22 00:00:00","2013-11-11 00:00:00","2013-11-06 00:00:00","2013-10-18 00:00:00","2013-11-06 00:00:00","2013-11-26 00:00:00","2013-11-02 00:00:00","2013-12-17 00:00:00")
sex<-c('f','f','m','f','f','f','f','f','f','f','f','f','f','m','f','f','f','f','m','f','f')
gravid<-c('np','np','np','np','np','np','np','np','np','p','p','p','p','np','np','np','p','np','np','np','p')
animals<-c('S','A','X','K','D','Q','B','B36','I','J','C','P','H','Z-B10','Z','G','M','B_backup','L','E','T')
# for(lizard in 1:21){
# source('activity.R')
# timestart<-timestarts[lizard]
# timefinish<-timefinishs[lizard]
# GDS.title<-activity(lizard,timestart,timefinish,GDS.folder,burrow.folder,soil.folder,adult.folder)
# }
################ read corrected data and assign states ##################

timebudget.files<-list.files('time budgets/')
timebudget.files<-timebudget.files[grep(timebudget.files,pattern = "timebudget_corrected.csv")]

# read in new, corrected time budget
#time_budget<-read.csv(paste("time budget/",GDS.title,"_timebudget_corrected.csv",sep=""))[,-1]

underground<-c("s.15cm","s.30cm","s.50cm","s.1m")
aboveground<-c("c.sun","c.part","c.shd")
entrance<-c("s.5cm","b.surf")

for(k in 1:length(timebudget.files)){
time_budget<-read.csv(paste("time budgets/",timebudget.files[k],sep=""))[,-1]

time_budget$transit<-0
time_budget$state<-"inact.burrow"
time_budget$timeday<-"night"
solmin<-10
solmax<-60
time_budget$timeday[time_budget$solar>solmin & time_budget$solar<=solmax]<-"duskdawn"
time_budget$timeday[time_budget$solar>solmax]<-"day"
plot(time_budget$pred_solar)
abline(solmin,0)
abline(solmax,0)

# states: inact.burrow = inactive under, act.burrow = active under, entrance = burrow surface & 5cm, surface = active surface)
for(i in 1:nrow(time_budget)){
  if(i>1){
    if(time_budget[i-1,18]!=time_budget[i,18]){
      time_budget[i,19]<-1
    }
    if(time_budget[i,19]==1){
      if(time_budget[i,18]%in%underground & time_budget[i-1,18]%in%underground){
        time_budget[i,20]<-"act.burrow"
      }
    }
    if(time_budget[i,18]%in%entrance){
      time_budget[i,20]<-"entrance"
    }
    if(time_budget[i,18]%in%aboveground){
      time_budget[i,20]<-"surface"
    }
    }
  }
 time_budget$sex<-sex[k]
 time_budget$gravid<-gravid[k]
 time_budget$N<-k
 time_budget$animalID<-animals[k]
 if(k==1){
   all_time_budgets<-time_budget
 }else{
   all_time_budgets<-rbind(all_time_budgets,time_budget)
 }
}
###################### analyses #############################
time_budget<-subset(all_time_budgets,animalID!='B_backup' & animalID != 'B36' & animalID != 'L') # get rid of problem animals
time_budget$sexgrav<-paste(time_budget$sex,time_budget$gravid,sep="") # make a new column of repro and sex together
summary(lm(Tb ~ sexgrav, data=time_budget))

# mixed model ANOVAs with sex/pregnant as fixed factors and individual as random factor
# males, females and gravid females
summary(aov(Tb ~ gravid + Error(animalID), data=time_budget))
# multi-level regression (see http://mindingthebrain.blogspot.com.au/2014/02/three-ways-to-get-parameter-specific-p.html for how I got the p-value)
library(lme4)
lm<-lmer(Tb ~ 1 + gravid + (1 | animalID), data=time_budget)
coefs <- data.frame(coef(summary(lm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# females only - effect of being gravid
summary(aov(Tb ~ sexgrav + Error(animalID), data=subset(time_budget,sex!='m')))
# nongravid females and males
summary(aov(Tb ~ sexgrav + Error(animalID), data=subset(time_budget,(sex=='m' | gravid=='np'))))
# gravid females and males
summary(aov(Tb ~ sexgrav + Error(animalID), data=subset(time_budget,(sex=='m' | gravid=='p'))))

# summarise mean Tb per individual and redo simple ANOVA

agg_Tbs<-as.data.frame(aggregate(time_budget$Tb,by=list(time_budget$animalID),FUN=median, na.rm=TRUE))
colnames(agg_Tbs)<-c("animals","Tb")
states<-cbind(animals,sex,gravid)
states<-states[order(animals),]
states<-subset(states,animals!='B_backup' & animals != 'B36' & animals != 'E' & animals != 'L' & animals != 'T')
agg_Tbs<-cbind(agg_Tbs,states[,2:3])
agg_Tbs$sexgrav<-paste(agg_Tbs$sex,agg_Tbs$gravid,sep="") # make a new column of repro and sex together
summary(aov(Tb ~ sexgrav,data=agg_Tbs))
summary(aov(Tb ~ gravid,data=agg_Tbs))

time_budget_gravid<-subset(all_time_budgets,gravid=='p')
time_budget_nongravid<-subset(all_time_budgets,gravid=='np' | sex=='f')
time_budget_males<-subset(all_time_budgets,sex=='m')

time_budget<-all_time_budgets

# histograms across all times of day
brks<-seq(0,80,0.5)
tpref_upper<-35
tpref_lower<-34
hist(x=time_budget$Tb,breaks=brks,col='NA',border="NA",main="all data",ylim=c(0,3000))
hist(x=time_budget$c.shd,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$c.sun,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$c.part,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$s.5cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.15cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.30cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.50cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.1m,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_nongravid$Tb,breaks=brks,col='pink',border="NA",add=TRUE)
hist(x=time_budget_gravid$Tb,breaks=brks,add=TRUE,border="NA",col=2)
hist(x=time_budget_males$Tb,breaks=brks,add=TRUE,border="NA",col='blue')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')

# histograms for daytime
time_budget_day<-subset(time_budget,timeday=='day')
time_budget_day_gravid<-subset(time_budget_day,gravid=='p')
time_budget_day_nongravid<-subset(time_budget_day,gravid=='np' | sex=='f')
time_budget_day_males<-subset(time_budget_day,sex=='m')
hist(x=time_budget_day_nongravid$Tb,breaks=brks,col='NA',border="NA",main="day",ylim=c(0,3000))
hist(x=time_budget_day$c.shd,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_day$c.sun,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_day$c.part,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_day$s.5cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_day$s.15cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_day$s.30cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_day$s.50cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_day$s.1m,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_day_nongravid$Tb,breaks=brks,col='pink',border="NA",add=TRUE)
hist(x=time_budget_day_gravid$Tb,breaks=brks,add=TRUE,border="NA",col=2)
hist(x=time_budget_day_males$Tb,breaks=brks,add=TRUE,border="NA",col='blue')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')

# histogram for night time
time_budget_night<-subset(time_budget,timeday=='night')
time_budget_night_gravid<-subset(time_budget_night,gravid=='p')
time_budget_night_nongravid<-subset(time_budget_night,gravid=='np' | sex=='f')
time_budget_night_males<-subset(time_budget_night,sex=='m')
hist(x=time_budget_night_nongravid$Tb,breaks=brks,col='NA',border="NA",main="night",ylim=c(0,3000))
hist(x=time_budget_night$c.shd,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_night$c.sun,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_night$c.part,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_night$s.5cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_night$s.15cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_night$s.30cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_night$s.50cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_night$s.1m,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_night_nongravid$Tb,breaks=brks,col='pink',border="NA",add=TRUE)
hist(x=time_budget_night_gravid$Tb,breaks=brks,add=TRUE,border="NA",col=2)
hist(x=time_budget_night_males$Tb,breaks=brks,add=TRUE,border="NA",col='blue')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')

# histogram dusk/dawn
time_budget_duskdawn<-subset(time_budget,timeday=='duskdawn')
time_budget_duskdawn_gravid<-subset(time_budget_duskdawn,gravid=='p')
time_budget_duskdawn_nongravid<-subset(time_budget_duskdawn,gravid=='np' | sex=='f')
time_budget_duskdawn_males<-subset(time_budget_duskdawn,sex=='m')
hist(x=time_budget_duskdawn_nongravid$Tb,breaks=brks,col='NA',border="NA",main="dusk/dawn",ylim=c(0,300))
hist(x=time_budget_duskdawn$c.shd,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_duskdawn$c.sun,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_duskdawn$c.part,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget_duskdawn$s.5cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_duskdawn$s.15cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_duskdawn$s.30cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_duskdawn$s.50cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_duskdawn$s.1m,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget_duskdawn_nongravid$Tb,breaks=brks,col='pink',border="NA",add=TRUE)
hist(x=time_budget_duskdawn_gravid$Tb,breaks=brks,add=TRUE,border="NA",col=2)
hist(x=time_budget_duskdawn_males$Tb,breaks=brks,add=TRUE,border="NA",col='blue')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')


plot(c.sun~hour,data=time_budget,cex=0.5,pch=16,main='nonpregnant females, all data')
points(c.shd~hour,data=time_budget,cex=0.5,pch=16)
points(c.part~hour,data=time_budget,cex=0.5,pch=16)
points(s.5cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.15cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.30cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.50cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.1m~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(Tb~hour,data=subset(time_budget,sex=='f', gravid='np'),col='pink',cex=0.5)
abline(tpref_lower,0,col='orange',lty=2,lwd=2)
abline(tpref_upper,0,col='orange',lty=2,lwd=2)

plot(c.sun~hour,data=time_budget,cex=0.5,pch=16,main="males, all data")
points(c.shd~hour,data=time_budget,cex=0.5,pch=16)
points(c.part~hour,data=time_budget,cex=0.5,pch=16)
points(s.5cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.15cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.30cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.50cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.1m~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(Tb~hour,data=subset(time_budget,sex=='m', gravid='np'),col='blue',cex=0.5)
abline(tpref_lower,0,col='orange',lty=2,lwd=2)
abline(tpref_upper,0,col='orange',lty=2,lwd=2)

plot(c.sun~hour,data=time_budget,cex=0.5,pch=16,main='pregnant females, all data')
points(c.shd~hour,data=time_budget,cex=0.5,pch=16)
points(c.part~hour,data=time_budget,cex=0.5,pch=16)
points(s.5cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.15cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.30cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.50cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.1m~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(Tb~hour,data=subset(time_budget,sex=='f', gravid='p'),col='red',cex=0.5)
abline(tpref_lower,0,col='orange',lty=2,lwd=2)
abline(tpref_upper,0,col='orange',lty=2,lwd=2)

# plot pie charts
par(mfrow=c(1,3))
# summary tables and bar plots
day<-subset(time_budget,timeday=='day')
counts <- table(day$state)
props <- prop.table(counts)
pie(props,main="day") 

day<-subset(time_budget,timeday=='night')
counts <- table(day$state)
props <- prop.table(counts)
pie(props,main="night")

day<-subset(time_budget,timeday=='duskdawn')
counts <- table(day$state)
props <- prop.table(counts)
pie(props,main="duskdawn")
par(mfrow=c(1,1))

# statistical test for independence between state and time of day
counts <- table(time_budget$state, time_budget$timeday)
props <- prop.table(counts,1)
barplot(props, main="GDS activity states",
  xlab="proportions", col=c("blue","black","orange","brown"),
  legend = rownames(props)) 
summary(counts) # chi-square test of indepedence

# subset data to explore where statistically significant differences lie
sub<-subset(time_budget,timeday!='night')
#sub<-subset(time_budget,timeday=='day' | timeday=='duskdawn')
counts <- table(sub$state, sub$timeday)
props <- prop.table(counts,1)
barplot(props, main="GDS activity states",
  xlab="proportions", col=c("blue","black","orange","brown"),
  legend = rownames(props)) 
summary(counts) # chi-square test of indepedence















# 2-Way Frequency Table 
attach(time_budget)
mytable <- table(state,timeday) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages 

mytable <- xtabs(~state+timeday, data=time_budget)
ftable(mytable) # print table 
summary(mytable) # chi-square test of indepedence

library(ca)
mytable <- with(time_budget, table(state,timeday)) # create a 2 way table
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
print(fit) # basic results 
summary(fit) # extended results 
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
   "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
