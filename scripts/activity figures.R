# script to plot GDS Tbs against available temperatures

# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"

#  [1] "01-Jan-14 1044_data-logger_S_B22.txt " female S - 23/10 to 11/11 female
#  [2] "01-Jan-14 1049_data-logger_A_B13.txt"  female A - 10/10 to 9/11 Female
#  [3] "01-Jan-14 1053_data-logger_X_B56.txt" male X - 22/10 to 19/11 Male
#  [4] "01-Jan-14 1057_datalogger_K_B21.txt" female 10th Oct to 17th Oct
#  [5] "01-Jan-14 1100_data-logger_D_B44.txt" female D - 14/10 to 19/10 Female
#  [6] "01-Jan-14 1103_datalogger_Q_B46.txt"  female 14th to 27th October
#  [7] "01-Jan-14 1104_data-logger_B_B40.txt" female B - 10/10 to 6/11 great one Female
#  [8] "01-Jan-14 1111_datalogger_B36_woma.txt" female 23/10 to 18/11
#  [9] "09-Dec-13 0941_data-logger_I.txt"   female 9/10 to 7/11
# [10] "10-Dec-13 1421_data-logger_J.txt"   pregnant female 23/10 to 9/11
# [11] "10-Dec-13 1424_data-logger_C.txt"    pregnant female 10/10 to 7/11
# [12] "11-Dec-13 1152_data-logger_P_B32.txt" pregnant female 14/10 to 9/11
# [13] "11-Dec-13 1217_data-logger_H B30.txt" pregnant female  25/10 to 16/11
# [14] "13-Dec-13 0550_data-logger_Z_B10.txt" male 14th to 22th October
# [15] "13-Nov-13 0548_data-logger_Z_B47.txt" female Z - 19/10 to 11/11 cat killed night of 10th. Female
# [16] "14-Dec-13 1133_data-loggers_G_B28.txt" female 10/10 to 6/11
# [17] "15-Dec-13 0615_data-logger_M_B41.txt"  pregnant female, 14th to 18th October?
# [18] "21-Apr-14 1202_B_Backup.txt" female B - 10/10 to 6/11??
# [19] "30-Nov-13 1020_data-logger_L.txt" male 17/10 to 26/11
# [20] "31-Dec-13 1741_data-logger_E_B45.txt" female  -  10/10 to 2/11
# [21] "31-Dec-13 1744_data-logger_T_B81.txt" pregant female, 26/10 to 17/12

time_budget=read.csv(paste0(dropbox,"csv summaries/time_budgets.csv"))[,-1]
tzone=paste0("Etc/GMT-10") # doing it this way ignores daylight savings!
time_budget$date=as.POSIXct(time_budget$days.i.,format="%d/%m/%Y",tz=tzone)
time_budget$date_time=time_budget$date+time_budget$hour*60

time_budget_gravid<-subset(time_budget,gravid=='p')
time_budget_nongravid<-subset(time_budget,gravid=='np' & sex=='f')
time_budget_males<-subset(time_budget,sex=='m')

tpref_upper<-quantile(time_budget$Tb,0.75,na.rm=TRUE)
tpref_lower<-quantile(time_budget$Tb,0.25,na.rm=TRUE)
maxTb=max(time_budget$Tb,na.rm=TRUE)
minTb=min(time_budget$Tb,na.rm=TRUE)

# histograms

# all Tb
brks<-seq(20,40,0.5)
hist(x=time_budget$Tb,breaks=brks,col='grey',border="NA")
points(rbind(c(maxTb,0),c(maxTb,10000)),type='l',lty=2,col='red')
points(rbind(c(minTb,0),c(minTb,10000)),type='l',lty=2,col='blue')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')

# foraging Tb
brks<-seq(20,40,0.5)
time_budget_act<-subset(time_budget,state=='surface')
hist(x=time_budget_act$Tb,breaks=brks,col='grey',border="NA")
points(rbind(c(maxTb,0),c(maxTb,10000)),type='l',lty=2,col='red')
points(rbind(c(minTb,0),c(minTb,10000)),type='l',lty=2,col='blue')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')

# histograms across all times of day
brks<-seq(0,80,0.5)
hist(x=time_budget$Tb,breaks=brks,col='NA',border="NA",main="all data",ylim=c(0,3000))
hist(x=time_budget$c.shd,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$c.sun,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$c.part,breaks=brks,col='black',border="NA",add=TRUE)
hist(x=time_budget$s.5cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.15cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.30cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.50cm,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$s.1m,breaks=brks,col='grey',border="NA",add=TRUE)
hist(x=time_budget$Tb,breaks=brks,col='orange',border="NA",add=TRUE)
hist(x=time_budget_nongravid$Tb,breaks=brks,col='pink',border="NA",add=TRUE)
hist(x=time_budget_gravid$Tb,breaks=brks,add=TRUE,border="NA",col=2)
hist(x=time_budget_males$Tb,breaks=brks,add=TRUE,border="NA",col='blue')
points(rbind(c(maxTb,0),c(maxTb,10000)),type='l',lty=2,col='red')
points(rbind(c(minTb,0),c(minTb,10000)),type='l',lty=2,col='blue')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')

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
points(rbind(c(maxTb,0),c(maxTb,10000)),type='l',lty=2,col='red')
points(rbind(c(minTb,0),c(minTb,10000)),type='l',lty=2,col='blue')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')

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
points(rbind(c(maxTb,0),c(maxTb,10000)),type='l',lty=2,col='red')
points(rbind(c(minTb,0),c(minTb,10000)),type='l',lty=2,col='blue')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')

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
points(rbind(c(maxTb,0),c(maxTb,10000)),type='l',lty=2,col='red')
points(rbind(c(minTb,0),c(minTb,10000)),type='l',lty=2,col='blue')
points(rbind(c(tpref_upper,0),c(tpref_upper,10000)),type='l',lty=2,col='orange')
points(rbind(c(tpref_lower,0),c(tpref_lower,10000)),type='l',lty=2,col='orange')

# hourly summaries

plot(c.sun~hour,data=time_budget,cex=0.5,pch=16,main='all data')
points(c.shd~hour,data=time_budget,cex=0.5,pch=16)
points(c.part~hour,data=time_budget,cex=0.5,pch=16)
points(s.5cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.15cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.30cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.50cm~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(s.1m~hour,data=time_budget,cex=0.5,pch=16,col='grey')
points(Tb~hour,data=time_budget,col='orange',cex=0.5)
abline(tpref_lower,0,col='red',lty=2,lwd=2)
abline(tpref_upper,0,col='red',lty=2,lwd=2)

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

# bar charts
counts <- table(time_budget$state, time_budget$timeday)
props <- prop.table(counts, margin = 2)
barplot(props, main="activity budget",
  xlab="", col=seq(1,6), legend = rownames(props)) 

counts <- table(time_budget$state, time_budget$timeday)
order=c(3,4,5,1,2,6)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,4]),][,-4]
barplot(props, main="activity budget",
  xlab="", col=c("black", "dark grey", "red", "orange", "blue", "light blue"), legend = rownames(props), 
  args.legend = list(x = "topright", bty = "n", inset=c(0.01, .2))) 
barplot(props, main="activity budget",
  xlab="", col=c("black", "dark grey", "red", "orange", "blue", "light blue"), legend = FALSE) 


barplot(props, ylim = c(0,1.5), cex.lab = 1.3, cex = 1.3, cex.axis =1.3, tck=0.01, yaxs = "i", ylab = "proportion of time", main="activity budget",
  xlab="", col=c("black", "dark grey", "red", "orange", "blue", "light blue"), legend = c("inactive deep","inactive shallow", "active deep",  "active shallow" , "entrance", "surface"), 
  args.legend = list(x = "topright",cex=1.3,pt.cex = 1, bty = "n", inset=c(0.01, .005)) )
box()

par(mfrow=c(1,2))

warm <- 0

potential.act <- time_budget[,c(1, 2, 15, 16, 17, 21, 6, 7, 8)]
potential.act$c.sun[potential.act$c.sun+warm>maxTb]<-0 # zero too hot
potential.act$c.sun[potential.act$c.sun+warm<minTb]<-0 # zero too cold
potential.act$c.sun[potential.act$c.sun!=0]<-1 # 1 for just right
potential.act$c.shd[potential.act$c.sun==1]<-0 # zero suitable in open
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm>maxTb]<-0 # zero too hot in shade
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm<minTb]<-0 # zero too cold in shade
potential.act$c.shd[potential.act$c.shd!=0]<-1 # 1 for just right in shade
potential.act$b.surf[potential.act$c.sun==1 | potential.act$c.shd==1]<-0 # zero suitable on surface
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm<minTb]<-0 # zero too cold shallow
potential.act$b.surf[potential.act$b.surf!=0]<-1 # 1 for just right shallow
potential.act$b.deep[potential.act$c.sun==1 | potential.act$c.shd==1 | potential.act$b.surf==1]<-0 # zero suitable on surface or shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm<minTb]<-0 # zero too cold shallow
potential.act$b.deep[potential.act$b.deep!=0]<-1 # 1 for just right shallow
potential.act$state <- "inactive.deep"
potential.act$state[potential.act$c.sun==1]<-"active.open"
potential.act$state[potential.act$c.shd==1]<-"active.shade"
potential.act$state[potential.act$b.surf==1]<-"active.shallow"


counts <- table(potential.act$state, potential.act$timeday)
order=c(4, 3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,4]),][,-4]


barplot(props, ylim = c(0,1.5), cex.lab = 1.3, cex = 1.3, cex.axis =1.3, tck=0.01, yaxs = "i", ylab = "proportion of time",
  xlab="", col=c("black", "orange", "blue", "light blue"), legend = c("inactive deep","active shallow", "active shade",  "active open"), 
  args.legend = list(x = "topright",cex=1.3,pt.cex = 1, bty = "n", inset=c(0.01, .005)) )
box()


warm <- 2.8

potential.act <- time_budget[,c(1, 2, 15, 16, 17, 21, 6, 7, 8)]
potential.act$c.sun[potential.act$c.sun+warm>maxTb]<-0 # zero too hot
potential.act$c.sun[potential.act$c.sun+warm<minTb]<-0 # zero too cold
potential.act$c.sun[potential.act$c.sun!=0]<-1 # 1 for just right
potential.act$c.shd[potential.act$c.sun==1]<-0 # zero suitable in open
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm>maxTb]<-0 # zero too hot in shade
potential.act$c.shd[potential.act$c.shd !=0 & potential.act$c.shd+warm<minTb]<-0 # zero too cold in shade
potential.act$c.shd[potential.act$c.shd!=0]<-1 # 1 for just right in shade
potential.act$b.surf[potential.act$c.sun==1 | potential.act$c.shd==1]<-0 # zero suitable on surface
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.surf[potential.act$b.surf !=0 & potential.act$b.surf+warm<minTb]<-0 # zero too cold shallow
potential.act$b.surf[potential.act$b.surf!=0]<-1 # 1 for just right shallow
potential.act$b.deep[potential.act$c.sun==1 | potential.act$c.shd==1 | potential.act$b.surf==1]<-0 # zero suitable on surface or shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm>maxTb]<-0 # zero too hot shallow
potential.act$b.deep[potential.act$b.deep !=0 & potential.act$b.deep+warm<minTb]<-0 # zero too cold shallow
potential.act$b.deep[potential.act$b.deep!=0]<-1 # 1 for just right shallow
potential.act$state <- "inactive.deep"
potential.act$state[potential.act$c.sun==1]<-"active.open"
potential.act$state[potential.act$c.shd==1]<-"active.shade"
potential.act$state[potential.act$b.surf==1]<-"active.shallow"


counts <- table(potential.act$state, potential.act$timeday)
order=c(4, 3, 2, 1)
props <- prop.table(counts, margin = 2)
props = cbind(props,order)
props = props[order(props[,4]),][,-4]


barplot(props, ylim = c(0,1.5), cex.lab = 1.3, cex = 1.3, cex.axis =1.3, tck=0.01, yaxs = "i", ylab = "proportion of time",
  xlab="", col=c("black", "orange", "blue", "light blue") )
box()