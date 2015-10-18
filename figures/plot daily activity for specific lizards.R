# script to plot GDS Tbs against available temperatures

# lizard to do
lizard<-3
# time period to plot
timestart<-"2013-10-22 00:00:00"
timefinish<-"2013-11-19 00:00:00"

GDS.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/GDS body temps/"
burrow.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/burrow data/"
soil.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/Soil profiles/"
adult.folder<-"C:/Users/Danaes Documents/aUNI/Research Masters/Data/dataloggers data/Adult models/"

#  [1] "01-Jan-14 1044_data-logger_S_B22.txt " S - 23/10 to 11/11 female  
#  [2] "01-Jan-14 1049_data-logger_A_B13.txt"  A – 10/10 to 9/11 Female
#  [3] "01-Jan-14 1053_data-logger_X_B56.txt" X – 22/10 to 19/11 Male 
#  [4] "01-Jan-14 1057_datalogger_K_B21.txt"  10th Oct to 17th Oct
#  [5] "01-Jan-14 1100_data-logger_D_B44.txt"  D – 14/10 to 19/10 Female 
#  [6] "01-Jan-14 1103_datalogger_Q_B46.txt"   14th to 27th October 
#  [7] "01-Jan-14 1104_data-logger_B_B40.txt" B – 10/10 to 6/11 great one Female  
#  [8] "01-Jan-14 1111_datalogger_B36_woma.txt" 23/10 to 18/11
#  [9] "09-Dec-13 0941_data-logger_I.txt"   9/10 to 7/11    
# [10] "10-Dec-13 1421_data-logger_J.txt"   23/10 to 9/11    
# [11] "10-Dec-13 1424_data-logger_C.txt"    10/10 to 7/11  
# [12] "11-Dec-13 1152_data-logger_P_B32.txt" 14/10 to 9/11 
# [13] "11-Dec-13 1217_data-logger_H B30.txt"  25/10 to 16/11 
# [14] "13-Dec-13 0550_data-logger_Z_B10.txt"  14th to 22th October
# [15] "13-Nov-13 0548_data-logger_Z_B47.txt" Z – 19/10 to 11/11 cat killed night of 10th. Female 
# [16] "14-Dec-13 1133_data-loggers_G_B28.txt" 10/10 to 6/11 
# [17] "15-Dec-13 0615_data-logger_M_B41.txt"  14th to 18th October? 
# [18] "21-Apr-14 1202_B_Backup.txt" B – 10/10 to 6/11??          
# [19] "30-Nov-13 1020_data-logger_L.txt"  17/10 to 26/11     
# [20] "31-Dec-13 1741_data-logger_E_B45.txt"  •  10/10 to 2/11
# [21] "31-Dec-13 1744_data-logger_T_B81.txt" 26/10 to 17/12
timestarts<-c("2013-10-23 00:00:00","2013-10-10 00:00:00","2013-10-22 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-14 00:00:00","2013-10-10 00:00:00","2013-10-23 00:00:00","2013-10-09 00:00:00","2013-10-23 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-25 00:00:00","2013-10-14 00:00:00","2013-10-19 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-10 00:00:00","2013-10-17 00:00:00","2013-10-10 00:00:00","2013-10-26 00:00:00")
timefinishs<-c("2013-11-11 00:00:00","2013-11-09 00:00:00","2013-11-19 00:00:00","2013-10-17 00:00:00","2013-10-19 00:00:00","2013-10-27 00:00:00","2013-11-06 00:00:00","2013-11-18 00:00:00","2013-11-07 00:00:00","2013-11-09 00:00:00","2013-11-07 00:00:00","2013-11-19 00:00:00","2013-11-16 00:00:00","2013-10-22 00:00:00","2013-11-11 00:00:00","2013-11-06 00:00:00","2013-10-18 00:00:00","2013-11-06 00:00:00","2013-11-26 00:00:00","2013-11-02 00:00:00","2013-12-17 00:00:00")

for(lizard in 1:21){
source('activity.R')
timestart<-timestarts(lizard)
timefinish<-timefinishs(lizard)
GDS.title<-activity(lizard,timestart,timefinish,GDS.folder,burrow.folder,soil.folder,adult.folder)
}
################ read corrected data and assign states ##################

# read in new, corrected time budget
time_budget<-read.csv(paste("time budget/",GDS.title,"_timebudget_corrected.csv",sep=""))[,-1]
underground<-c("s.15cm","s.30cm","s.50cm","s.1m")
aboveground<-c("c.sun","c.part","c.shd")
entrance<-c("s.5cm","b.surf")

time_budget$transit<-0
time_budget$state<-"inact.burrow"
time_budget$timeday<-"night"
solmin<-20
solmax<-150
time_budget$timeday[time_budget$solar>solmin & time_budget$solar<=solmax]<-"duskdawn"
time_budget$timeday[time_budget$solar>solmax]<-"day"
#plot(h.data.$solar)
#abline(solmin,0)
#abline(solmax,0)

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

###################### analyses #############################

transitions<-subset(time_budget,transit==1)
hist(x=time_budget$Tb,breaks=seq(20,40,0.5))
hist(x=transitions$Tb,breaks=seq(20,40,0.5),add=TRUE,col=2)

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
