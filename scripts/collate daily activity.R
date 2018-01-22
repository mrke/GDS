# script to plot GDS Tbs against available temperatures

# dropbox path
dropbox="c:/Users/mrke/My Dropbox/Student Projects/gds/"

GDS.folder=paste0(dropbox,"raw data/GDS body temps/")
burrow.folder=paste0(dropbox,"raw data/burrow data/")
soil.folder=paste0(dropbox,"raw data/Soil profiles/")
adult.folder=paste0(dropbox,"raw data/Adult models/")
weather.folder=paste0(dropbox,"raw data/weather data/")
microclimate.folder=paste0(dropbox,"microclimate/Newhaven 2012_2014/")
timebudgets.folder=paste0(dropbox,"/time budgets/")

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

tzone=paste0("Etc/GMT-10") # doing it this way ignores daylight savings!
timestarts=c("2013-10-23 00:00:00","2013-10-10 00:00:00","2013-10-22 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-14 00:00:00","2013-10-10 00:00:00","2013-10-23 00:00:00","2013-10-09 00:00:00","2013-10-23 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-25 00:00:00","2013-10-14 00:00:00","2013-10-19 00:00:00","2013-10-10 00:00:00","2013-10-14 00:00:00","2013-10-10 00:00:00","2013-10-17 00:00:00","2013-10-10 00:00:00","2013-10-26 00:00:00")
timefinishs=c("2013-11-11 00:00:00","2013-11-09 00:00:00","2013-11-19 00:00:00","2013-10-17 00:00:00","2013-10-19 00:00:00","2013-10-27 00:00:00","2013-11-06 00:00:00","2013-11-18 00:00:00","2013-11-07 00:00:00","2013-11-09 00:00:00","2013-11-07 00:00:00","2013-11-19 00:00:00","2013-11-16 00:00:00","2013-10-22 00:00:00","2013-11-11 00:00:00","2013-11-06 00:00:00","2013-10-18 00:00:00","2013-11-06 00:00:00","2013-11-26 00:00:00","2013-11-02 00:00:00","2013-12-17 00:00:00")
sex=c('f','f','m','f','f','f','f','f','f','f','f','f','f','m','f','f','f','f','m','f','f')
gravid=c('np','np','np','np','np','np','np','np','np','p','p','p','p','np','np','np','p','np','np','np','p')
animals=c('S','A','X','K','D','Q','B','J','C','P','H','Z-B10','Z','G','M','L','E','T')
underground<-c("s.15cm","s.30cm","s.50cm","s.1m")
deep.underground<-c("s.50cm","s.1m")
shallow.underground<-c("s.15cm","s.30cm")
aboveground<-c("c.sun","c.part","c.shd")
entrance<-c("s.5cm","b.surf")

# for(lizard in 1:21){
# source('activity.R')
# timestart=timestarts[lizard]
# timefinish=timefinishs[lizard]
# GDS.title=activity(lizard,timestart,timefinish,GDS.folder,burrow.folder,soil.folder,adult.folder,weather.folder,microclimate.folder,timebudgets.folder)
# }
################ read corrected data and assign states ##################

timebudget.files=list.files(timebudgets.folder)
timebudget.files=timebudget.files[grep(timebudget.files,pattern = "timebudget_corrected.csv")]


for(k in 1:length(timebudget.files)){
time_budget=read.csv(paste0("time budgets/",timebudget.files[k]))[,-1]

time_budget$transit=0
time_budget$state="inact.burrow.deep"
time_budget$timeday="night"
solmin=10
solmax=60
time_budget$timeday[time_budget$solar>solmin & time_budget$solar<=solmax]="duskdawn"
time_budget$timeday[time_budget$solar>solmax]="day"
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
      if(time_budget[i,18]%in%underground & time_budget[i-1,18]%in%deep.underground){
        time_budget[i,20]<-"act.burrow.deep"
      }
    }
    if(time_budget[i,19]==0){
      if(time_budget[i,18]%in%underground & time_budget[i-1,18]%in%deep.underground){
        time_budget[i,20]<-"inact.burrow.deep"
      }
    }
    if(time_budget[i,19]==0){
      if(time_budget[i,18]%in%underground & time_budget[i-1,18]%in%shallow.underground){
        time_budget[i,20]<-"inact.burrow.shallow"
      }
    }    
    if(time_budget[i,19]==1){
      if(time_budget[i,18]%in%underground & time_budget[i-1,18]%in%shallow.underground){
        time_budget[i,20]<-"act.burrow.shallow"
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

write.csv(time_budget,paste0(dropbox,"csv summaries/time_budgets.csv"))
