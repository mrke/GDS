options(stringsAsFactors = FALSE)
loc <- "Nyrripi, Northern Territory, Australia" # type in a location here, used if option 1 is chosen above
longlat <- dismo::geocode(loc)[3:4] # assumes first geocode match is correct
if(nrow(longlat>1)){longlat<-longlat[1,]}
x <- t(as.matrix(as.numeric(c(longlat[1,1],longlat[1,2]))))
nyears=1

scenarios=c("Access 1.3", "Access 1.0", "CanESM2","GDFLCM3", "HadGEM2-CC", "HadGEM2-ES")
years=c(2050,2070)




# diff spline function
getdiff<-function(diffs,grid){
  diff1<-(unlist(diffs[1])+unlist(diffs[12]))/2
  
  # generate list of days
  for(ys in 1:nyears){
    day<-c(1,15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5, 365)
    day.leap<-c(1,15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5, 366)
    if(ys==1){
      days2=day
      days=day
    }else{
      days2=c(days2,(day+365*(ys-1)))
      days=c(days,day)
    }
  }
  
  if(is.na(diffs[1])==TRUE){
    # find the nearest cell with data
    NArem<-grid[[1]]
    NArem<-Which(!is.na(NArem), cells=TRUE)
    dist<-distanceFromPoints(maxTst05[[1]],x)
    distNA<-extract(dist,NArem)
    cellsR<-cbind(distNA,NArem)
    distmin<-which.min(distNA)
    cellrep<-cellsR[distmin,2]
    diffs<-extract(maxTst05,cellrep)
    diff1<-(unlist(diffs[1])+unlist(diffs[12]))/2
  }
  diffs3=rep(c(diff1,diffs,diff1),nyears)
  days_diffs<-data.frame(matrix(NA, nrow = nyears*14, ncol = 3))
  days_diffs[,1]<-days
  days_diffs[,3]<-days2
  days_diffs[,2]<-diffs3
  colnames(days_diffs)<-c("days","diffs","new_day")
  
  # interpolate monthly differences
  f<-approxfun(x=days_diffs$new_day, y=days_diffs$diffs)
  xx<-seq(1,max(days2),1)
  sp_diff<-f(xx)
  return(sp_diff)
}

for(i in 1:length(scenarios)){
  for(j in 1:length(years)){
    scenario=scenarios[i]
    year=years[j]
    
    ########### Max and Min Air Temps ################
    
    load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","maxTst05_",scenario,"_",year,".Rda",sep="")) #maxTst05
    
    diffs<-extract(maxTst05,x)
    TMAXX_diff<-getdiff(diffs,maxTst05)
    
    load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","minTst05_",scenario,"_",year,".Rda",sep="")) #minTst05
    
    diffs<-extract(minTst05,x)
    TMINN_diff<-getdiff(diffs,minTst05)
    
#     ################ RH ############################
#     
#     load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","RHst05_",scenario,"_",year,".Rda",sep="")) #maxTst05
#     
#     diffs<-extract(RHst05,x)
#     RH_diff<-getdiff(diffs,RHst05)
#     
#     ################ wind ############################
#     
#     load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","PT_VELst05_",scenario,"_",year,".Rda",sep=""))
#     
#     diffs<-extract(PT_VELst05,x)
#     WIND_diff<-getdiff(diffs,PT_VELst05)
#     
#     ############# SOLAR/CLOUD COVER ##################
#     
#     load(file = paste("c:/Spatial_Data/Australia Climate Change/",scenario,"/","SOLCst05_",scenario,"_",year,".Rda",sep=""))
#     
#     diffs<-extract(SOLCst05,x)
#     SOLAR_diff<-getdiff(diffs,SOLCst05)
    
    results<-as.data.frame(cbind(seq(1,365), year, round(TMAXX_diff,1),round(TMINN_diff,1)) )
    results<-cbind(results,scenario)
    if(i==1 & j==1){
      allresults<-results
    }else{
      allresults<-rbind(allresults,results)
    }
  }
}
colnames(allresults)<-c("day","year","dTmax","dTmin","scenario")
par(mfrow=c(3,2))
for(j in 1:2){
for(i in 1:length(scenarios)){
plot(dTmax~day,type='l',data=subset(allresults,year==years[j] & scenario==scenarios[i]),main=paste(scenarios[i],years[j]),ylim=c(0,6),col='red', ylab = expression(Delta~(degree*C)))
points(dTmin~day,type='l',data=subset(allresults,year==years[j] & scenario==scenarios[i]),main=paste(scenarios[i],years[j]),ylim=c(0,6),col='blue')
}
}
write.csv(allresults,"c:/git/GDS/climate_change_deltaTemp.csv")
