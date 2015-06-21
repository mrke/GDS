############# ectotherm model parameters ################################

microin<-"/git/Niveoscincus/microclimates/Orford 2000_2013/" # subfolder containing the microclimate input data
#microin<-"/git/Niveoscincus/microclimates/Central Plateau 2000_2013/" # subfolder containing the microclimate input data
project.dir<-getwd() # save initial working directory
# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism) or dead animal?
enberr<-0.0002 # tolerance for energy balance
timeinterval<-365 # number of time intervals in a year
ystart<-read.csv(paste(microin,'ectoin.csv',sep=""))[7,2]
yfinish<-read.csv(paste(microin,'ectoin.csv',sep=""))[8,2]
nyears<-ceiling(nrow(read.csv(paste(microin,'rainfall.csv',sep="")))/365) # number of years the simulation runs for (work out from input data)
write_input<-0 # write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv(paste(microin,'ectoin.csv',sep=""))[3,2],read.csv(paste(microin,'ectoin.csv',sep=""))[4,2])
grasshade<-0 # use grass shade values from microclimate model as min shade values (1) or not (0)? (simulates effect of grass growth on shading, as a function of soil moisture)

# habitat settings
FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
soilnode<-7. # soil node at which eggs are laid (overridden if frogbreed is 1)
minshade<-0. # minimum available shade (percent)
maxshade<-80. # maximum available shade (percent)
REFL<-rep(0.18,timeinterval*nyears) # substrate reflectances 

# morphological traits
rinsul<-0. # m, insulative fat layer thickness
# 'lometry' determines whether standard or custom shapes/surface area/volume relationships are used.
# 0=plate,1=cyl,2=ellips,3=lizard (desert iguana),4=frog (leopard frog),
# 5=custom (cylinder geometry is automatically invoked when container model operates)
lometry<-3 # organism shape (see above)
# 'custallom' below operates if lometry=5, and consists of 4 pairs of values representing 
# the parameters a and b of a relationship AREA=a*mass^b, where AREA is in cm2 and mass is in g.
# The first pair are a and b for total surface area, then a and b for ventral area, then for  
# sillhouette area normal to the sun, then sillhouette area perpendicular to the sun
customallom<-c(10.4713,.688,0.425,0.85,3.798,.683,0.694,.743) # custom allometry coefficients (see above)
shape_a<-1. 
shape_b<-3
shape_c<-0.6666666667
Flshcond<-0.5 # W/mC, thermal conductivity of flesh (range: 0.412-2.8 )
Spheat<-4185 # J/(kg-K), specific heat of flesh
Andens<-1000 # kg/m3, density of flesh
ABSMAX<-0.9 # ** decimal %, maximum solar absorptivity (Christian, K.A., Bedford, G.S. & Shannahan, S.T. (1996) Solar absorptance of some Australian lizards and its relationship to temperature. Australian Journal of Zoology, 44.)
ABSMIN<-0.9 # ** decimal %, maximum solar absorptivity (Christian, K.A., Bedford, G.S. & Shannahan, S.T. (1996) Solar absorptance of some Australian lizards and its relationship to temperature. Australian Journal of Zoology, 44.)
EMISAN<-1. # emissivity of animal
ptcond<-0.1 # decimal % of surface contacting the substrate
FATOSK<-0.4 # configuration factor to sky
FATOSB<-0.4 # configuration factor to substrate

# wing model, for butterflies
wings<-0 # wing model off (0) or on (1)
rho1_3<-0.2 # decimal %, wing reflectance
trans1<-0.00 # decimal %, wing transmissivity
aref<-0.26 # cm, width of surface #2 (back or horizontal or reference surface)
bref<-2.04 # cm, common length where the two rectangles join
cref<-1.47 # cm, width of surface #1 (wing)
phi<-179. # degrees, initial wing angle (90 = vertical relative to body)
phimax<- phi # degrees, max wing angle (90 = vertical relative to body)
phimin<- phi # degrees, min wing angle (90 = vertical relative to body

# physiological traits
TMAXPR<-34.2 # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection) # based on Yuni's field data for lowland site across all seasons
TMINPR<-29.1 # degrees C, voluntary thermal minimum (lower body temperature for foraging) # based on Yuni's field data for lowland site across all seasons
TBASK<-TMINPR#5 # degrees C, minimum basking temperature 
TEMERGE<-10 # degrees C, temperature at which animal will move to a basking site, educated guess
ctmax<-41.78  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
ctmin<-4.27 # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing) # Mandy's data
ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
ctkill<-0 #if 1, animal dies when it hits critical thermal limits
TPREF<-32.74 # preferred body temperature (animal will attempt to regulate as close to this value as possible) # mean of Yuni's Orford data across all seasons
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-0.0 # estimated from data in Bently 1959 at 23 degrees and 34.5 degrees #0.2#0.35 # %, of surface area acting like a free water surface (e.g. most frogs are 100% wet, many lizards less than 5% wet)
extref<-20. # %, oxygen extraction efficiency (need to check, but based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)
PFEWAT<-73. # %, fecal water (from Shine's thesis on Egernia cunninghami, mixed diet 75% clover, 25% mealworms)
PTUREA<-0. # %, water in excreted nitrogenous waste
FoodWater<-82#82 # 82%, water content of food (from Shine's thesis on Egernia cunninghami, clover)
minwater<-15 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
raindrink<-0. # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available)
gutfill<-101 # % gut fill at which satiation occurs - if greater than 100%, animal always tries to forage

# behavioural traits
dayact<-1 # diurnal activity allowed (1) or not (0)?
nocturn<-0 # nocturnal activity allowed (1) or not (0)?
crepus<-0 # crepuscular activity allowed (1) or not (0)?
burrow<-1 # shelter in burrow allowed (1) or not (0)?
shdburrow<-0 #
mindepth<-3 # minimum depth (soil node) to which animal can retreat if burrowing
maxdepth<-10 # maximum depth (soil node) to which animal can retreat if burrowing
CkGrShad<-1 # shade seeking allowed (1) or not (0)?
climb<-0 # climbing to seek cooler habitats allowed (1) or not (0)?
fosorial<-0 # fossorial activity (1) or not (0)
rainact<-0 # activity is limited by rainfall (1) or not (0)?
actrainthresh<-0.1 # threshold mm of rain causing activity if rainact=1
breedactthresh<-1 # threshold numbers of hours active after start of breeding season before eggs can be laid (simulating movement to the breeding site)
flyer<-0 # does the animal fly?
flyspeed<-5 # flying speed, m/s
flymetab<-0.1035 # flight metabolic excess, w/g

# containter simulation settings
container<-0 # run the container model? (aquatic start of life cycle, e.g. frog or mosquito)
conth<-10 # cylindrical container/pond height (cm)
contw<-100. # cylindrical container/pond diameter (cm)
contype<-1 # is 'containter' sitting on the surface, like a bucket (0) or sunk into the ground like a pond (1)
rainmult<-1 # rainfall multiplier to reflect catchment (don't make this zero unless you want a drought!)
continit<-0 # initial container water level (cm)
conthole<- 0#2.8 # daily loss of height (mm) due to 'hole' in container (e.g. infiltration to soil, drawdown from water tank)
contonly<-1 # just run the container model and quit?
contwet<-80 # percent wet value for container
wetmod<-0 # run the wetland model?
soilmoisture<-0 # run the soil moisture model? (models near-surface soil moisture rather than a pond as a function of field capacity and wilting point)

# which energy budget model to use? 
DEB<-1 # run the DEB model (1) or just heat balance, using allometric respiration below (0)

# parameters for allometric model of respiration, for use in heat budget when DEB model is not
# run so that metabolic heat generation and respiratory water loss can be calculated.
# Metabolic rate, MR (ml O2/h, STP) at a given body mass (g) and body temperature, Tb (deg C)
# MR=MR1*M^MR2*10^(MR3*Tb) based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231
amass<-4. # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
MR_1<-0.013
MR_2<-0.8
MR_3<-0.038

################### Dynamic Energy Budget Model Parameters ################
debpars<-as.data.frame(read.csv('DEB models/Niveoscincus_ocellatus_lowland/DEB_pars_Niveoscincus_ocellatus_lowlands.csv',header=FALSE))$V1
fract<-1
f<-1.
MsM<-186.03*6. # J/cm3 produces a stomach volume of 5.3 cm3/100 g, as measured for Disosaurus dorsalis, adjusted for Egernia cunninghami
z<-debpars[8]*fract
delta<-debpars[9]
kappa_X<-debpars[11]#0.85
v_dotref<-debpars[13]/24.
kappa<-debpars[14]
p_Mref<-debpars[16]/24.
E_G<-debpars[19]
k_R<-debpars[15]
k_J<-debpars[18]/24.
E_Hb<-debpars[20]*fract^3
E_Hj<-E_Hb*fract^3
E_Hp<-debpars[21]*fract^3
h_aref<-debpars[22]/(24.^2) #3.61e-11/(24.^2) 
s_G<-debpars[23]

E_Egg<-debpars[24]*fract^4# J, initial energy of one egg # this includes the residual yolk, which is eaten upon hatching
E_m<-(p_Mref*z/kappa)/v_dotref
p_Xm<-13290#12420 # J/h.cm2, maximum intake rate when feeding
K<-0.01 # half-saturation constant
X<-10 # food density J/cm2, approximation based on 200 Tetragonia berries per 1m2 (Dubasd and Bull 1990) assuming energy content of Lilly Pilly (http://www.sgapqld.org.au/bush_food_safety.pdf)

# for insect model
metab_mode<-0 # 0 = off, 1 = hemimetabolus model (to do), 2 = holometabolous model
stages<-8 # number of stages (max = 8) = number of instars plus 1 for egg + 1 for pupa + 1 for imago
y_EV_l<-debpars[19] # mol/mol, yield of imago reserve on larval structure
S_instar<-c(rep(debpars[17],4)) # -, stress at instar n: L_n^2/ L_n-1^2
s_j<-debpars[18] # -, reprod buffer/structure at pupation as fraction of max

# these next five parameters control the thermal response, effectively generating a thermal response curve
T_REF<-debpars[1]-273 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-debpars[2]
TAL<-debpars[5]
TAH<-debpars[6]
TL<-debpars[3]
TH<-debpars[4]

# life-stage specific parameters, we are making these all the same for all life stages so no need to worry about this section

arrhenius<-matrix(data = 0, nrow = 8, ncol = 5)
arrhenius[,1]<-TA # critical thermal minimum
arrhenius[,2]<-TAL # critical thermal maximum
arrhenius[,3]<-TAH # voluntary thermal minimum
arrhenius[,4]<-TL # voluntary thermal maximum
arrhenius[,5]<-TH # basking threshold 

thermal_stages<-matrix(data = 0, nrow = 8, ncol = 6)
thermal_stages[,1]<-ctmin # critical thermal minimum
thermal_stages[,2]<-ctmax # critical thermal maximum
thermal_stages[,3]<-TMINPR # voluntary thermal minimum
thermal_stages[,4]<-TMAXPR # voluntary thermal maximum
thermal_stages[,5]<-TBASK # basking threshold
thermal_stages[,6]<-TPREF # preferred body temperature

behav_stages<-matrix(data = 0, nrow = 8, ncol = 14)

behav_stages[,1]<-dayact
behav_stages[,2]<-nocturn
behav_stages[,3]<-crepus
behav_stages[,4]<-burrow
behav_stages[,5]<-shdburrow
behav_stages[,6]<-mindepth
behav_stages[,7]<-maxdepth
behav_stages[,8]<-CkGrShad
behav_stages[,9]<-climb
behav_stages[,10]<-fosorial
behav_stages[,11]<-rainact
behav_stages[,12]<-actrainthresh
behav_stages[,13]<-breedactthresh
behav_stages[,14]<-flyer

water_stages<-matrix(data = 0, nrow = 8, ncol = 8)

water_stages[,1]<-skinwet
water_stages[,2]<-extref
water_stages[,3]<-PFEWAT
water_stages[,4]<-PTUREA
water_stages[,5]<-FoodWater
water_stages[,6]<-minwater
water_stages[,7]<-raindrink
water_stages[,8]<-gutfill

# composition related parameters
andens_deb<-1. # g/cm3, density of structure 
d_V<-0.3 # density of structure (reflects fraction of mass that is dry)
d_E<-0.3 # density of reserve (reflects fraction of mass that is dry)
eggdryfrac<-0.3 # decimal percent, dry mass of eggs
mu_X<-525000 # J/cmol, chemical potential of food
mu_E<-585000 # J/cmol, chemical potential of reserve
mu_V<-500000 # J/cmol, chemical potential of structure 
mu_P<-480000 # J/cmol, chemical potential of product (faeces)
kappa_X_P<-0.1 # fraction of food energy into faeces
nX<-c(1,1.8,0.5,.15) # composition of food (atoms per carbon atoms for CHON)
nE<-c(1,1.8,0.5,.15) # composition of reserve (atoms per carbon atoms for CHON)
nV<-c(1,1.8,0.5,.15) # composition of structure (atoms per carbon atoms for CHON)
nP<-c(1,1.8,0.5,.15) # composition of product/faeces (atoms per carbon atoms for CHON)
N_waste<-c(1,4/5,3/5,4/5) # chemical formula for nitrogenous waste product, CHON, e.g. Urea c(0,3,0,1), Uric acid c(5/5,4,3,4)

# breeding life history
clutchsize<-5. # clutch size, if using regression below, make this the max clutch size
clutch_ab<-c(2.5,12.5) # paramters for relationship between length and clutch size: clutch size = a*SVL-b, make zero if fixed clutch size
viviparous<-1 # 1=yes, 0=no
batch<-1 # invoke Pequerie et al.'s batch laying model?

# the following four parameters apply if batch = 1, i.e. animal mobilizes
breedrainthresh<-0 # rain dependent breeder? 0 means no, otherwise enter rainfall threshold in mm
# photoperiod response triggering ovulation, none (0), summer solstice (1), autumnal equinox (2),  
# winter solstice (3), vernal equinox (4), specified daylength thresholds (5)
photostart<- 4 # photoperiod initiating breeding, 4 means that vitellogenesis doesn't start until 22nd Sept, but happens quickly, and model assumes that dev can start once activity is possible so gestation is probably starting a little early 
photofinish<- 1 # photoperiod terminating breeding
daylengthstart<- 12.5 # threshold daylength for initiating breeding
daylengthfinish<- 12.5 # threshold daylength for terminating breeding
photodirs <- 1 # is the start daylength trigger during a decrease (0) or increase (1) in day length?
photodirf <- 0 # is the finish daylength trigger during a decrease (0) or increase (1) in day length?
startday<-1 # make it 90 for T. rugosa loop day of year at which DEB model starts
breedtempthresh<-200 # body temperature threshold below which breeding will occur
breedtempcum<-24*7 # cumulative time below temperature threshold for breeding that will trigger breeding

reset<-0 # reset options, 0=quit simulation upon death, 1=restart at emergence, 2=restart at first egg laid, 3=restart at end of breeding season, 4=reset at death

# frog breeding mode 0 is off, 
# 1 is exotrophic aquatic (eggs start when water present in container and within breeding season)
# 2 is exotrophic terrestrial/aquatic (eggs start at specified soil node within breeding season, 
# diapause at birth threshold, start larval phase if water present in container)
# 3 endotrophic terrestrial (eggs start at specified soil node within breeding season and continue
# to metamorphosis on land)
# 4 turtle mode (eggs start at specified soil node within breeding season, hatch and animals enter
# water and stay there for the rest of their life, but leave the water if no water is present)
frogbreed<-0 # frog breeding mode
frogstage<-0 # 0 is whole life cycle, 1 is just to metamorphosis (then reset and start again)

# metabolic depression
aestivate<-0
depress<-0.3

# DEB model initial conditions
v_init<-3e-9
E_init<-E_Egg/v_init
E_H_init<-0
stage<-0
v_init<-(debpars[25]^3)*fract^3 #hatchling
E_init<-E_m
E_H_init<-E_Hb+5
stage<-1
# v_init<-(debpars[26]^3)*fract^3*0.85
# E_init<-E_m
# E_H_init<-E_Hp+1
# stage<-3

# mortality rates
ma<-1e-4  # hourly active mortality rate (probability of mortality per hour)
mi<-0  # hourly inactive mortality rate (probability of mortality per hour)
mh<-0.5   # survivorship of hatchling in first year
wilting<-1
ystrt<-0

setwd("/git/ectotherm/")
#set up call to NicheMapR function
niche<-list(y_EV_l=y_EV_l,S_instar=S_instar,s_j=s_j,clutch_ab=clutch_ab,wilting=wilting,ystrt=ystrt,soilmoisture=soilmoisture,write_input=write_input,minshade=minshade,maxshade=maxshade,REFL=REFL,nyears=nyears,enberr=enberr,FLTYPE=FLTYPE,SUBTK=SUBTK,soilnode=soilnode,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,extref=extref,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,fosorial=fosorial,rainact=rainact,actrainthresh=actrainthresh,container=container,conth=conth,contw=contw,rainmult=rainmult,andens_deb=andens_deb,d_V=d_V,d_E=d_E,eggdryfrac=eggdryfrac,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,kappa_X_P=kappa_X_P,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,nX=nX,nE=nE,nV=nV,nP=nP,N_waste=N_waste,T_REF=T_REF,TA=TA,TAL=TAL,TAH=TAH,TL=TL,TH=TH,z=z,kappa=kappa,kappa_X=kappa_X,p_Mref=p_Mref,v_dotref=v_dotref,E_G=E_G,k_R=k_R,MsM=MsM,delta=delta,h_aref=h_aref,viviparous=viviparous,k_J=k_J,E_Hb=E_Hb,E_Hj=E_Hj,E_Hp=E_Hp,frogbreed=frogbreed,frogstage=frogstage,clutchsize=clutchsize,v_init=v_init,E_init=E_init,E_H_init=E_H_init,batch=batch,breedrainthresh=breedrainthresh,daylengthstart=daylengthstart,daylenghtfinish=daylengthfinish,photodirs=photodirs,photodirf=photodirf,photostart=photostart,photofinish=photofinish,amass=amass,customallom=customallom,E_Egg=E_Egg,PTUREA=PTUREA,PFEWAT=PFEWAT,FoodWater=FoodWater,DEB=DEB,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,f=f,minwater=minwater,s_G=s_G,K=K,X=X,flyer=flyer,flyspeed=flyspeed,maxdepth=maxdepth,mindepth=mindepth,ctminthresh=ctminthresh,ctkill=ctkill,metab_mode=metab_mode,stages=stages,arrhenius=arrhenius,startday=startday,raindrink=raindrink,reset=reset,gutfill=gutfill,TBASK=TBASK,TEMERGE=TEMERGE,p_Xm=p_Xm,flymetab=flymetab,live=live,continit=continit,wetmod=wetmod,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,stage=stage,ma=ma,mi=mi,mh=mh,aestivate=aestivate,depress=depress,contype=contype,rainmult=rainmult,conthole=conthole,contonly=contonly,contwet=contwet,microin=microin,mac=mac,grasshade=grasshade)
source('NicheMapR_Setup_ecto.R')
nicheout<-NicheMapR_ecto(niche)
setwd(project.dir)

# retrieve output
metout<-as.data.frame(nicheout$metout)[1:(nyears*365*24),]
shadmet<-as.data.frame(nicheout$shadmet)[1:(nyears*365*24),]
soil<-as.data.frame(nicheout$soil)[1:(nyears*365*24),]
shadsoil<-as.data.frame(nicheout$shadsoil)[1:(nyears*365*24),]
rainfall<-as.data.frame(nicheout$RAINFALL)
grassgrowths<-as.data.frame(nicheout$grassgrowths)
grasstsdms<-as.data.frame(nicheout$grasstsdms)
environ<-as.data.frame(nicheout$environ[1:(365*24*nyears),])
enbal<-as.data.frame(nicheout$enbal[1:(365*24*nyears),])
masbal<-as.data.frame(nicheout$masbal[1:(365*24*nyears),])

yearout<-as.data.frame(nicheout$yearout)
if(nyears>1){
  yearsout<-as.data.frame(nicheout$yearsout[1:nyears,])
}else{
  yearsout<-t(as.data.frame(nicheout$yearsout))
}
if(container==1){
  wetlandTemps<-as.data.frame(environ$WATERTEMP)
  wetlandDepths<-as.data.frame(environ$CONDEP)
}

# append dates
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
if(DEB==1){
  debout<-as.data.frame(nicheout$debout[1:(365*24*nyears),])
  debout<-cbind(dates,debout)
}
environ<-cbind(dates,environ)
masbal<-cbind(dates,masbal)
enbal<-cbind(dates,enbal)
soil<-cbind(dates,soil)
metout<-cbind(dates,metout)
shadsoil<-cbind(dates,shadsoil)
shadmet<-cbind(dates,shadmet)

dates2<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
grass<-cbind(dates2,grassgrowths,grasstsdms)
colnames(grass)<-c("dates","growth","tsdm")
rainfall<-as.data.frame(cbind(dates2,rainfall))
colnames(rainfall)<-c("dates","rainfall")


############### plot results ######################
library(lattice)

# first plot observed vs predicted SVL vs. time
plot(debout$SVL~debout$dates,type='l',ylim=c(20,75)) # predicted SVL vs time

Growth<-read.csv('life history/N_ocellatus_Orford_growth.csv') # read in first dataset
Growth$date<-as.POSIXct(Growth$date,format='%d/%m/%Y') # convert dates
points(Growth$SVL~Growth$date,col='red') # plot results

Growth_skeleto<-read.csv('life history/N_ocellatus_Orford_growth_skeleto.csv') # bring in second dataset
Growth_skeleto<-subset(Growth_skeleto,site=='Orford' & sex!='m') # choose site and sex
#Growth_skeleto<-subset(Growth_skeleto,site=='CP' & sex!='m') # choose site and sex
Growth_skeleto$date<-as.POSIXct(Growth_skeleto$date,format='%d/%m/%Y') # convert dates
points(Growth_skeleto$svl~Growth_skeleto$date,col='blue') # plot results

# plot activity windows
plotenviron<-subset(environ,YEAR<3) #choose time period
forage<-subset(plotenviron,ACT==2) # get foraging times
bask<-subset(plotenviron,ACT==1) # get basking times
night<-subset(metout,ZEN==90) # get night period
with(forage,plot((TIME-1)~DAY,pch=15,cex=.5,col='blue',ylab='hour of day',ylim=c(0,23),xlab='day of year')) # plot foraging
with(bask,points((TIME-1)~DAY,pch=15,cex=.5,col='grey')) # plot basking
with(night,points(TIME/60~JULDAY,pch=15,cex=.5)) # plot night hours
actstart<-subset(plotenviron,as.character(plotenviron$dates,format='%d/%m')=="15/09" | as.character(plotenviron$dates,format='%d/%m')=="15/04") # get 15th Sept and 15th April
actstart<-subset(actstart,as.character(actstart$dates,format='%H')=="00") # get midnight
actstart<-subset(actstart,JULDAY>0) # get rid of years after death
abline(v=actstart$DAY,col='grey',lty=2) # plot 15th September and 15th April for reference

# plot mass and reproduction phenology
plotdebout<-subset(debout,as.numeric(format(dates, "%Y"))<1994) # use this if you want to subset particular years
plotdebout<-debout # this just keeps it to all years
year_vals<-subset(plotdebout,as.character(dates,format='%d/%m')=="01/01")
year_vals<-subset(year_vals,as.character(year_vals$dates,format='%H')=="00") # get midnight
plot(plotdebout$WETMASS~plotdebout$dates,type='l', ylab="wet mass, g",xlab="date") # plot wet mass as a function of date
abline(v=year_vals$dates,col='grey',lty=2) # add lines to show beginning of each year
plot(debout$CUMBATCH/1000~debout$dates,type='l', ylab='total energy, kJ',xlab="date") # plot energy in batch buffer (yolking eggs)
points(debout$CUMREPRO/1000~debout$dates,type='l',col='red') # plot energy in the reproduction buffer (captial for egg production, but not currently transformed to eggs)
abline(v=year_vals$dates,col='grey',lty=2) # add lines to show beginning of each year
plot(debout$V_baby~debout$dates,type='l', ylab='embryo structure (cm3)',xlab="date") # plot embryo development (volume of structure)

# plot thermoregulation
subdate<-subset(environ, format(environ$dates,"%y/%m")=="01/07") # use this to subset a particular year and month
#subdate<-environ # just use the whole data set
with(subdate, plot(TC~dates,ylim=c(-15,50),type = "l",col='blue')) # plot Tb
with(subdate, points(ACT*5~dates,type = "l",col='pink')) # plot activity
with(subdate, points(SHADE/10~dates,type = "l",col='dark green')) # plot shade use
with(plotenviron, points(DEP/10~dates,type = "l",col="brown")) # plot depth under ground
with(metout, points(TAREF~dates,type = "l",col="light blue")) # plot reference (1.5m) air temp
with(rainfall,points(rainfall~dates,type='h',col='purple'))
# plot thermal thresholds
abline(TMAXPR,0,lty=2,col='red',lwd=2)
abline(TMINPR,0,lty=2,col='blue',lwd=2)
abline(TBASK,0,lty=2,col='pink',lwd=2)
abline(ctmin,0,lty=2,col='cyan',lwd=2)
abline(TPREF,0,lty=2,col='orange',lwd=2)

# code to get constant temperature equivalent (CTE) based on Arrhenius temperature function
# !!!!!!!!!!!! run this with DEB<-0 on line 112 above so that DEB model isn't running !!!!!!!!

# reference temp and 5 parameters for the Arrhenius response curve
T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-10191 # Arrhenius temperture
TAL<-50000 # Arrhenius temperature at lower temperature threshold
TAH<-90000 # Arrhenius temperature at upper temperature threshold
TL<-273+10 # lower temperature threshold
TH<-273+37 # upper temperature threshold 

temps<-seq(0,50,.25) # dummy temps for demo plot of response curve
plot(as.numeric(exp(TA*(1/(273+T_REF)-1/(273+temps)))/(1+exp(TAL*(1/(273+temps)-1/TL))+exp(TAH*(1/TH-1/(273+temps)))))~temps,type='l',ylab='correction factor',xlab='body temperature deg C',main='5 parameter Arrhenius thermal response curve')

TC<-environ$TC # temps from simulation to be used to estimate CTE

TempCorr<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+TC)))/(1+exp(TAL*(1/(273+TC)-1/TL))+exp(TAH*(1/TH-1/(273+TC))))) # convert Tb each hour to temperature correction factor
TempCorr_mean<-mean(TempCorr) # get mean temperature correction factor
TempCorr_mean # report value to console
getTb<-function(Tb){ # function finding the difference between a temperature correction factor for a specified Tb compared to the mean calculated one (aim to make this zero)
      x<-exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))-TempCorr_mean
   }
CTE<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections
mean(TC) # report mean Tb to screen
CTE # report constant temperature equivalent to screen


