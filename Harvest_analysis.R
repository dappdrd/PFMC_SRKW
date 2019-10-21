rm(list = ls())

library(xlsx)
library(ggplot2)
library(readxl)
library(dplyr)
library(doBy)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

inFile <- "C:\\Users\\dappdrd\\Desktop\\Main folder for stuff\\SRKW Workgroup\\Updated Harvest Analysis\\Harvest_analysis_data_file_with_PFMC_Harvest_Attempt_2.xlsx"
output <- "C:\\Users\\dappdrd\\Desktop\\Main folder for stuff\\SRKW Workgroup\\Updated Harvest Analysis\\Graphics\\"
RunIDDF <- read_excel(inFile,
                      sheet="RunIDs")
StockDF <- read_excel(inFile,
                      sheet="FRAM to Shelton Stks")
ColSprDF <- read_excel(inFile,
                       sheet="Up Col Spr Abundances")
CohortDF <- read_excel(inFile,
                       sheet="Cohort")
StockFRAMDF <- read_excel(inFile,
                          sheet="StockFRAM")
SheltonDistsDF <- read_excel(inFile,
                             sheet="Shelton Dists")
NatMortDF <- read_excel(inFile,
                        sheet="Natural Mortality")
SRFCRUNDF <- read_excel(inFile,
                        sheet="SRFC Run Dat")
SRFCHARVDF <- read_excel(inFile,
                         sheet="SRFC Harv Dat")
KRFCDF <- read_excel(inFile,
                     sheet="KRFC")
ROPIDF <- read_excel(inFile,
                     sheet="ROPI")
KRFCHarvDF <- read_excel(inFile,
                     sheet="KRFC Harvest")
ValiCohortDF <- read_excel(inFile,
                         sheet="Chinook cohorts - 6.2 Vali")
ValiMortDF <- read_excel(inFile,
                           sheet="Mortalities (6.2 Validation)")
NoPFMCMortDF <- read_excel(inFile,
                         sheet="Mortalities (0 PFMC Run)")
FecundityDF <- read_excel(inFile,
                          sheet="SRKW Fecundity")
AgeStageDF <- read_excel(inFile,
                         sheet="SRKW Ages 2 Stages")
PeanutsDF <- read_excel(inFile,
                        sheet="SRKW Peanuts")


#List of data frames
InputDataList <- list(StockFRAMDF, CohortDF, RunIDDF, ColSprDF, StockDF, SheltonDistsDF, NatMortDF,KRFCDF, ROPIDF, SRFCRUNDF, SRFCHARVDF, KRFCHarvDF, ValiCohortDF,
                      ValiMortDF, NoPFMCMortDF, FecundityDF, AgeStageDF, PeanutsDF)

# Identify range of years for analysis
minYr <- min(InputDataList[[3]]$RunYear)
maxYr <- max(InputDataList[[3]]$RunYear)

#Filter just time steps 1-3 and remove the primary key column
#Filter out age 2s as per technical discussion on 7/30/2019
cohort <- InputDataList[[2]][InputDataList[[2]]$TimeStep < 4 & InputDataList[[2]]$Age > 2, c(2:10)]

#Convert from 78 to 39 stock format
cohort$Stock <- ceiling(cohort$StockID/2)

#Get Stock names by 39 format StockID, merge into main data frame
InputDataList[[1]]$Stock <- ceiling(InputDataList[[1]]$StockID/2)
StkNms <- InputDataList[[1]] %>% distinct(Stock, .keep_all = TRUE)
#Admittedly, we do not need the StockLongName as this is just filtered out later as things are aggregated to the level of 
#Shelton stocks.  However, I included this line for my own testing purposes.
StkNms$StockLongName <- substring(StkNms$StockLongName, 7)
StkNms <- StkNms[c(7:8)]
StkNms <- merge (StkNms, InputDataList[[5]], by = "Stock")
cohort <- merge(cohort, StkNms, by= "Stock")

#Next, we need to translate RunID to a year
RunIDs <- InputDataList[[3]][c(2,11)]
cohort <- merge(cohort, RunIDs, by= "RunID")


# Combine Marked and Unmarked components of each stock
# Use the starting cohort for the time step as per technical discussion on 7/30/2019
cohort_Shelton_Stk <- summaryBy(StartCohort~RunYear+SheltonStk+Age+TimeStep, data = cohort, FUN = sum)

#Remove Central Valley, will add in later with Will's data
cohort_Shelton_Stk <- cohort_Shelton_Stk[cohort_Shelton_Stk$SheltonStk != "CENTV",]

# Spring Stock Handling ----

# Columbia River Spring Stocks
# For this, there are two issues.
# First, distributions are not available in the Shelton model for Spring stocks.  There are two different options to handle this.
# We could use the inside, outside, coastal measures from the Puget Sound SRKW framework.
# However, these distributions are purely based on CWTs recoveries (e.g., no scaling to differential fishery effort)
# and are not timestep-specific (e.g., annual distributions only).
# A second method was developed by Eric Ward (see email from EW on June 28th, 2019).
# This method assumes that 1.) spring stocks are concentrated in front of their river mouths in the spring more than they would be other
# times of the year, 2.) proportion of fall stock abundances directly adjacent to the river during the season of migration is approximately similar
# to the proportion of spring stock abundances directly adjacent to the river during the season of migration.
# The second method is what is used here; see input file, "Shelton Dists" tab.

# A second issue is that only Lower Columbia (Cowlitz, Willamette, Lewis, Kalama, Sandy) Springs are represented in FRAM.
# We will need to account for Upriver Columbia Springs in some way.
# To do this, we aggregated the terminal run size of Upriver Columbia Springs across stocks, by age
# This information came from the STT, and is used to produce tables B-12 and B-13 of
# the annual PFMC Review of OCean Salmon Fisheries Document: https://www.pcouncil.org/wp-content/uploads/2019/02/2018_Review_of_Ocean_Salmon_Fisheries_Final_021419.pdf
# We also used age compositions from Stuart Ellis (Upper; CRITFC).
# There were two potential ways that were discussed for modeling Upriver Columbia Springs - with or without natural mortality.
# It is thought that Upriver Columbia Springs have an offshore distribution

#subset the data to only the years of interest
UpColSprDF <- subset(InputDataList[[4]], RunYear >= minYr & RunYear <= maxYr)

for(i in 1:nrow(UpColSprDF)){
  UpColSprDF$StartCohort.sum[i] <- UpColSprDF$StartCohort.sum[i]/(1-subset(InputDataList[[7]], Age == UpColSprDF$Age[i] & TimeStep == UpColSprDF$TimeStep[i])$NaturalMortalityRate)
}

#South of Falcon Stock Handling ----

#This section was coded by Will Satterthwaite and was distributed on 8.5.2019

#An alternative approach to estimating SRFC adult ocean abundance
#Assume 20% annual natural mortality of adult SRFC in ocean
#Equates to 1.842347% monthly mortality
m=0.01842347
#further assume all harvest takes place on first day of month (so don't have to adjust harvest for mortality over course of uknown portion of month before it was harvested)
#this means August 1 abundance = river run size scaled up to account for one month's mortality, plus August harvest
#July 1 abundance = August 1 abundance scaled up to account for one month's mortallity, plus July harvest
#and so on...

#This is nat mortality from each time step
m1 = 1-(1-.2)^(7/12)
m2 = 1-(1-.2)^(2/12)
m3 = 1-(1-.2)^(3/12)

years=c(minYr:maxYr)
SOFabundances=array(NA,c(0,5))
colnames(SOFabundances)=c("RunYear","SheltonStk","Age","TimeStep","StartCohort.sum")

#calculate SFB (SRFC, from SI) abundances
SheltonStk="CENTV"
Age=3

SOFHarv <- data.frame(RunYear = as.integer(), SheltonStk = as.character(), Age = as.integer(), TimeStep = as.integer(), TotMort.sum = as.integer())

for (i in years)
{
  SRFC_river_run=InputDataList[[10]]$SRFC_river_run[InputDataList[[10]]$Year==i]
  annual.harv=InputDataList[[11]][InputDataList[[11]]$year==i,]
  monthses=c(8,7,6,5,4,3,2,1,12,11,10,9) #this is the order to step through months in this simplified cohort reconstruction
  monthly.abundance=array(NA,12)
  step=1 # first calculate August 1 abundance
  monthly.abundance[step]=SRFC_river_run/(1-m)+annual.harv$H[annual.harv$month==monthses[step]]
  for (step in 2:12) #now do remaining months
  {
    monthly.abundance[step]=monthly.abundance[step-1]/(1-m)+annual.harv$H[annual.harv$month==monthses[step]]
  }
  
  #injected this code... this should be the abundances with natural mortality removed only, e.g., in a no harvest scenario...
  oct.abund=monthly.abundance[12]* 1-(1-.2)^(1/12)
  may.abund=oct.abund * (1-m1)
  july.abund=may.abund * (1-m2)
  
  year=array(i,3)
  stock=array(SheltonStk,3)
  ages=array(Age,3)
  seasons=c(1:3) #seasons 1-3 correspond to abundances on Oct 1 (of calendar yr = mgmt yr-1), May 1, and July 1 respectively
  yearly.abunds=c(oct.abund,may.abund,july.abund)
  
  new.abundances=cbind(year,stock,ages,seasons,yearly.abunds)
  
  SOFabundances=rbind(SOFabundances,new.abundances)
  
  SRFCT1HarvRow <- data.frame(RunYear = i, SheltonStk = "CENTV", Age = 3, TimeStep = 1, TotMort.sum = annual.harv$H[annual.harv$month==10]+
                                annual.harv$H[annual.harv$month==11] + annual.harv$H[annual.harv$month==12] + annual.harv$H[annual.harv$month==1]+
                                annual.harv$H[annual.harv$month==2] + annual.harv$H[annual.harv$month==3] + annual.harv$H[annual.harv$month==4])
  SRFCT2HarvRow <- data.frame(RunYear = i, SheltonStk = "CENTV", Age = 3, TimeStep = 2, TotMort.sum = annual.harv$H[annual.harv$month==5]+
                                annual.harv$H[annual.harv$month==6])
  SRFCT3HarvRow <- data.frame(RunYear = i, SheltonStk = "CENTV", Age = 3, TimeStep = 3, TotMort.sum = annual.harv$H[annual.harv$month==7]+
                                annual.harv$H[annual.harv$month==8] + annual.harv$H[annual.harv$month==9])
  
  SOFHarv <- rbind(SOFHarv, SRFCT1HarvRow, SRFCT2HarvRow, SRFCT3HarvRow)
  
}

SheltonStk="NCA"

for (i in years)
{
  KRFC.dat.yr=InputDataList[[8]][InputDataList[[8]]$mgmtyr==i,]
  #do aggregations one age at a time within each year
  for (j in 3:5){
    
    KRFC.dat.aged=KRFC.dat.yr[KRFC.dat.yr$age==j,]

    KRFC.sep=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==9]
    
    if(j == 3){
      KRFC.oct=KRFC.sep * (1-.5)^(1/12)
      KRFC.may=KRFC.oct * (1-.5)^(7/12)
      KRFC.july = KRFC.may * (1-.2)^(2/12)
    } else{
      KRFC.oct=KRFC.sep * (1-.2)^(1/12)
      KRFC.may=KRFC.oct * (1-.2)^(7/12)
      KRFC.july = KRFC.may * (1-.2)^(2/12)
    }

    if(j == 3){
      ROPI.sep=InputDataList[[9]]$ROPI_3[InputDataList[[9]]$Year==i]
    } else if(j == 4){
      ROPI.sep=InputDataList[[9]]$ROPI_4[InputDataList[[9]]$Year==i]
    } else if(j == 5){
      ROPI.sep=InputDataList[[9]]$ROPI_5[InputDataList[[9]]$Year==i]
    }
    ROPI.oct=ROPI.sep*KRFC.oct/KRFC.sep
    ROPI.may=ROPI.sep*KRFC.may/KRFC.sep
    ROPI.july=ROPI.sep*KRFC.july/KRFC.sep
    oct.abund=KRFC.oct+ROPI.oct
    may.abund=KRFC.may+ROPI.may
    july.abund=KRFC.july+ROPI.july
    
    year=array(i,3)
    stock=array(SheltonStk,3)
    ages=array(j,3)
    seasons=c(1:3) #seasons 1-3 correspond to abundances on Oct 1 (of calendar yr = mgmt yr-1), May 1, and July 1 respectively
    yearly.abunds=c(oct.abund,may.abund,july.abund)
    
    new.abundances=cbind(year,stock,ages,seasons,yearly.abunds)
    
    SOFabundances=rbind(SOFabundances,new.abundances)
  }
}


#Next we have to calculate ROPI/KRFC harvest...


for (i in years)
{
  KRFC.dat.yr=InputDataList[[8]][InputDataList[[8]]$mgmtyr==i,]
  #do aggregations one age at a time within each year
  for (j in 3:5){
    #July/Oct/May are needed for time step starts
    #September is used for setting up ratios
    #Other months are needed to calculating ROPI harvests later (applying KRFC ERs to monthly abundance data)
    KRFC.dat.aged=KRFC.dat.yr[KRFC.dat.yr$age==j,]
    KRFC.sep=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==9]
    KRFC.oct=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==10]
    KRFC.may=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==5]
    KRFC.july=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==7]
    KRFC.jan=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==1]
    KRFC.feb=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==2]
    KRFC.mar=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==3]
    KRFC.apr=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==4]
    KRFC.june=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==6]
    KRFC.aug=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==8]
    KRFC.nov=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==11]
    KRFC.dec=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==12]

    if(j == 3){
      ROPI.sep=InputDataList[[9]]$ROPI_3[InputDataList[[9]]$Year==i]
    }
    else if(j == 4){
      ROPI.sep=InputDataList[[9]]$ROPI_4[InputDataList[[9]]$Year==i]
    }
    else if(j == 5){
      ROPI.sep=InputDataList[[9]]$ROPI_5[InputDataList[[9]]$Year==i]
    }
    
    #Harvests in all months, based off KRFC harvest
    ROPI.Harv.oct=ROPI.sep*KRFC.oct/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==10 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.may=ROPI.sep*KRFC.may/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==5 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.july=ROPI.sep*KRFC.july/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==7 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.jan=ROPI.sep*KRFC.jan/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==1 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.feb=ROPI.sep*KRFC.feb/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==2 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.mar=ROPI.sep*KRFC.mar/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==3 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.apr=ROPI.sep*KRFC.apr/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==4 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.june=ROPI.sep*KRFC.june/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==6 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.aug=ROPI.sep*KRFC.aug/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==8 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.nov=ROPI.sep*KRFC.nov/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==11 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.dec=ROPI.sep*KRFC.dec/KRFC.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==12 & InputDataList[[12]]$age == j,]$ER[1]
    ROPI.Harv.sep=ROPI.sep * InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==9 & InputDataList[[12]]$age == j,]$ER[1]

    
    NCAT1HarvRow <- data.frame(RunYear = i, SheltonStk = "NCA", Age = j, TimeStep = 1, TotMort.sum = ROPI.Harv.oct + ROPI.Harv.jan + ROPI.Harv.feb +
                                 ROPI.Harv.mar + ROPI.Harv.apr + ROPI.Harv.nov + ROPI.Harv.dec + InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==10 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==11 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==12 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==1 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==2 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==3 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==4 & InputDataList[[12]]$age == j,]$oceanimpacts[1])
    NCAT2HarvRow <- data.frame(RunYear = i, SheltonStk = "NCA", Age = j, TimeStep = 2, TotMort.sum = ROPI.Harv.may + ROPI.Harv.june +
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==5 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==6 & InputDataList[[12]]$age == j,]$oceanimpacts[1])
    NCAT3HarvRow <- data.frame(RunYear = i, SheltonStk = "NCA", Age = j, TimeStep = 3, TotMort.sum = ROPI.Harv.july + ROPI.Harv.aug + ROPI.Harv.sep +
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==7 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==8 & InputDataList[[12]]$age == j,]$oceanimpacts[1]+
                                 InputDataList[[12]][InputDataList[[12]]$mgmtyr==i & InputDataList[[12]]$month==9 & InputDataList[[12]]$age == j,]$oceanimpacts[1])
    
    SOFHarv <- rbind(SOFHarv, NCAT1HarvRow, NCAT2HarvRow, NCAT3HarvRow)

  }
}


cohort_Shelton_Stk <- rbind(cohort_Shelton_Stk,SOFabundances, UpColSprDF)

#For loop that combines Shelton distributions with abundances

#First, set up DF... create columns for area-specific abundances
cohort_by_area <- cohort_Shelton_Stk
cohort_by_area$SALISH <- NULL
cohort_by_area$NOF <- NULL
cohort_by_area$OR <- NULL
cohort_by_area$CALI <- NULL
cohort_by_area$NORTH <- NULL
cohort_by_area$SWWCVI <- NULL

cohort_by_area$StartCohort.sum <- as.double(cohort_by_area$StartCohort.sum)

for (i in 1:nrow(cohort_by_area)){
  
  # First subset the data frame; then for each stock applying the proportions to each abundance.
  RowProps <- subset(InputDataList[[6]], Stock == cohort_by_area$SheltonStk[i] & TS == cohort_by_area$TimeStep[i])
  cohort_by_area$SALISH[i] <- cohort_by_area$StartCohort.sum[i] * RowProps$SALISH[1]
  cohort_by_area$NOF[i] <- cohort_by_area$StartCohort.sum[i] * RowProps$NOF[1]
  cohort_by_area$OR[i] <- cohort_by_area$StartCohort.sum[i] * RowProps$OR[1]
  cohort_by_area$CALI[i] <- cohort_by_area$StartCohort.sum[i] * RowProps$CALI[1]
  cohort_by_area$NORTH[i] <- cohort_by_area$StartCohort.sum[i] * RowProps$NORTH[1]
  cohort_by_area$SWWCVI[i] <- cohort_by_area$StartCohort.sum[i] * RowProps$SWWCVI[1]
  
  
}

#Summing together area-specific abundances by year, time step
cohort_SALISH <- summaryBy(SALISH~RunYear+TimeStep, data = cohort_by_area, FUN = sum)
cohort_NOF <- summaryBy(NOF~RunYear+TimeStep, data = cohort_by_area, FUN = sum)
cohort_OR <- summaryBy(OR~RunYear+TimeStep, data = cohort_by_area, FUN = sum)
cohort_CALI <- summaryBy(CALI~RunYear+TimeStep, data = cohort_by_area, FUN = sum)
cohort_SWWCVI <- summaryBy(SWWCVI~RunYear+TimeStep, data = cohort_by_area, FUN = sum)

cohort_SALISH$Area <- "SALISH"
cohort_NOF$Area <- "NOF"
cohort_OR$Area <- "OR"
cohort_CALI$Area <- "CALI"
cohort_SWWCVI$Area <- "SWWCVI"

# Correlations SRKW/Chinook Abundance; preparing outputs ---- 

#First, set up the data.  

#To merge data frames, the year column must have the same name.  Data must be in the same format.

colnames(cohort_NOF)[colnames(cohort_NOF)=="NOF.sum"] <- "abundance"
colnames(cohort_OR)[colnames(cohort_OR)=="OR.sum"] <- "abundance"
colnames(cohort_CALI)[colnames(cohort_CALI)=="CALI.sum"] <- "abundance"
colnames(cohort_SWWCVI)[colnames(cohort_SWWCVI)=="SWWCVI.sum"] <- "abundance"
colnames(cohort_SALISH)[colnames(cohort_SALISH)=="SALISH.sum"] <- "abundance"

cohort_all_areas <- rbind (cohort_NOF, cohort_OR, cohort_CALI, cohort_SWWCVI, cohort_SALISH)

colnames(cohort_all_areas)[colnames(cohort_all_areas)=="RunYear"] <- "year"
cohort_all_areas$year <- as.numeric(cohort_all_areas$year)

write.csv(cohort_all_areas, file = "Test.csv")

cohort_all_areas_Zero_PFMC <- cohort_all_areas

cohort_all_areas_Vali <- InputDataList[[13]]

cohort_all_areas_Vali <- as.data.frame(cohort_all_areas_Vali)

#Preparing outputs related to starting cohorts post-fishing -----
#If assessing the impacts of fisheries, we want to remove fishery mortalities from the starting abundances.
#The reason is, if we do not, we will not account for fisheries occurring within a particular time step.
#E.g., starting abundances in Time Step 3 will not show fishery impacts from time step 3.
#Of course SRKW feed throughout the whole time step rather than just the start, so fisheries should be accounted for.

Morts_Vali <- InputDataList[[14]]
Morts_No_PFMC <- InputDataList[[15]]

#Convert from 78 to 39 stock format
Morts_Vali$Stock <- ceiling(Morts_Vali$StockID/2)
Morts_Vali$TotMort <- Morts_Vali$LandedCatch+Morts_Vali$NonRetention+Morts_Vali$Shaker+Morts_Vali$DropOff+
  Morts_Vali$MSFLandedCatch+Morts_Vali$MSFNonRetention+Morts_Vali$MSFShaker+Morts_Vali$MSFDropOff
#merge with DFs from earlier sections of the code, to get year and SheltonStk
Morts_Vali <- merge(Morts_Vali, StkNms, by= "Stock")
Morts_Vali <- merge(Morts_Vali, RunIDs, by= "RunID")

#Convert to data frame for summaryBy function
Morts_Vali <- as.data.frame(Morts_Vali)

#Combine Marked and Unmarked components of each stock; combine into Shelton stocks rather than FRAM stocks
Morts_Shelton_Stk_Vali <- summaryBy(TotMort~RunYear+SheltonStk+Age+TimeStep, data = Morts_Vali, FUN = sum)

#Remove Central Valley
Morts_Shelton_Stk_Vali <- Morts_Shelton_Stk_Vali[Morts_Shelton_Stk_Vali$SheltonStk != "CENTV",]
Morts_Shelton_Stk_Vali <- rbind(Morts_Shelton_Stk_Vali, SOFHarv)

Morts_by_area_Vali <- Morts_Shelton_Stk_Vali
Morts_by_area_Vali$SALISH <- NULL
Morts_by_area_Vali$NOF <- NULL
Morts_by_area_Vali$OR <- NULL
Morts_by_area_Vali$CALI <- NULL
Morts_by_area_Vali$NORTH <- NULL
Morts_by_area_Vali$SWWCVI <- NULL

Morts_by_area_Vali$TotMort.sum <- as.double(Morts_by_area_Vali$TotMort.sum)

for (i in 1:nrow(Morts_by_area_Vali)){
  
  # First subset the data frame; then for each stock applying the proportions to each abundance.
  RowProps <- subset(InputDataList[[6]], Stock == Morts_by_area_Vali$SheltonStk[i] & TS == Morts_by_area_Vali$TimeStep[i])
  Morts_by_area_Vali$SALISH[i] <- Morts_by_area_Vali$TotMort.sum[i] * RowProps$SALISH[1]
  Morts_by_area_Vali$NOF[i] <- Morts_by_area_Vali$TotMort.sum[i] * RowProps$NOF[1]
  Morts_by_area_Vali$OR[i] <- Morts_by_area_Vali$TotMort.sum[i] * RowProps$OR[1]
  Morts_by_area_Vali$CALI[i] <- Morts_by_area_Vali$TotMort.sum[i] * RowProps$CALI[1]
  Morts_by_area_Vali$NORTH[i] <- Morts_by_area_Vali$TotMort.sum[i] * RowProps$NORTH[1]
  Morts_by_area_Vali$SWWCVI[i] <- Morts_by_area_Vali$TotMort.sum[i] * RowProps$SWWCVI[1]
  
  
}

#Summing together area-specific abundances by year, time step
Morts_SALISH_Vali <- summaryBy(SALISH~RunYear+TimeStep, data = Morts_by_area_Vali, FUN = sum)
Morts_NOF_Vali <- summaryBy(NOF~RunYear+TimeStep, data = Morts_by_area_Vali, FUN = sum)
Morts_OR_Vali <- summaryBy(OR~RunYear+TimeStep, data = Morts_by_area_Vali, FUN = sum)
Morts_CALI_Vali <- summaryBy(CALI~RunYear+TimeStep, data = Morts_by_area_Vali, FUN = sum)
Morts_SWWCVI_Vali <- summaryBy(SWWCVI~RunYear+TimeStep, data = Morts_by_area_Vali, FUN = sum)

Morts_SALISH_Vali$Area <- "SALISH"
Morts_NOF_Vali$Area <- "NOF"
Morts_OR_Vali$Area <- "OR"
Morts_CALI_Vali$Area <- "CALI"
Morts_SWWCVI_Vali$Area <- "SWWCVI"

colnames(Morts_NOF_Vali)[colnames(Morts_NOF_Vali)=="NOF.sum"] <- "Morts"
colnames(Morts_OR_Vali)[colnames(Morts_OR_Vali)=="OR.sum"] <- "Morts"
colnames(Morts_CALI_Vali)[colnames(Morts_CALI_Vali)=="CALI.sum"] <- "Morts"
colnames(Morts_SWWCVI_Vali)[colnames(Morts_SWWCVI_Vali)=="SWWCVI.sum"] <- "Morts"
colnames(Morts_SALISH_Vali)[colnames(Morts_SALISH_Vali)=="SALISH.sum"] <- "Morts"

Morts_all_areas_Vali <- rbind (Morts_NOF_Vali, Morts_OR_Vali, Morts_CALI_Vali, Morts_SWWCVI_Vali, Morts_SALISH_Vali)


#Convert from 78 to 39 stock format
Morts_No_PFMC$Stock <- ceiling(Morts_No_PFMC$StockID/2)
Morts_No_PFMC$TotMort <- Morts_No_PFMC$LandedCatch+Morts_No_PFMC$NonRetention+Morts_No_PFMC$Shaker+Morts_No_PFMC$DropOff+
  Morts_No_PFMC$MSFLandedCatch+Morts_No_PFMC$MSFNonRetention+Morts_No_PFMC$MSFShaker+Morts_No_PFMC$MSFDropOff
#merge with DFs from earlier sections of the code, to get year and SheltonStk
Morts_No_PFMC <- merge(Morts_No_PFMC, StkNms, by= "Stock")
Morts_No_PFMC <- merge(Morts_No_PFMC, RunIDs, by= "RunID")

#Convert to data frame for summaryBy function
Morts_No_PFMC <- as.data.frame(Morts_No_PFMC)

#Combine Marked and Unmarked components of each stock; combine into Shelton stocks rather than FRAM stocks
Morts_Shelton_Stk_No_PFMC <- summaryBy(TotMort~RunYear+SheltonStk+Age+TimeStep, data = Morts_No_PFMC, FUN = sum)

#Remove Central Valley
Morts_Shelton_Stk_No_PFMC <- Morts_Shelton_Stk_No_PFMC[Morts_Shelton_Stk_No_PFMC$SheltonStk != "CENTV",]

Morts_by_area_No_PFMC <- Morts_Shelton_Stk_No_PFMC
Morts_by_area_No_PFMC$SALISH <- NULL
Morts_by_area_No_PFMC$NOF <- NULL
Morts_by_area_No_PFMC$OR <- NULL
Morts_by_area_No_PFMC$CALI <- NULL
Morts_by_area_No_PFMC$NORTH <- NULL
Morts_by_area_No_PFMC$SWWCVI <- NULL

Morts_by_area_No_PFMC$TotMort.sum <- as.double(Morts_by_area_No_PFMC$TotMort.sum)

for (i in 1:nrow(Morts_by_area_No_PFMC)){
  
  # First subset the data frame; then for each stock applying the proportions to each abundance.
  RowProps <- subset(InputDataList[[6]], Stock == Morts_by_area_No_PFMC$SheltonStk[i] & TS == Morts_by_area_No_PFMC$TimeStep[i])
  Morts_by_area_No_PFMC$SALISH[i] <- Morts_by_area_No_PFMC$TotMort.sum[i] * RowProps$SALISH[1]
  Morts_by_area_No_PFMC$NOF[i] <- Morts_by_area_No_PFMC$TotMort.sum[i] * RowProps$NOF[1]
  Morts_by_area_No_PFMC$OR[i] <- Morts_by_area_No_PFMC$TotMort.sum[i] * RowProps$OR[1]
  Morts_by_area_No_PFMC$CALI[i] <- Morts_by_area_No_PFMC$TotMort.sum[i] * RowProps$CALI[1]
  Morts_by_area_No_PFMC$NORTH[i] <- Morts_by_area_No_PFMC$TotMort.sum[i] * RowProps$NORTH[1]
  Morts_by_area_No_PFMC$SWWCVI[i] <- Morts_by_area_No_PFMC$TotMort.sum[i] * RowProps$SWWCVI[1]
  
  
}

#Summing together area-specific abundances by year, time step
Morts_SALISH_No_PFMC <- summaryBy(SALISH~RunYear+TimeStep, data = Morts_by_area_No_PFMC, FUN = sum)
Morts_NOF_No_PFMC <- summaryBy(NOF~RunYear+TimeStep, data = Morts_by_area_No_PFMC, FUN = sum)
Morts_OR_No_PFMC <- summaryBy(OR~RunYear+TimeStep, data = Morts_by_area_No_PFMC, FUN = sum)
Morts_CALI_No_PFMC <- summaryBy(CALI~RunYear+TimeStep, data = Morts_by_area_No_PFMC, FUN = sum)
Morts_SWWCVI_No_PFMC <- summaryBy(SWWCVI~RunYear+TimeStep, data = Morts_by_area_No_PFMC, FUN = sum)

Morts_SALISH_No_PFMC$Area <- "SALISH"
Morts_NOF_No_PFMC$Area <- "NOF"
Morts_OR_No_PFMC$Area <- "OR"
Morts_CALI_No_PFMC$Area <- "CALI"
Morts_SWWCVI_No_PFMC$Area <- "SWWCVI"

colnames(Morts_NOF_No_PFMC)[colnames(Morts_NOF_No_PFMC)=="NOF.sum"] <- "Morts"
colnames(Morts_OR_No_PFMC)[colnames(Morts_OR_No_PFMC)=="OR.sum"] <- "Morts"
colnames(Morts_CALI_No_PFMC)[colnames(Morts_CALI_No_PFMC)=="CALI.sum"] <- "Morts"
colnames(Morts_SWWCVI_No_PFMC)[colnames(Morts_SWWCVI_No_PFMC)=="SWWCVI.sum"] <- "Morts"
colnames(Morts_SALISH_No_PFMC)[colnames(Morts_SALISH_No_PFMC)=="SALISH.sum"] <- "Morts"

Morts_all_areas_No_PFMC <- rbind (Morts_NOF_No_PFMC, Morts_OR_No_PFMC, Morts_CALI_No_PFMC, Morts_SWWCVI_No_PFMC, Morts_SALISH_No_PFMC)

#Now add in mortalities for each area-time step-year

cohort_all_areas_Zero_PFMC$Morts <- NULL

for(i in 1:nrow(cohort_all_areas_Zero_PFMC)){
  cohort_all_areas_Zero_PFMC$Morts[i] <- subset(Morts_all_areas_No_PFMC, RunYear == cohort_all_areas_Zero_PFMC$year[i] & TimeStep == cohort_all_areas_Zero_PFMC$TimeStep[i] &
                                               Area == cohort_all_areas_Zero_PFMC$Area[i])$Morts[1]
}

cohort_all_areas_Vali$Morts <- NULL

for(i in 1:nrow(cohort_all_areas_Vali)){
  cohort_all_areas_Vali$Morts[i] <- subset(Morts_all_areas_Vali, RunYear == cohort_all_areas_Vali$year[i] & TimeStep == cohort_all_areas_Vali$TimeStep[i] &
                                                  Area == cohort_all_areas_Vali$Area[i])$Morts[1]
}

cohort_all_areas_Vali$RunType <- "Validation"
cohort_all_areas_Zero_PFMC$RunType <- "Zero PFMC"

cohort_all_areas <- rbind(cohort_all_areas_Vali, cohort_all_areas_Zero_PFMC)

cohort_all_areas$Post.Fish.Abund <- cohort_all_areas$abundance-cohort_all_areas$Morts

filePath <- file.path("C:\\Users\\dappdrd\\Desktop\\Main folder for stuff\\SRKW Workgroup\\Updated Harvest Analysis\\Harvest_Analysis_Output_File.xlsx")

write.xlsx(cohort_all_areas, file = filePath, sheetName = "Chinook cohorts", append = FALSE)

AreaList <- c("NOF", "OR", "CALI", "SWWCVI", "SALISH", "SOF", "COASTWIDE")

for(i in 1:length(AreaList)){
  
  if(AreaList[i] == "SOF"){
    T1Data <- subset(cohort_all_areas, Area %in% c("OR","CALI") & TimeStep == 1)
    T1Data <- summaryBy(Post.Fish.Abund~year+TimeStep+RunType, data = T1Data, FUN = sum)
    colnames(T1Data)[colnames(T1Data)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
    T2Data <- subset(cohort_all_areas, Area %in% c("OR","CALI") & TimeStep == 2)
    T2Data <- summaryBy(Post.Fish.Abund~year+TimeStep+RunType, data = T2Data, FUN = sum)
    colnames(T2Data)[colnames(T2Data)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
    T3Data <- subset(cohort_all_areas, Area %in% c("OR","CALI") & TimeStep == 3)
    T3Data <- summaryBy(Post.Fish.Abund~year+TimeStep+RunType, data = T3Data, FUN = sum)
    colnames(T3Data)[colnames(T3Data)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
  }
  else if(AreaList[i] == "COASTWIDE"){
    T1Data <- subset(cohort_all_areas, Area %in% c("OR","CALI","NOF") & TimeStep == 1)
    T1Data <- summaryBy(Post.Fish.Abund~year+TimeStep+RunType, data = T1Data, FUN = sum)
    colnames(T1Data)[colnames(T1Data)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
    T2Data <- subset(cohort_all_areas, Area %in% c("OR","CALI","NOF") & TimeStep == 2)
    T2Data <- summaryBy(Post.Fish.Abund~year+TimeStep+RunType, data = T2Data, FUN = sum)
    colnames(T2Data)[colnames(T2Data)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
    T3Data <- subset(cohort_all_areas, Area %in% c("OR","CALI","NOF") & TimeStep == 3)
    T3Data <- summaryBy(Post.Fish.Abund~year+TimeStep+RunType, data = T3Data, FUN = sum)
    colnames(T3Data)[colnames(T3Data)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
  }
  else{
  
    T1Data <- subset(cohort_all_areas, Area == AreaList[i] & TimeStep == 1)
    T2Data <- subset(cohort_all_areas, Area == AreaList[i] & TimeStep == 2)
    T3Data <- subset(cohort_all_areas, Area == AreaList[i] & TimeStep == 3)
  }
  
  p1 <- ggplot(data = T1Data, aes(x = RunType, y = Post.Fish.Abund, fill = RunType)) + geom_bar(stat='identity') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    facet_grid(~year, scale='free_x')+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Run Type and Year") + ylab("Chinook Abundance")+
    ggtitle(paste(AreaList[i]," - Time Step ",1, sep=""))+theme(strip.text = element_text(size = 8))
  
  p2 <- ggplot(data = T2Data, aes(x = RunType, y = Post.Fish.Abund, fill = RunType)) + geom_bar(stat='identity') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    facet_grid(~year, scale='free_x')+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Run Type and Year") + ylab("Chinook Abundance")+
    ggtitle(paste(AreaList[i]," - Time Step ",2, sep=""))+theme(strip.text = element_text(size = 8))
  
  p3 <- ggplot(data = T3Data, aes(x = RunType, y = Post.Fish.Abund, fill = RunType)) + geom_bar(stat='identity') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    facet_grid(~year, scale='free_x')+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Run Type and Year") + ylab("Chinook Abundance")+
    ggtitle(paste(AreaList[i]," - Time Step ",3, sep="")) +theme(strip.text = element_text(size = 8))
  
  ggsave(paste(output,AreaList[i]," - Time Step ",1,".png", sep=""), p1, units = "in", width = 10, height = 7)
  ggsave(paste(output,AreaList[i]," - Time Step ",2,".png", sep=""), p2, units = "in", width = 10, height = 7)
  ggsave(paste(output,AreaList[i]," - Time Step ",3,".png", sep=""), p3, units = "in", width = 10, height = 7)
    
}

#This next section is to apply regression results to our data...

#First prep SRKW pop data...

fems_srkw <- data.frame(dplyr::filter(InputDataList[[16]], 
                                      includeFec==1, 
                                      sexF1M2==1,
                                      alive==1,
                                      age%in%seq(10,42), 
                                      gave_birth != "NA",
                                      year>1980))

fems_srkw$gave_birth <- as.integer(fems_srkw$gave_birth)
fems_srkw$year <- as.integer(fems_srkw$year)
fems_srkw$age<- as.integer(fems_srkw$age)

SRKWPopDat <- InputDataList[[16]]

SRKWPopDat$age <- as.numeric(SRKWPopDat$age)

SurvivalDF <- dplyr::left_join(SRKWPopDat, InputDataList[[17]])

all_srkw = dplyr::filter(SurvivalDF, 
                         includeSurv==1,
                         alive != "NA",
                         year>1980)

all_srkw$alive <- as.integer(all_srkw$alive)
all_srkw$year <- as.integer(all_srkw$year)

peanut <- InputDataList[[18]]

fems_srkw_subsetyears <- subset(fems_srkw, year >= minYr & year <= maxYr)
all_srkw_subsetyears <- subset(all_srkw, year >= minYr & year <= maxYr)
peanut_subsetyears <- subset(peanut, year >= minYr & year <= maxYr)

#Create DF with vali/zero pfmc abundances in the same row

cohort_all_areas_Vali$Post.Fish.Abund <- cohort_all_areas_Vali$abundance-cohort_all_areas_Vali$Morts
cohort_all_areas_Vali$Post.Fish.Abund.Zero.PFMC <-NULL

for (i in 1:nrow(cohort_all_areas_Vali)){
  cohort_all_areas_Vali$Post.Fish.Abund.Zero.PFMC[i] <- subset(cohort_all_areas, RunType == "Zero PFMC" & Area == cohort_all_areas_Vali$Area[i] & 
                                                              TimeStep == cohort_all_areas_Vali$TimeStep[i] & year == cohort_all_areas_Vali$year[i])$Post.Fish.Abund[1]
}

#Create a blank DF for saving differences in SRKW pop data.

Regression_Summary <- data.frame(year = as.integer(), TimeStep = as.integer(), Area = as.character(),
                                    Vali_Surv = as.integer(), Vali_Fec = as.integer(), Vali_Peanut = as.integer(), 
                                    Vali_Fec_lag_1 = as.integer(), Vali_Fec_lag_2 = as.integer(), Vali_Surv_lag_1 = as.integer(),
                                    Zero_Surv = as.integer(), Zero_Fec = as.integer(), Zero_Peanut = as.integer(),
                                    Zero_Fec_lag_1 = as.integer(), Zero_Fec_lag_2 = as.integer(), Zero_Surv_lag_1 = as.integer())


#Processing loops through areas/time steps
for (i in 1:length(AreaList)){
  for (j in 1:3){
    
    if(AreaList[i] == "SOF"){
      TempDat <- subset(cohort_all_areas_Vali, Area %in% c("OR","CALI") & TimeStep == j)
      TempDat <- summaryBy(abundance+Post.Fish.Abund.Zero.PFMC+Post.Fish.Abund~year+TimeStep, data = TempDat, FUN = sum)
      colnames(TempDat)[colnames(TempDat)=="abundance.sum"] <- "abundance"
      colnames(TempDat)[colnames(TempDat)=="Post.Fish.Abund.Zero.PFMC.sum"] <- "Post.Fish.Abund.Zero.PFMC"
      colnames(TempDat)[colnames(TempDat)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
    }
    else if(AreaList[i] == "COASTWIDE"){
      TempDat <- subset(cohort_all_areas_Vali, Area %in% c("OR","CALI","NOF") & TimeStep == j)
      TempDat <- summaryBy(abundance+Post.Fish.Abund.Zero.PFMC+Post.Fish.Abund~year+TimeStep, data = TempDat, FUN = sum)
      colnames(TempDat)[colnames(TempDat)=="abundance.sum"] <- "abundance"
      colnames(TempDat)[colnames(TempDat)=="Post.Fish.Abund.Zero.PFMC.sum"] <- "Post.Fish.Abund.Zero.PFMC"
      colnames(TempDat)[colnames(TempDat)=="Post.Fish.Abund.sum"] <- "Post.Fish.Abund"
    }
    else{
      TempDat <- subset(cohort_all_areas_Vali, Area == AreaList[i] & TimeStep == j)
    }
    
    #create an identifier to Timestep and area
    TSArea <- paste (AreaList[i],"_TimeStep_",j, sep = "")
    
    #Merge Surv/Fec/Peanut/ind data sets with abundance data
    TempFecDat <- dplyr::left_join(TempDat, fems_srkw_subsetyears, TempDat, by="year")
    TempSurvDat <- dplyr::left_join(TempDat, all_srkw_subsetyears, TempDat, by="year")
    TempPeanutDat <- dplyr::left_join(TempDat, peanut_subsetyears, TempDat, by="year")
    
    #Note that for this analysis I used post-fishery abundances in the regressions.
    #This made more sense than starting abundances because otherwise, we would not be comparing apples to apples.
    
    #model each glm as in the original code
    Fecundityglm <- glm(gave_birth~Post.Fish.Abund + age + I(age^2), family = binomial, data = TempFecDat)
    Survglm <- glm(alive ~ Post.Fish.Abund + stage, family = binomial, data = TempSurvDat)
    Peanutglm = glm(peanut ~ Post.Fish.Abund, data=TempPeanutDat, family=poisson)
    
    #Lag 1-year and 2-year Fecundity
    #For this section, we have to start the analysis at 1993/1994 (we don't have a 1991 abundance estimate to use to start at 1992)
    TempFecDat$LagOneAbundance <- NA
    TempFecDat$LagTwoAbundance <- NA
    TempFecDat$LagOneAbundance.Zero.PFMC <- NA
    TempFecDat$LagTwoAbundance.Zero.PFMC <- NA
    for(k in 1:nrow(TempFecDat)){
      #if the year is one greater than the min year
      if (TempFecDat$year[k] > minYr){
        TempFecDat$LagOneAbundance[k] <- subset(TempFecDat, year == TempFecDat$year[k]-1)$Post.Fish.Abund[1]
        TempFecDat$LagOneAbundance.Zero.PFMC[k] <- subset(TempFecDat, year == TempFecDat$year[k]-1)$Post.Fish.Abund.Zero.PFMC[1]
      }
      #if the year is two greater than the min year
      if (TempFecDat$year[k] > minYr+1){
        TempFecDat$LagTwoAbundance[k] <- subset(TempFecDat, year == TempFecDat$year[k]-2)$Post.Fish.Abund[1]
        TempFecDat$LagTwoAbundance.Zero.PFMC[k] <- subset(TempFecDat, year == TempFecDat$year[k]-2)$Post.Fish.Abund.Zero.PFMC[1]
      }
    }
    TempFecDatLagOne <- subset(TempFecDat, year > minYr)
    TempFecDatLagTwo <- subset(TempFecDat, year > minYr+1)
    
    #model fecundity as a logistic regression. Eric recommended age as a quadratic.
    FecundityLagOneglm <- glm(gave_birth~LagOneAbundance + age + I(age^2), family = binomial, data = TempFecDatLagOne)
    FecundityLagTwoglm <- glm(gave_birth~LagTwoAbundance + age + I(age^2), family = binomial, data = TempFecDatLagTwo)
    
    #Lag 1-year survival
    #For this section, we have to start the analysis at 1993 (we don't have a 1991 abundance estimate to use to start at 1992)
    TempSurvDat$LagOneAbundance <- NA
    TempSurvDat$LagOneAbundance.Zero.PFMC <- NA
    
    for(k in 1:nrow(TempSurvDat)){
      #if the year is one greater than the min year
      if (TempSurvDat$year[k] > minYr){
        TempSurvDat$LagOneAbundance[k] <- subset(TempSurvDat, year == TempSurvDat$year[k]-1)$Post.Fish.Abund[1]
        TempSurvDat$LagOneAbundance.Zero.PFMC[k] <- subset(TempSurvDat, year == TempSurvDat$year[k]-1)$Post.Fish.Abund.Zero.PFMC[1]
      }
    }
    TempSurvDatLagOne <- subset(TempSurvDat, year > minYr)
    
    #model survival as above
    SurvLagOneglm <- glm(alive ~ LagOneAbundance + stage, family = binomial, data = TempSurvDatLagOne)
    
    
    #Go through each year, saving the difference in SRKW population parameters
    for (k in minYr:maxYr){
      Fecpreddat <- with(TempFecDat, data.frame(Post.Fish.Abund= c(subset(TempFecDat, year == k)$Post.Fish.Abund.Zero.PFMC[1], 
                                                                   subset(TempFecDat, year == k)$Post.Fish.Abund[1]), age = 20))
      Fecfit <-  predict(Fecundityglm, newdata = Fecpreddat, type = "link", se = TRUE)
      Survpreddat <- with(TempSurvDat, data.frame(Post.Fish.Abund = c(subset(TempSurvDat, year == k)$Post.Fish.Abund.Zero.PFMC[1], 
                                                                subset(TempSurvDat, year == k)$Post.Fish.Abund[1]), stage = "young_female"))
      Survfit <-  predict(Survglm, newdata = Survpreddat, type = "link", se = TRUE)
      Peanutpreddat <- with(TempPeanutDat, data.frame(Post.Fish.Abund = c(subset(TempPeanutDat, year == k)$Post.Fish.Abund.Zero.PFMC[1], 
                                                                          subset(TempPeanutDat, year == k)$Post.Fish.Abund[1])))
      Peanutfit <-  predict(Peanutglm, newdata = Peanutpreddat, type = "link", se = TRUE)
      FecLagOnepreddat <- with(TempFecDatLagOne, data.frame(LagOneAbundance = c(subset(TempFecDatLagOne, year == k)$LagOneAbundance.Zero.PFMC[1], 
                                                                                subset(TempFecDatLagOne, year == k)$LagOneAbundance[1]), age = 20))
      FecLagOnefit <-  predict(FecundityLagOneglm, newdata = FecLagOnepreddat, type = "link", se = TRUE)
      FecLagTwopreddat <- with(TempFecDatLagTwo, data.frame(LagTwoAbundance = c(subset(TempFecDatLagOne, year == k)$LagTwoAbundance.Zero.PFMC[1], 
                                                                                subset(TempFecDatLagOne, year == k)$LagTwoAbundance[1]), age = 20))
      FecLagTwofit <-  predict(FecundityLagTwoglm, newdata = FecLagTwopreddat, type = "link", se = TRUE)
      SurvLagOnepreddat <- with(TempSurvDatLagOne, data.frame(LagOneAbundance = c(subset(TempSurvDatLagOne, year == k)$LagOneAbundance.Zero.PFMC[1], 
                                                                                subset(TempSurvDatLagOne, year == k)$LagOneAbundance[1]),  stage = "young_female"))
      SurvLagOnefit <-  predict(SurvLagOneglm, newdata = SurvLagOnepreddat, type = "link", se = TRUE)

      
      newrow <- data.frame(year = k, TimeStep = j, Area = AreaList[i], Vali_Surv = binomial()$linkinv(Survfit$fit[2]), Vali_Fec = binomial()$linkinv(Fecfit$fit[2]),
                           Vali_Peanut = binomial()$linkinv(Peanutfit$fit[2]), Vali_Fec_lag_1 = binomial()$linkinv(FecLagOnefit$fit[2]), Vali_Fec_lag_2 = binomial()$linkinv(FecLagTwofit$fit[2]),
                           Vali_Surv_lag_1 = binomial()$linkinv(SurvLagOnefit$fit[2]), 
                           Zero_Surv = binomial()$linkinv(Survfit$fit[1]), Zero_Fec = binomial()$linkinv(Fecfit$fit[1]), Zero_Peanut = binomial()$linkinv(Peanutfit$fit[1]),
                           Zero_Fec_lag_1 = binomial()$linkinv(FecLagOnefit$fit[1]), Zero_Fec_lag_2 = binomial()$linkinv(FecLagTwofit$fit[1]),
                           Zero_Surv_lag_1 = binomial()$linkinv(SurvLagOnefit$fit[1]))
      
      Regression_Summary <- rbind(Regression_Summary, newrow)
    }
  }
}

write.xlsx(Regression_Summary, file = filePath, sheetName = "Reg Results", append = TRUE)

#Create a summary

Regression_Summary_Averages = Regression_Summary

Regression_Summary_Averages$SurvIncrease <- Regression_Summary_Averages$Zero_Surv - Regression_Summary_Averages$Vali_Surv
Regression_Summary_Averages$FecIncrease <- Regression_Summary_Averages$Zero_Fec - Regression_Summary_Averages$Vali_Fec
Regression_Summary_Averages$PeanutDecrease <- Regression_Summary_Averages$Zero_Peanut - Regression_Summary_Averages$Vali_Peanut
Regression_Summary_Averages$FecIncrease_Lag_1 <- Regression_Summary_Averages$Zero_Fec_lag_1 - Regression_Summary_Averages$Vali_Fec_lag_1
Regression_Summary_Averages$FecIncrease_Lag_2 <- Regression_Summary_Averages$Zero_Fec_lag_2 - Regression_Summary_Averages$Vali_Fec_lag_2
Regression_Summary_Averages$SurvIncrease_Lag_1 <- Regression_Summary_Averages$Zero_Surv_lag_1 - Regression_Summary_Averages$Vali_Surv_lag_1

keeps <- c("year", "TimeStep","Area","SurvIncrease", "FecIncrease", "PeanutDecrease", "FecIncrease_Lag_1", "FecIncrease_Lag_2", "SurvIncrease_Lag_1")

Regression_Summary_Averages <- Regression_Summary_Averages[, names(Regression_Summary_Averages) %in% keeps]

write.xlsx(Regression_Summary_Averages, file = filePath, sheetName = "Yrly SRKW Pop Changes", append = TRUE)

Yearly_Average_Regressions <-  summaryBy(SurvIncrease+FecIncrease+PeanutDecrease~TimeStep+Area, data = Regression_Summary_Averages, FUN=mean)

Lag1Regressions <- subset(Regression_Summary_Averages, year > minYr)

Lag_1_Yearly_Average_Regressions <- summaryBy(FecIncrease_Lag_1 + SurvIncrease_Lag_1 ~ TimeStep + Area, data = Lag1Regressions, Fun = mean)

Lag2Regressions <- subset(Regression_Summary_Averages, year > minYr + 1)

Lag_2_Yearly_Average_Regressions <- summaryBy(FecIncrease_Lag_2 ~ TimeStep + Area, data = Lag2Regressions, Fun = mean)

Yearly_Average_Regressions <- merge(Yearly_Average_Regressions, Lag_1_Yearly_Average_Regressions, by= c("Area", "TimeStep"))
Yearly_Average_Regressions <- merge(Yearly_Average_Regressions, Lag_2_Yearly_Average_Regressions, by= c("Area", "TimeStep"))

write.xlsx(Yearly_Average_Regressions, file = filePath, sheetName = "Average Pop Change", append = TRUE)