############################################################################################# 
# SRKW Framework - V. 0.0                                                                   #
#                                                                                           #
# Shiny Application coded by: Derek Dapp, Eric Ward, and Will Satterthwaite                 #
#                                                                                           #
# Design & methodology developed by: The Ad Hoc Southern Resident Killer Whale Workgroup    #
#                                                                                           #
# Ad Hoc SRKW Workgroup membership:                                                         #
#                                                                                           #
# Jeromy Jording (NOAA/Co-chair)                                                            #
# Phil Anderson (PFMC/Co-chair)                                                             #
# Susan Bishop (NOAA)                                                                       #
# Teresa Mongillo (NOAA)                                                                    #
# Will Satterthwaite (NOAA)                                                                 #
# Eric Ward (NOAA)                                                                          #
# Robin Ehlke (PFMC)                                                                        #
# Kyle Adicks (WDFW)                                                                        #
# Derek Dapp (WDFW)                                                                         #
# Lance Hebdon (IDFG)                                                                       #
# Chris Kozfkay (IDFG)                                                                      #
# Craig Foster (ODFW)                                                                       #
# Chris Kern (ODFW)                                                                         #
# Brett Kormos (CDFG)                                                                       #
# Erica Meyers (CDFG)                                                                       #
# Nate Tyler (Makah)                                                                        #
# Patrick DePoe (Makah)                                                                     #
# Tyler Gross (Quileute)                                                                    #
# Melvinjohn Ashue (Hoh)                                                                    #
# Tyler Jurasin (Quinault)                                                                  #
# Mike Matylewich (CRITFC)                                                                  #
#                                                                                           #
# Additional Assistance and data provided by:                                               #
#                                                                                           #
# Stuart Ellis (CRITFC)                                                                     #
# Alex Letvin (CDFG)                                                                        #
# Jon Carey (NOAA)                                                                          #
# Michael O'Farrell (NOAA)                                                                  #
#############################################################################################

# To do:
# - Analysis of Eric's data synthesized with abundance data.

#All the required packages
library(shiny)
library(RODBC)
library(ggplot2)
library(readxl)
library(doBy)
library(dplyr)
#used for sending emails
#From a github repo, to install use the code below
#install.packages("devtools", dep = T)
#library(devtools)
#devtools::install_github("rpremraj/mailR", host = "https://api.github.com")
library(mailR)
#for drop box interfacing
library(rdrop2)
library(mgcv)
library(ggplot2)
library(depmixS4)
library(clustMixType)
library(xlsx)
library(broom)

#Authenticates to Dropbox
token <- readRDS("droptoken_SRKW.rds")
drop_acc(dtoken = token)

#a blank data frame for displaying no table prior to processing
BlankDF <<- data.frame(blank = as.character())

#a blank data frame for displaying a message for those attempting to view tables prior to processing
BlankDF2 <<- data.frame(Error = as.character("Please complete processing to view tables"))

Password <- as.character(read.csv("EmailPass.csv")$EmailPass[1])

#There are both server and ui components to a Shiny application, the section below represents the
#server portion of the code.
server <- function(input, output) {
  
  #Increases the maximum upload size to 30 MB, just in case we have a very large FRAM DB
  options(shiny.maxRequestSize=30*1024^2)
  
  #DB Function ----
  
  #the function below allows the user to save an xlsx input file from the file browser (InputFile button)
  Input.file <- reactive({
    
    inFile <- input$InputFile
    RunIDDF <- read_excel(inFile$datapath,
                          sheet="RunIDs")
    StockDF <- read_excel(inFile$datapath,
                          sheet="FRAM to Shelton Stks")
    ColSprDF <- read_excel(inFile$datapath,
                           sheet="Up Col Spr Abundances")
    CohortDF <- read_excel(inFile$datapath,
                           sheet="Cohort")
    StockFRAMDF <- read_excel(inFile$datapath,
                              sheet="StockFRAM")
    SRFCRUNDF <- read_excel(inFile$datapath,
                              sheet="SRFC Run Dat")
    SRFCHARVDF <- read_excel(inFile$datapath,
                              sheet="SRFC Harv Dat")
    KRFCDF <- read_excel(inFile$datapath,
                             sheet="KRFC")
    ROPIDF <- read_excel(inFile$datapath,
                         sheet="ROPI")
    SheltonDistsDF <- read_excel(inFile$datapath,
                         sheet="Shelton Dists")
    NatMortDF <- read_excel(inFile$datapath,
                                 sheet="Natural Mortality")
    FecundityDF <- read_excel(inFile$datapath,
                            sheet="SRKW Fecundity")
    AgeStageDF <- read_excel(inFile$datapath,
                              sheet="SRKW Ages 2 Stages")
    PeanutsDF <- read_excel(inFile$datapath,
                             sheet="SRKW Peanuts")
    
    #List of data frames
    InputDataList <- list(StockFRAMDF, CohortDF, RunIDDF, ColSprDF, StockDF, SRFCRUNDF, SRFCHARVDF, KRFCDF, ROPIDF, SheltonDistsDF, NatMortDF, 
                          FecundityDF, AgeStageDF, PeanutsDF)
    
    return(InputDataList)
  })
  
  # Plot JPEG ----
  #Creates a function for plotting pictures
  plot_jpeg = function(path, add=FALSE)
  {
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[1:2] # get the resolution
    if (!add) # initialize an empty plot area if add==FALSE
      plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
  }
  
  #Email input data button----
  observe({
    # Take a dependency on input$EmailButton
    if (input$EmailButton == 0)
      return(NULL)
    # Use isolate() to avoid dependency on input$EmailButton
    isolate({
      
      #This checks and makes sure that the user filled out the email address, if not, throw an error
      if (input$EmailAdd == ""){
        showModal(modalDialog(
          title = "Error message",
          "Please fill out your email address to access this function"
        ))
      }
      
      else{
        withProgress(message = 'Sending Email', value = 0, {
          incProgress(1/1, detail = "Attaching Files")
          #sends a mail from my dummy email address.
          send.mail(from = "pfmc.srkw@gmail.com",
                  to = input$EmailAdd,
                  subject = "SRKW Input Data",
                  body = "Please see the attached for a copy of the most recently uploaded input file.",
                  #the email's password is hidden in a file that is rolled into the server upon hosting
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "pfmc.srkw@gmail.com", passwd = Password, ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE,

                  attach.files = c("https://dl.dropboxusercontent.com/s/u4b5skseuypcjl1/SRKW_Framework_Data_File.xlsx"),
                  file.descriptions = c("SRKW Input File"),
                  debug = TRUE)
        })
      }
    })
  })

  
  #Processing after button press----
  
  observe({
    # Take a dependency on input$ProcessButton
    if (input$ProcessButton == 0)
      return(NULL)
    # Use isolate() to avoid dependency on input$ProcessButton
    isolate({
      
      #This checks and makes sure that the user filled out the email address, if not, throw an error
      if (input$EmailAdd == ""){
        showModal(modalDialog(
          title = "Error message",
          "Please fill out your email address to access this function"
        ))
      }
      
      else{
      withProgress(message = 'Processing', value = 0, {
        incProgress(1/1, detail = "Please allow a few minutes")
        # initial data processing ----
        # Uses the Input.file() function defined above to retrieve input data
        InputDataList <<- Input.file()
      
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
        
        #reads the user input, if the Upriver Columbia Spring natural mortality button is set to yes, enter if statement
        if(input$UpColSpr == "Yes"){
          
          #For loop finds the age, calculates terminal run size before natural mortality.
          
          for(i in 1:nrow(UpColSprDF)){
            UpColSprDF$StartCohort.sum[i] <- UpColSprDF$StartCohort.sum[i]/(1-subset(InputDataList[[11]], Age == UpColSprDF$Age[i] & TimeStep == UpColSprDF$TimeStep[i])$NaturalMortalityRate)
          }
          
        }
        
        #Else... do nothing.
      
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
        
        years=c(minYr:maxYr)
        SOFabundances=array(NA,c(0,5))
        colnames(SOFabundances)=c("RunYear","SheltonStk","Age","TimeStep","StartCohort.sum")
        
        #calculate SFB (SRFC, from SI) abundances
        SheltonStk="CENTV"
        Age=3
        
        for (i in years)
        {
          SRFC_river_run=InputDataList[[6]]$SRFC_river_run[InputDataList[[6]]$Year==i]
          annual.harv=InputDataList[[7]][InputDataList[[7]]$year==i,]
          monthses=c(8,7,6,5,4,3,2,1,12,11,10,9) #this is the order to step through months in this simplified cohort reconstruction
          monthly.abundance=array(NA,12)
          step=1 # first calculate August 1 abundance
          monthly.abundance[step]=SRFC_river_run/(1-m)+annual.harv$H[annual.harv$month==monthses[step]]
          for (step in 2:12) #now do remaining months
          {
            monthly.abundance[step]=monthly.abundance[step-1]/(1-m)+annual.harv$H[annual.harv$month==monthses[step]]
          }
          oct.abund=monthly.abundance[11]
          may.abund=monthly.abundance[4]
          july.abund=monthly.abundance[2]
          
          year=array(i,3)
          stock=array(SheltonStk,3)
          ages=array(Age,3)
          seasons=c(1:3) #seasons 1-3 correspond to abundances on Oct 1 (of calendar yr = mgmt yr-1), May 1, and July 1 respectively
          yearly.abunds=c(oct.abund,may.abund,july.abund)
          
          new.abundances=cbind(year,stock,ages,seasons,yearly.abunds)
          
          SOFabundances=rbind(SOFabundances,new.abundances)
          
        }
        
        SheltonStk="NCA"
        for (i in years)
        {
          KRFC.dat.yr=InputDataList[[8]][InputDataList[[8]]$mgmtyr==i,]
          #do aggregations one age at a time within each year
          for (j in 3:5){
          
            KRFC.dat.aged=KRFC.dat.yr[KRFC.dat.yr$age==j,]
            KRFC.sep=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==9]
            KRFC.oct=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==10]
            KRFC.may=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==5]
            KRFC.july=KRFC.dat.aged$totalpop[KRFC.dat.aged$month==7]
            if(j == 3){
              ROPI.sep=InputDataList[[9]]$ROPI_3[InputDataList[[9]]$Year==i]
            }
            else if(j == 4){
              ROPI.sep=InputDataList[[9]]$ROPI_4[InputDataList[[9]]$Year==i]
            }
            else if(j == 5){
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
          RowProps <- subset(InputDataList[[10]], Stock == cohort_by_area$SheltonStk[i] & TS == cohort_by_area$TimeStep[i])
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
        
        #Testing purposes to view outputs...
        cohort_Shelton_Stk <<- cohort_Shelton_Stk
        SOFabundances <<- SOFabundances
        cohort_by_area <<- cohort_by_area
        
        
        #Processing SRKW Fecundity Data----
        #This section of the code was developed by Eric Ward in July 2019
        
        #First, the estimated fecundity rates for a 20 year old female. All other ages have the same approximate shape, 
        #but we'll use 20 because fecundity is thought to peak in the early 20s ((Ward et al. 2009)[https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2664.2009.01647.x]).
        
        fems_srkw <- data.frame(dplyr::filter(InputDataList[[12]], 
                                  includeFec==1, 
                                  sexF1M2==1,
                                  alive==1,
                                  age%in%seq(10,42), 
                                  gave_birth != "NA",
                                  year>1980))
        
        fems_srkw$gave_birth <- as.integer(fems_srkw$gave_birth)
        fems_srkw$year <- as.integer(fems_srkw$year)
        fems_srkw$age<- as.integer(fems_srkw$age)
        
        sim_srkw = mgcv::gam(gave_birth~s(year)+s(age,k=3),
                             family="binomial",data=fems_srkw)

        df_fec = data.frame("population"="SRKW",
                            year=unique(fems_srkw$year),age=20)
        df_fec = dplyr::arrange(df_fec, year)
        df_fec$fec_rate = predict(sim_srkw,
                                  newdata=df_fec, type="response", se.fit=TRUE)$fit
        df_fec$se = predict(sim_srkw,
                            newdata=df_fec, type="response", se.fit=TRUE)$se.fit

        df_fec_display <-df_fec

        df_fec_display$fec_rate = round(df_fec_display$fec_rate,3)
        df_fec_display$se = round(df_fec_display$se,3)

        df_fec_display <<- df_fec_display
        
        
        #Processing SRKW survival data ----
        #This section of the code was developed by Eric Ward in July 2019
        
        #Survival rates are estimated by stage, because of uncertainty in some of the ages. 
        #But we can similarly estimate the year effect as a smooth term, and have the population and stage effects as estimated fixed effects offsets. 
        #[Ward et al. (2013)](https://www.nwfsc.noaa.gov/assets/25/4647_08132013_113012_ImpactsOnSRKWsTM123WebFinal.pdf)
        
        SRKWPopDat <- InputDataList[[12]]
        
        SRKWPopDat$age <- as.numeric(SRKWPopDat$age)
        
        SurvivalDF <- dplyr::left_join(SRKWPopDat, InputDataList[[13]])
        
        all_srkw = dplyr::filter(SurvivalDF, 
                                 includeSurv==1,
                                 alive != "NA",
                                 year>1980)
        
        all_srkw$alive <- as.integer(all_srkw$alive)
        all_srkw$year <- as.integer(all_srkw$year)
        
        sim_srkw = mgcv::gam(alive ~ s(year) + stage, 
                             family = "binomial", 
                             data = all_srkw)
        
        df_surv = data.frame("population"="SRKW",
                             year=unique(all_srkw$year),stage="young_female")
        df_surv = dplyr::arrange(df_surv, year)
        
        df_surv$surv_rate = predict(sim_srkw, newdata=df_surv, 
                                    type="response", se.fit=TRUE)$fit
        df_surv$se = predict(sim_srkw, newdata=df_surv, 
                             type="response", se.fit=TRUE)$se.fit
        
        df_surv_display <- df_surv
        
        df_surv_display$surv_rate = round(df_surv_display$surv_rate,3)
        df_surv_display$se = round(df_surv_display$se,3)
        
        df_surv_display <<- df_surv_display
        
        #Processing SRKW peanut head data ----
        #This section of the code was developed by Eric Ward in July 2019
        
        #The data on peanut head whales was discussed in the Hilborn et al. report as an indicator of killer whale mortality 
        #[Hilborn et al. (2012)](https://www.westcoast.fisheries.noaa.gov/publications/protected_species/marine_mammals/killer_whales/recovery/kw-effects_of_salmon_fisheries_on_srkw-final-rpt.pdf). 
        #As a follow up, Durban and Ellifrit (2019 pers. comm.) have updated the more recent instances of peanut head whales: 
        #L73 (died 2010), J28 (died 2016), J54 (died 2016), J52 (died 2017), J50 (died 2018), J17 (missing 2019).
        
        #To model the occurrence of peanut head syndrome, we could either use the raw data as an indicator, or the predicted response (using a Poisson GAM). 
        #We'll fit a model to the raw data, similar to how the fecundity and survival data are treated above. 
        
        peanut <- InputDataList[[14]]
        g = gam(peanut ~ s(year), offset = log(total), 
                data=peanut, family="poisson")
        peanut$peanut_rate = predict(g,type="response")
        
        peanut <<- peanut
        
        ### Increases in SRKW population size ----
        #This section of the code was developed by Eric Ward in July 2019
        
        #As we discussed at the 07/23/2019 meeting, there's a number of reasons why a declining SRKW population might not 
        #be informative with respect to prey. A small population is subject to variation because of demographic stochasticity (random chance), 
        #and other factors including disease, ship strikes and other human disturbance, and other factors. 

        #As an indicator, we can coarsely bin the time series of SRKW data into periods when the population was increasing (indicator = 1) or 
        #not (indicator = 0). Data here is taken from the [Center for Whale Research's annual census](https://www.whaleresearch.com/orca-population?lightbox=dataItem-joisp3fr).
        
        srkw_increase <- InputDataList[[14]]
        srkw_increase$increase = 1
        
        # fit gam to create inflection points
        srkw_increase$pred = predict(gam(total ~ s(year), data=srkw_increase))
        srkw_increase$increase[-1] = ifelse(diff(srkw_increase$pred) > 0, 1, 0)
        
        srkw_increase$increase = as.factor(srkw_increase$increase)
        
        ## Aggregating metrics ----
        #This section of the code was developed by Eric Ward in July 2019
        
        #We can start by creating a data frame from the 4 metrics above: 
        #fecundity rates, survival rates, occurrence of peanut head whales. 
        
        ind <- dplyr::left_join(df_fec, df_surv, by=c("population","year")) %>% 
          dplyr::left_join(peanut) %>% 
          dplyr::left_join(srkw_increase) %>% 
          dplyr::select(year,fec_rate,surv_rate,peanut_rate,increase)
        
        #Clustering this kind of data is a little complicated because of mixed types; all variables are continuous, 
        #but the population increase is a categorical variable. One option is to use the R package 'clustMixType' for clustering mixed type data. 
        
        #We'll try applying this with 2:4 clusters. An important point to make is that regardless of the number of clusters used here, 
        #the model tends to assign low-risk years to the same cluster (Cluser #1 in the 2-cluster model, Cluster #1 in the 3-cluster model, Cluster #3 in the 4-cluster model).

        ind$increase =as.factor(ind$increase)
        k2 = kproto(dplyr::select(ind,fec_rate,surv_rate,peanut_rate,increase),2)
        k3 = kproto(dplyr::select(ind,fec_rate,surv_rate,peanut_rate,increase),3)
        k4 = kproto(dplyr::select(ind,fec_rate,surv_rate,peanut_rate,increase),4)
        ind$cluster_2 = as.factor(k2$cluster)
        ind$cluster_3 = as.factor(k3$cluster)
        ind$cluster_4 = as.factor(k4$cluster)
        
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
        
        #Output important data to excel sheets
        
        filePath <- file.path(tempdir(), "SRKW_Output_File.xlsx")
        
        write.xlsx(cohort_all_areas, file = filePath, sheetName = "Chinook cohorts", append = FALSE)
        write.xlsx(ind, file = filePath, sheetName = "SRKW Pop Indices", append = TRUE)
        
        #list of areas to explore
        Areas <- c("NOF", "OR", "CALI", "SOF", "COASTWIDE", "SALISH", "SWWCVI")
        
        fems_srkw_subsetyears <- subset(fems_srkw, year >= minYr & year <= maxYr)
        all_srkw_subsetyears <- subset(all_srkw, year >= minYr & year <= maxYr)
        peanut_subsetyears <- subset(peanut, year >= minYr & year <= maxYr)
        Ind_subsetyears <- subset(ind, year >= minYr & year <= maxYr)
        
        RegDat <- data.frame(Regression = as.character(), Area = as.character(), TimeStep = as.numeric(), Coef = as.numeric(), SE = as.numeric(), p_Value = as.numeric())
        
        #Processing loops through areas/time steps
        for (i in 1:length(Areas)){
          for (j in 1:3){
            
              if(Areas[i] == "SOF"){
                TempDat <- subset(cohort_all_areas, Area %in% c("OR","CALI") & TimeStep == j)
                TempDat <- summaryBy(abundance~year+TimeStep, data = TempDat, FUN = sum)
                colnames(TempDat)[colnames(TempDat)=="abundance.sum"] <- "abundance"
              }
              else if(Areas[i] == "COASTWIDE"){
                TempDat <- subset(cohort_all_areas, Area %in% c("OR","CALI","NOF") & TimeStep == j)
                TempDat <- summaryBy(abundance~year+TimeStep, data = TempDat, FUN = sum)
                colnames(TempDat)[colnames(TempDat)=="abundance.sum"] <- "abundance"
              }
              else{
                TempDat <- subset(cohort_all_areas, Area == Areas[i] & TimeStep == j)
              }
              #create an identifier to Timestep and area
              TSArea <- paste (Areas[i],"_TimeStep_",j, sep = "")
              
              #Merge Surv/Fec/Peanut/ind data sets with abundance data
              TempFecDat <- dplyr::left_join(TempDat, fems_srkw_subsetyears, TempDat, by="year")
              TempSurvDat <- dplyr::left_join(TempDat, all_srkw_subsetyears, TempDat, by="year")
              TempPeanutDat <- dplyr::left_join(TempDat, peanut_subsetyears, TempDat, by="year")
              TempIndDat <- dplyr::left_join(TempDat, Ind_subsetyears, TempDat, by="year")
  
              #model fecundity as a logistic regression. Eric recommended age as a quadratic.
              Fecundityglm <- glm(gave_birth~abundance + age + I(age^2), family = binomial, data = TempFecDat)
              
              #TESTTTTTT model fecundity with abundance scaled
              TempFecDat$Scaled_Ab <- scale(TempFecDat$abundance)
              Fecundityscaledglm <- glm(gave_birth~Scaled_Ab + age + I(age^2), family = binomial, data = TempFecDat)
              
              #predicting values along glm.
              Fecpreddat <- with(TempFecDat, data.frame(abundance = seq(min(abundance), max(abundance), len = 500), age = 20))
              Fecfit <-  predict(Fecundityglm, newdata = Fecpreddat, type = "link", se = TRUE)
              
              #plotting fecundity relationship
              q <- qt(0.975, df = df.residual(Fecundityglm))
              Fecplotdat <- cbind(Fecpreddat, fit = binomial()$linkinv(Fecfit$fit), 
                                  lower = binomial()$linkinv(Fecfit$fit - q * Fecfit$se.fit), 
                                  upper = binomial()$linkinv(Fecfit$fit + q * Fecfit$se.fit))
              
              FecPlot <- ggplot(data = Fecplotdat, aes(y = fit, x = abundance)) + geom_point(data = TempFecDat,
                                aes(y = gave_birth)) + geom_ribbon(aes(ymin = lower, ymax = upper),
                                fill = "gray", alpha = 0.3) + geom_line() + scale_y_continuous("Age 20 Females Giving Birth") +
                                scale_x_continuous(paste("Chinook Abundance in ", Areas[i], " TS ", j)) + theme_classic()
              
              #modeling survival relationship
              Survglm <- glm(alive ~ abundance + stage, family = binomial, data = TempSurvDat)
              
              #TESTTTTTT model survival with abundance scaled
              TempSurvDat$Scaled_Ab <- scale(TempSurvDat$abundance)
              Survscaledglm <- glm(alive ~ Scaled_Ab + stage, family = binomial, data = TempSurvDat)
              
              #predicting values along glm.
              Survpreddat <- with(TempSurvDat, data.frame(abundance = seq(min(abundance), max(abundance), len = 500), stage = "young_female"))
              Survfit <-  predict(Survglm, newdata = Survpreddat, type = "link", se = TRUE)
              
              #plotting survival relationship
              q <- qt(0.975, df = df.residual(Survglm))
              Survplotdat <- cbind(Survpreddat, fit = binomial()$linkinv(Survfit$fit), 
                                  lower = binomial()$linkinv(Survfit$fit - q * Survfit$se.fit), 
                                  upper = binomial()$linkinv(Survfit$fit + q * Survfit$se.fit))
              
              SurvPlot <- ggplot(data = Survplotdat, aes(y = fit, x = abundance)) + geom_ribbon(aes(ymin = lower, ymax = upper),
                                 fill = "gray", alpha = 0.3) + geom_line() + scale_y_continuous("Young Female SRKW Survival Rates", limits = c(.75,1)) +
                                 scale_x_continuous(paste("Chinook Abundance in ", Areas[i], " TS ", j)) + theme_classic()
              
              #modeling peanut head relationship, as per Eric's suggestion, using a Poisson distribution here
              Peanutglm = glm(peanut ~ abundance, data=TempPeanutDat, family=poisson)
              
              #TESTTTTTT model peanuts with abundance scaled
              TempPeanutDat$Scaled_Ab <- scale(TempPeanutDat$abundance)
              Peanutscaledglm = glm(peanut ~ Scaled_Ab, data=TempPeanutDat, family=poisson)
              
              #predicting values along glm.
              Peanutpreddat <- with(TempPeanutDat, data.frame(abundance = seq(min(abundance), max(abundance), len = 500)))
              Peanutfit <-  predict(Peanutglm, newdata = Peanutpreddat, type = "link", se = TRUE)
              
              #plotting peanut relationship
              q <- qt(0.975, df = df.residual(Peanutglm))
              Peanutplotdat <- cbind(Peanutpreddat, fit = binomial()$linkinv(Peanutfit$fit), 
                                   lower = binomial()$linkinv(Peanutfit$fit - q * Peanutfit$se.fit), 
                                   upper = binomial()$linkinv(Peanutfit$fit + q * Peanutfit$se.fit))
              
              PeanutPlot <- ggplot(data = Peanutplotdat, aes(y = fit, x = abundance)) + geom_ribbon(aes(ymin = lower, ymax = upper),
                                                                                                fill = "gray", alpha = 0.3) + geom_line() + scale_y_continuous("Occurrence of Peanut Head") +
                scale_x_continuous(paste("Chinook Abundance in ", Areas[i], " TS ", j)) + theme_classic()
              
              #Lag 1-year and 2-year Fecundity
              #For this section, we have to start the analysis at 1993/1994 (we don't have a 1991 abundance estimate to use to start at 1992)
              TempFecDat$LagOneAbundance <- NA
              TempFecDat$LagTwoAbundance <- NA
              for(k in 1:nrow(TempFecDat)){
                #if the year is one greater than the min year
                if (TempFecDat$year[k] > minYr){
                  TempFecDat$LagOneAbundance[k] <- subset(TempFecDat, year == TempFecDat$year[k]-1)$abundance[1]
                }
                #if the year is two greater than the min year
                if (TempFecDat$year[k] > minYr+1){
                  TempFecDat$LagTwoAbundance[k] <- subset(TempFecDat, year == TempFecDat$year[k]-2)$abundance[1]
                }
              }
              TempFecDatLagOne <- subset(TempFecDat, year > minYr)
              TempFecDatLagTwo <- subset(TempFecDat, year > minYr+1)

              #model fecundity as a logistic regression. Eric recommended age as a quadratic.
              FecundityLagOneglm <- glm(gave_birth~LagOneAbundance + age + I(age^2), family = binomial, data = TempFecDatLagOne)
              FecundityLagTwoglm <- glm(gave_birth~LagTwoAbundance + age + I(age^2), family = binomial, data = TempFecDatLagTwo)
              
              #TESTTTTTT model fec lags with abundance scaled
              TempFecDatLagOne$Scaled_Ab_Lag_One <- scale(TempFecDatLagOne$LagOneAbundance)
              FecundityscaledLagOneglm <- glm(gave_birth~Scaled_Ab_Lag_One + age + I(age^2), family = binomial, data = TempFecDatLagOne)
              TempFecDatLagTwo$Scaled_Ab_Lag_Two <- scale(TempFecDatLagTwo$LagTwoAbundance)
              FecundityscaledLagTwoglm <- glm(gave_birth~Scaled_Ab_Lag_Two + age + I(age^2), family = binomial, data = TempFecDatLagTwo)
              
              #predicting values along glm.
              FecLagOnepreddat <- with(TempFecDatLagOne, data.frame(LagOneAbundance = seq(min(LagOneAbundance), max(LagOneAbundance), len = 500), age = 20))
              FecLagOnefit <-  predict(FecundityLagOneglm, newdata = FecLagOnepreddat, type = "link", se = TRUE)
              
              #plotting fecundity lag relationships
              q <- qt(0.975, df = df.residual(FecundityLagOneglm))
              FecLagOneplotdat <- cbind(FecLagOnepreddat, fit = binomial()$linkinv(FecLagOnefit$fit), 
                                  lower = binomial()$linkinv(FecLagOnefit$fit - q * FecLagOnefit$se.fit), 
                                  upper = binomial()$linkinv(FecLagOnefit$fit + q * FecLagOnefit$se.fit))
              
              FeclagOnePlot <- ggplot(data = FecLagOneplotdat, aes(y = fit, x = LagOneAbundance)) + geom_point(data = TempFecDatLagOne,
                                       aes(y = gave_birth)) + geom_ribbon(aes(ymin = lower, ymax = upper),
                                       fill = "gray", alpha = 0.3) + geom_line() + scale_y_continuous("Age 20 Females Giving Birth") +
                                       scale_x_continuous(paste("Chinook Abundance in ", Areas[i], " TS ", j)) + theme_classic()
              
              #predicting values along glm.
              FecLagTwopreddat <- with(TempFecDatLagTwo, data.frame(LagTwoAbundance = seq(min(LagTwoAbundance), max(LagTwoAbundance), len = 500), age = 20))
              FecLagTwofit <-  predict(FecundityLagTwoglm, newdata = FecLagTwopreddat, type = "link", se = TRUE)
              
              #plotting fecundity lag relationships
              q <- qt(0.975, df = df.residual(FecundityLagTwoglm))
              FecLagTwoplotdat <- cbind(FecLagTwopreddat, fit = binomial()$linkinv(FecLagTwofit$fit), 
                                        lower = binomial()$linkinv(FecLagTwofit$fit - q * FecLagTwofit$se.fit), 
                                        upper = binomial()$linkinv(FecLagTwofit$fit + q * FecLagTwofit$se.fit))
              
              FeclagTwoPlot <- ggplot(data = FecLagTwoplotdat, aes(y = fit, x = LagTwoAbundance)) + geom_point(data = TempFecDatLagTwo,
                                      aes(y = gave_birth)) + geom_ribbon(aes(ymin = lower, ymax = upper),
                                      fill = "gray", alpha = 0.3) + geom_line() + scale_y_continuous("Age 20 Females Giving Birth") +
                                      scale_x_continuous(paste("Chinook Abundance in ", Areas[i], " TS ", j)) + theme_classic()
              
              #Lag 1-year survival
              #For this section, we have to start the analysis at 1993 (we don't have a 1991 abundance estimate to use to start at 1992)
              TempSurvDat$LagOneAbundance <- NA
              for(k in 1:nrow(TempSurvDat)){
                #if the year is one greater than the min year
                if (TempSurvDat$year[k] > minYr){
                  TempSurvDat$LagOneAbundance[k] <- subset(TempSurvDat, year == TempSurvDat$year[k]-1)$abundance[1]
                }
              }
              TempSurvDatLagOne <- subset(TempSurvDat, year > minYr)
              
              #model survival as above
              SurvLagOneglm <- glm(alive ~ LagOneAbundance + stage, family = binomial, data = TempSurvDatLagOne)
              
              #TESTTTTTT model surv lags with abundance scaled
              TempSurvDatLagOne$Scaled_Ab_Lag_One <- scale(TempSurvDatLagOne$LagOneAbundance)
              SurvscaledLagOneglm <- glm(alive ~ Scaled_Ab_Lag_One + stage, family = binomial, data = TempSurvDatLagOne)
              
              #predicting values along glm.
              SurvpreddatLagOne <- with(TempSurvDatLagOne, data.frame(LagOneAbundance = seq(min(LagOneAbundance), max(LagOneAbundance), len = 500), stage = "young_female"))
              SurvfitLagOne <-  predict(SurvLagOneglm, newdata = SurvpreddatLagOne, type = "link", se = TRUE)
              
              #plotting survival relationship
              q <- qt(0.975, df = df.residual(SurvLagOneglm))
              SurvplotdatLagOne <- cbind(SurvpreddatLagOne, fit = binomial()$linkinv(SurvfitLagOne$fit), 
                                   lower = binomial()$linkinv(SurvfitLagOne$fit - q * SurvfitLagOne$se.fit), 
                                   upper = binomial()$linkinv(SurvfitLagOne$fit + q * SurvfitLagOne$se.fit))
              
              SurvPlotLagOne <- ggplot(data = SurvplotdatLagOne, aes(y = fit, x = LagOneAbundance)) + geom_ribbon(aes(ymin = lower, ymax = upper),
                                fill = "gray", alpha = 0.3) + geom_line() + scale_y_continuous("Young Female SRKW Survival Rates", limits = c(.75,1)) +
                                scale_x_continuous(paste("One Year Lag Chinook Abundance in ", Areas[i], " TS ", j)) + theme_classic()
              
              #Cluster analysis
              #Chose to use 4 clusters
              
              k4abund <- kproto(dplyr::select(TempIndDat,fec_rate,surv_rate,peanut_rate,increase,abundance),4)
              TempIndDat$abundcluster_4 <- as.factor(k4abund$cluster)
              
              k4abund$centers = cbind("cluster"=c(1,2,3,4),k4abund$centers)
              
              FecRow <- data.frame(Regression = "Fecundity", Area = Areas[i], TimeStep = j, Coef = tidy(Fecundityscaledglm)$estimate[2], 
                                   SE = tidy(Fecundityscaledglm)$std.error[2], p_Value = tidy(Fecundityscaledglm)$p.value[2])
              SurvRow <- data.frame(Regression = "Survival", Area = Areas[i], TimeStep = j, Coef = tidy(Survscaledglm)$estimate[2], 
                                   SE = tidy(Survscaledglm)$std.error[2], p_Value = tidy(Survscaledglm)$p.value[2])
              PeanutRow <- data.frame(Regression = "Peanut", Area = Areas[i], TimeStep = j, Coef = tidy(Peanutscaledglm)$estimate[2], 
                                      SE = tidy(Peanutscaledglm)$std.error[2], p_Value = tidy(Peanutscaledglm)$p.value[2])
              FecLag1Row <- data.frame(Regression = "Fecundity Lag 1", Area = Areas[i], TimeStep = j, Coef = tidy(FecundityscaledLagOneglm)$estimate[2], 
                                      SE = tidy(FecundityscaledLagOneglm)$std.error[2], p_Value = tidy(FecundityscaledLagOneglm)$p.value[2])
              FecLag2Row <- data.frame(Regression = "Fecundity Lag 2", Area = Areas[i], TimeStep = j, Coef = tidy(FecundityscaledLagTwoglm)$estimate[2], 
                                       SE = tidy(FecundityscaledLagTwoglm)$std.error[2], p_Value = tidy(FecundityscaledLagTwoglm)$p.value[2])
              SurvLag1Row <- data.frame(Regression = "Survival Lag 1", Area = Areas[i], TimeStep = j, Coef = tidy(SurvscaledLagOneglm)$estimate[2], 
                                       SE = tidy(SurvscaledLagOneglm)$std.error[2], p_Value = tidy(SurvscaledLagOneglm)$p.value[2])
              
              RegDat <- rbind(RegDat, FecRow, SurvRow, PeanutRow, FecLag1Row, FecLag2Row, SurvLag1Row)
              
              #Now add plot and outputs into the Excel file
              write.xlsx(tidy(Fecundityglm), file = filePath, sheetName = TSArea, append = TRUE)
              
              #Set up plot path - Fecundity
              PlotPath <- file.path(tempdir(), paste(TSArea,".jpeg", sep=""))
              png(filename = PlotPath)
              plot(FecPlot)
              dev.off()
              
              #Sets up workbook path/output
              wb <- loadWorkbook(filePath)
              ws <- getSheets(wb)[TSArea][[1]]
              addPicture(file = PlotPath, sheet = ws, startRow = 2, startColumn = 8)
              addDataFrame(tidy(Survglm), sheet = ws, startRow = 28, startColumn = 1)
              addDataFrame(tidy(Peanutglm), sheet = ws, startRow = 54, startColumn = 1)
              addDataFrame(tidy(FecundityLagOneglm), sheet = ws, startRow = 80, startColumn = 1)
              addDataFrame(tidy(FecundityLagTwoglm), sheet = ws, startRow = 106, startColumn = 1)
              addDataFrame(k4abund$centers, sheet = ws, startRow = 132, startColumn = 1)
              addDataFrame(tidy(SurvLagOneglm), sheet = ws, startRow = 158, startColumn = 1)
              
              addDataFrame(tidy(Fecundityscaledglm), sheet = ws, startRow = 170, startColumn = 1)
              addDataFrame(tidy(Survscaledglm), sheet = ws, startRow = 182, startColumn = 1)
              addDataFrame(tidy(Peanutscaledglm), sheet = ws, startRow = 194, startColumn = 1)
              addDataFrame(tidy(FecundityscaledLagOneglm), sheet = ws, startRow = 206, startColumn = 1)
              addDataFrame(tidy(FecundityscaledLagTwoglm), sheet = ws, startRow = 218, startColumn = 1)
              addDataFrame(tidy(SurvscaledLagOneglm), sheet = ws, startRow = 230, startColumn = 1)

              unlink(PlotPath)
              
              #Set up plot path - Survival
              PlotPath <- file.path(tempdir(), paste(TSArea,".jpeg", sep=""))
              png(filename = PlotPath)
              plot(SurvPlot)
              dev.off()
              
              addPicture(file = PlotPath, sheet = ws, startRow = 28, startColumn = 8)
              
              saveWorkbook(wb, filePath)
              unlink(PlotPath)
              
              #Set up plot path - Peanut
              PlotPath <- file.path(tempdir(), paste(TSArea,".jpeg", sep=""))
              png(filename = PlotPath)
              plot(PeanutPlot)
              dev.off()
              
              addPicture(file = PlotPath, sheet = ws, startRow = 54, startColumn = 8)
              
              saveWorkbook(wb, filePath)
              unlink(PlotPath)
              
              #Set up plot path - Lag One Fecundity
              PlotPath <- file.path(tempdir(), paste(TSArea,".jpeg", sep=""))
              png(filename = PlotPath)
              plot(FeclagOnePlot)
              dev.off()
              
              addPicture(file = PlotPath, sheet = ws, startRow = 80, startColumn = 8)
              
              saveWorkbook(wb, filePath)
              unlink(PlotPath)
              
              #Set up plot path - Lag Two Fecundity
              PlotPath <- file.path(tempdir(), paste(TSArea,".jpeg", sep=""))
              png(filename = PlotPath)
              plot(FeclagOnePlot)
              dev.off()
              
              addPicture(file = PlotPath, sheet = ws, startRow = 106, startColumn = 8)
              
              saveWorkbook(wb, filePath)
              unlink(PlotPath)
              
              
              #Set up plot path - Lag One Survival
              PlotPath <- file.path(tempdir(), paste(TSArea,".jpeg", sep=""))
              png(filename = PlotPath)
              plot(SurvPlotLagOne)
              dev.off()
              
              addPicture(file = PlotPath, sheet = ws, startRow = 158, startColumn = 8)
              
              saveWorkbook(wb, filePath)
              unlink(PlotPath)
              
            
          }
        }
        
        write.xlsx(RegDat, file = filePath, sheetName = "Regression Sum", append = TRUE)
        
        #sends a mail from my dummy email address.
        send.mail(from = "pfmc.srkw@gmail.com",
                  to = input$EmailAdd,
                  subject = "SRKW Output Data",
                  body = "Please see the attached for the SRKW model results.",
                  #the email's password is hidden in a file that is rolled into the server upon hosting
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "pfmc.srkw@gmail.com", passwd = Password, ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE,
                  
                  attach.files = c(filePath),
                  file.descriptions = c("SRKW Output File"),
                  debug = TRUE)
        
        # These get set to global variables for table viewing/testing purposes
        cohort_NOF <<- cohort_NOF
        cohort_OR <<- cohort_OR
        cohort_CALI <<- cohort_CALI
        cohort_SWWCVI <<- cohort_SWWCVI
        
        #Graphical Outputs ----
        FecundityPlot <<- ggplot(df_fec, aes(year,fec_rate)) + 
          geom_line() + 
          xlab("Year") + 
          ylab("Estimated fecundity rate") + 
          geom_ribbon(aes(ymin=fec_rate-1.96*se,
                          ymax=fec_rate+1.96*se), alpha=0.3)
        
        SurvivalPlot <<- ggplot(df_surv, aes(year,surv_rate)) + 
          geom_line() + 
          xlab("Year") + 
          ylab("Estimated survival rate") + 
          geom_ribbon(aes(ymin=surv_rate-1.96*se, 
                          ymax=surv_rate+1.96*se), alpha=0.3)
        
        IncreasePlot <<- ggplot(srkw_increase, aes(year,total,col=increase)) + 
          geom_point() + 
          ylab("SRKW population size") + 
          xlab("Year")
        
        
        
      })
      }
      
    })
  })
  
  output$Plot1 <- renderPlot({
    #Display the PFMC logo 
    plot_jpeg('PFMCLogo.jpg')
  })
  
  #Commented out as I use this section for testing.
  
  #The ProcessingButton if statements and BlankDF2 checks are to display an error message if a user tries to view a table prior to processing
  # output$Table <- renderDataTable({
  #   #Display a blank data frame if no table is selected
  #   if(input$table == "None"){
  #     BlankDF
  #   }
  #   else if (input$table == "Input - FRAM Stocks"){
  #     if(input$ProcessButton != 0){
  #       InputDataList[[1]]
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "SOF Stock Abundances - Total"){
  #     if(input$ProcessButton != 0){
  #       SOFabundances
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "All Stock Abundances - Total"){
  #     if(input$ProcessButton != 0){
  #       cohort_Shelton_Stk
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "Stock Abundances by Area"){
  #     if(input$ProcessButton != 0){
  #       cohort_by_area
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "NOF Abundances by Year"){
  #     if(input$ProcessButton != 0){
  #       cohort_NOF
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "OR Abundances by Year"){
  #     if(input$ProcessButton != 0){
  #       cohort_OR
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "CALI Abundances by Year"){
  #     if(input$ProcessButton != 0){
  #       cohort_CALI
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "SWWCVI Abundances by Year"){
  #     if(input$ProcessButton != 0){
  #       cohort_SWWCVI
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "SRKW Fecundity Rates"){
  #     if(input$ProcessButton != 0){
  #       df_fec_display
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "SRKW Survival Rates"){
  #     if(input$ProcessButton != 0){
  #       df_surv_display
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  #   else if (input$table == "SRKW Peanut Head Data"){
  #     if(input$ProcessButton != 0){
  #       peanut
  #     }
  #     else{
  #       BlankDF2
  #     }
  #   }
  # })

}

# UI Code ----

ui <- fluidPage(
  headerPanel("PFMC SRKW-Chinook Abundance Evaluation Tool"),
  sidebarLayout(
    sidebarPanel(
      textInput("EmailAdd", "Email Address:", ""),
      actionButton("EmailButton",label = "Send Input File to my Email"),
      fileInput("InputFile", "Choose Input File (.xlsx)", accept = c(".xlsx")),
      selectInput("UpColSpr", "Include Natural Mortality for Upriver Columbia Springs?", choice = c("Yes", "No")),
      actionButton("ProcessButton",label = "Begin Processing/Email Outputs")
      #Commented out as I use this section for testing.
      # actionButton("ProcessButton",label = "Begin Processing/Email Outputs"),
      # selectInput("table", "Please choose a table to display",
      #             choices = c("None","Input - FRAM Stocks", "SOF Stock Abundances - Total", "All Stock Abundances - Total", 
      #                         "Stock Abundances by Area", "NOF Abundances by Year", "OR Abundances by Year", "CALI Abundances by Year",
      #                         "SWWCVI Abundances by Year", "SRKW Fecundity Rates", "SRKW Survival Rates", "SRKW Peanut Head Data"))

    ),
    #Main panel is for graphics
    mainPanel(
      #outputs a plot
      plotOutput("Plot1"),
      
      
      #outputs a table
      dataTableOutput("Table")
    )
  )
)

shinyApp(ui = ui, server = server)