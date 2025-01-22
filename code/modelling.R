# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #


### Initialise
require(apollo)
require(dplyr)
apollo_initialise()


### Set core controls
apollo_control = list(
  modelName  ="int_mnl_PT_04",
  modelDescr ="Simple MNL on Gender safety data;
              Model with income and age;
              Considering same cofficient for time and cost",
  indivID    ="id",
  outputDirectory = "results/"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
database <- read.csv('data/SP_Gendersafety.csv')

# Data wrangling to generate PT and non-PT users
database$PT_users <- ifelse(database$Primary_mode %in% c(1,2),1,0)

database$saccstop_bus[database$saccstop_bus=="Active and well-lit streets and footpaths"] <- 1
database$saccstop_bus[database$saccstop_bus=="Clear signages and boards"] <- 2
database$saccstop_bus[database$saccstop_bus=="Access infrastructure as of now"] <- 3
database$saccstop_bus <-as.numeric(database$saccstop_bus)

database$saccstop_metro[database$saccstop_metro=="Active and well-lit streets and footpaths"] <- 1
database$saccstop_metro[database$saccstop_metro=="Clear signages and boards"] <- 2
database$saccstop_metro[database$saccstop_metro=="Access infrastructure as of now"] <- 3
database$saccstop_metro <- as.numeric(database$saccstop_metro)

database$sboal_bus[database$sboal_bus=="Boarding/alighting in crowded conditions"] <- 1
database$sboal_bus[database$sboal_bus=="Passengers following queue behaviour while boarding"] <- 2
database$sboal_bus <- as.numeric(database$sboal_bus)

database$sboal_metro[database$sboal_metro=="Boarding/alighting in crowded conditions"] <- 1
database$sboal_metro[database$sboal_metro=="Passengers following queue behaviour while boarding"] <- 2
database$sboal_metro <- as.numeric(database$sboal_metro)

database$swaitenv_bus[database$swaitenv_bus=="Other people present at the stop"] <- 1
database$swaitenv_bus[database$swaitenv_bus=="Staff/police presence or help booths in public transport stops"] <- 2
database$swaitenv_bus[database$swaitenv_bus=="Waiting alone"] <- 3
database$swaitenv_bus <- as.numeric(database$swaitenv_bus)

database$swaitenv_metro[database$swaitenv_metro=="Other people present at the stop"] <- 1
database$swaitenv_metro[database$swaitenv_metro=="Staff/police presence or help booths in public transport stops"] <- 2
database$swaitenv_metro[database$swaitenv_metro=="Waiting alone"] <- 3
database$swaitenv_metro <- as.numeric(database$swaitenv_metro)

database$safety_bus[database$safety_bus=="Seating space available"] <- 1
database$safety_bus[database$safety_bus=="Comfortable standing with adequate grab handles for support"] <- 2
database$safety_bus[database$safety_bus=="Crowded standing"] <- 3
database$safety_bus <- as.numeric(database$safety_bus)

database$safety_metro[database$safety_metro=="Seating space available"] <- 1
database$safety_metro[database$safety_metro=="Comfortable standing with adequate grab handles for support"] <- 2
database$safety_metro[database$safety_metro=="Crowded standing"] <- 3
database$safety_metro <- as.numeric(database$safety_metro)

database$av_bus <- TRUE
database$av_metro <- TRUE
database$av_others <- TRUE

# Converting categories to numeric values

# Assign random values to each category based on the respective income range
convert_to_numeric <- function(category) {
  if (category == 1) {
    return(runif(1, min = 15000, max = 25000))
  } else if (category == 2) {
    return(runif(1, min = 25000, max = 50000))
  } else if (category == 3) {
    return(runif(1, min = 50000, max = 100000))
  } else if (category == 4) {
    return(runif(1, min = 100000, max = 150000))
  } else if (category == 5) {
    return(runif(1, min = 150000, max = 200000))
    }
}

database$HH_Inc_num <- sapply(database$HH_Inc,convert_to_numeric)

database$relInc <- database$HH_Inc_num/mean(database$HH_Inc_num)

database$relAge <- database$Age/mean(database$Age)

database$t_bus <- 1.49*database$at_bus + 1.83*database$wt_bus + database$tt_bus
database$t_metro <- 1.49*database$at_metro + 1.83*database$wt_metro + database$tt_metro

database_PT <-database[database$PT_users==1,]
database_nonPT <-database[database$PT_users==0,]

# database <- database %>%
#   filter(Choice!="None")


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #


### Parameters (including fixed)
# apollo_beta=c(asc_bus = 1, asc_metro = 1, asc_others = 1, 
#               bAT = 1, bWT = 1, bTT = 1, bTC = 1, 
#               bCro= 1, bWaitEnv= 1, bStop = 1, bSafety = 1, 
#               mAT = 1, mWT = 1, mTT = 1, mTC = 1, 
#               mCro= 1, mWaitEnv= 1, mStop = 1, mSafety = 1)

# No modification of travel time, access time and waiting time
# apollo_beta=c(asc_bus = 0, asc_metro = 0, asc_others = 0,
#               bAT = 0, bWT = 0, bTT = 0, bTC = 0,
#               bCro= 0, bWaitEnv1= 0,bWaitEnv2= 0,
#               bStop1 = 0, bStop2 = 0,
#               bSafety1 = 0,bSafety2 = 0,
#               mAT = 0, mWT = 0, mTT = 0, mTC = 0,
#               mCro= 0, mWaitEnv1= 0, mWaitEnv2= 0,
#               mStop1 = 0, mStop2 = 0,
#               mSafety1 = 0, mSafety2 = 0)

# Modified travel time equation in line 73 & 74. Interacting travel time and travel cost with household income
# apollo_beta=c(asc_bus = 0, asc_metro = 0, asc_others = 0,
#               bTInc=0, bCost = 0, bCro= 0, bWaitEnv1= 0,bWaitEnv2= 0,
#               bStop1 = 0, bStop2 = 0,
#               bSafety1 = 0,bSafety2 = 0,
#               mCro= 0, mWaitEnv1= 0, mWaitEnv2= 0,
#               mStop1 = 0, mStop2 = 0,
#               mSafety1 = 0, mSafety2 = 0)

apollo_beta=c(asc_bus = 0, asc_metro = 0, asc_others = 0,
              bTInc=0, bCost = 0, bCro= 0, 
              bWaitEnv1= 0, bWaitEnv2= 0,
              bStop1 = 0, bStop2 = 0,
              bSafety1 = 0,bSafety2 = 0)

# apollo_beta=c(asc_bus = 0, asc_metro = 0, asc_others = 0,
#               bTInc=0, bCost = 0, bCro= 0, bStWa1=0,bStWa2=0,
#               bStWa3=0,bStWa4=0,bStWa5=0,bStWa6=0,
#               bStWa7=0, bStWa8=0, bStWa9=0,
#               bSafety1 = 0,bSafety2 = 0)

# apollo_beta=c(asc_bus = 0, asc_metro = 0, bAT = -1, bWT = -1, bTT = -1, bTC = -1,
#               mAT = -1, mWT = -1, mTT = -1, mTC = -1)


### Name of fixed params
apollo_fixed = c('asc_bus')

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #


# apollo_inputs = apollo_validateInputs(database = database)
apollo_inputs = apollo_validateInputs(database = database_PT)


# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #


apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  ### Initialise
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  ### MNL with modified interactions of travel time and hh income and travel cost and hh income
  
  ### Utility equation for PT users
  # tInc <- relInc^3
  # V = list(
  #   bus = asc_bus + bTInc*tInc*t_bus + bCost*(0.5/relInc)*tc_bus+bCro*(sboal_bus==2)+
  #     bWaitEnv1*(swaitenv_bus ==1) + bWaitEnv2*(swaitenv_bus ==2)+
  #     bStop1*(saccstop_bus==1) + bStop2*(saccstop_bus==2) +
  #     bSafety1*(safety_bus==1) + bSafety2*(safety_bus==2),
  #   metro = asc_metro + bTInc*tInc*t_metro + bCost*(0.5/relInc)*tc_metro+ mCro*(sboal_metro==2) +
  #     mWaitEnv1*(swaitenv_metro ==1) + mWaitEnv2*(swaitenv_metro ==2)+      mStop1*(saccstop_metro ==1) + mStop2*(saccstop_metro ==2) +
  #     mSafety1*(safety_metro==1) + mSafety2*(safety_metro==2),
  #   others = asc_others)
  tInc <- relInc^3
  V = list(
    bus = asc_bus + bTInc*tInc*t_bus + bCost*(0.5/relInc)*tc_bus+bCro*relInc*(sboal_bus==2)+
      bWaitEnv1*(swaitenv_bus ==1) + bWaitEnv2*(swaitenv_bus ==2)+
      bStop1*(saccstop_bus==1) + bStop2*(saccstop_bus==2) +
      bSafety1*(safety_bus==1) + bSafety2*(safety_bus==2),
    metro = asc_metro + bTInc*tInc*t_metro + bCost*(0.5/relInc)*tc_metro+ bCro*relInc*(sboal_metro==2) +
      bWaitEnv1*(swaitenv_metro ==1) + bWaitEnv2*(swaitenv_metro ==2)+
      bStop1*(saccstop_metro ==1) + bStop2*(saccstop_metro ==2) +
      bSafety1*(safety_metro==1) + bSafety2*(safety_metro==2),
    others = asc_others)
  
  ### Utility equation for non-PT users
  ### Equations without interaction variables and considering the same coefficients across different modes
  # tInc <- relInc^3
  # V = list(
  #   bus = asc_bus + bTInc*log(tInc)*t_bus + bCost*log(relInc**0.5)*tc_bus+bCro*(sboal_bus==2) +
  #     bWaitEnv1*(swaitenv_bus ==1) + bWaitEnv2*(swaitenv_bus ==2)+
  #     bStop1*(saccstop_bus==1) + bStop2*(saccstop_bus==2) +
  #     bSafety1*(safety_bus==1) + bSafety2*(safety_bus==2),
  #   metro = asc_metro + bTInc*log(tInc)*t_metro + bCost*log(relInc**0.5)*tc_metro+ mCro*(sboal_metro==2) +
  #     mWaitEnv1*(swaitenv_metro ==1) + mWaitEnv2*(swaitenv_metro ==2)+
  #     mStop1*(saccstop_metro ==1) + mStop2*(saccstop_metro ==2) +
  #     mSafety1*(safety_metro==1) + mSafety2*(safety_metro==2),
  #   others = asc_others)
  ### Equations with interaction variables and considering the same coefficients across different modes
  # tInc <- relInc^3
  # V = list(
  #   bus = asc_bus + bTInc*log(tInc)*t_bus + bCost*log(relInc**0.5)*tc_bus+bCro*(sboal_bus==2) +
  #     bStWa1*(saccstop_bus==1)*(swaitenv_bus ==1) + bStWa2*(saccstop_bus==2)*(swaitenv_bus ==1) +bStWa3*(saccstop_bus==3)*(swaitenv_bus ==1) +
  #     bStWa4*(saccstop_bus==1)*(swaitenv_bus ==2) + bStWa5*(saccstop_bus==2)*(swaitenv_bus ==2) +bStWa6*(saccstop_bus==3)*(swaitenv_bus ==2) +
  #     bStWa7*(saccstop_bus==1)*(swaitenv_bus ==3) +bStWa8*(saccstop_bus==2)*(swaitenv_bus ==3) +bStWa9*(saccstop_bus==3)*(swaitenv_bus ==3) +
  #     bSafety1*(safety_bus==1) + bSafety2*(safety_bus==2),
  #   metro = asc_metro + bTInc*log(tInc)*t_metro + bCost*log(relInc**0.5)*tc_metro+ bCro*(sboal_metro==2) +
  #     bStWa1*(saccstop_metro==1)*(swaitenv_metro ==1) + bStWa2*(saccstop_metro==2)*(swaitenv_metro ==1) +bStWa3*(saccstop_metro==3)*(swaitenv_metro ==1) +
  #     bStWa4*(saccstop_metro==1)*(swaitenv_metro ==2) + bStWa5*(saccstop_metro==2)*(swaitenv_metro ==2) +bStWa6*(saccstop_metro==3)*(swaitenv_metro ==2) +
  #     bStWa7*(saccstop_metro==1)*(swaitenv_metro ==3) +bStWa8*(saccstop_metro==2)*(swaitenv_metro ==3) +bStWa9*(saccstop_metro==3)*(swaitenv_metro ==3)+
  #     bSafety1*(safety_metro==1) + bSafety2*(safety_metro==2),
  #   others = asc_others)
  
  # tInc <- relInc^3
  # V = list(
  #   bus = asc_bus + bTInc*log(tInc)*t_bus + bCost*log(relInc**0.5)*tc_bus+bCro*(sboal_bus==2) +
  #     bWaitEnv1*(swaitenv_bus ==1) + bWaitEnv1*(swaitenv_bus ==2)+
  #     bStop1/relInc*(saccstop_bus==1) + bStop1/relInc*(saccstop_bus==2) +
  #     bSafety1*(safety_bus==1) + bSafety2*(safety_bus==2),
  #   metro = asc_metro + bTInc*log(tInc)*t_metro + bCost*log(relInc**0.5)*tc_metro+ bCro*(sboal_metro==2) +
  #     bWaitEnv1*(swaitenv_metro ==1) + bWaitEnv1*(swaitenv_metro ==2)+
  #     bStop1/relInc*(saccstop_metro ==1) + bStop1/relInc*(saccstop_metro ==2) +
  #     bSafety1*(safety_metro==1) + bSafety2*(safety_metro==2),
  #   others = asc_others)
  
  
  mnl_settings = list(
    alternatives  = c(bus="Bus", metro="Metro",others="None"),
    avail         = list(bus=av_bus, metro=av_metro,others=av_others),
    choiceVar     = Choice,
    V             = V
  )
  # V = list(
  #   bus = asc_bus + bAT*at_bus + bWT*wt_bus + bTT*tt_bus + bTC*tc_bus,
  #   metro = asc_metro + mAT*at_metro + mWT*wt_metro + mTT*tt_metro + mTC*tc_metro)
  # mnl_settings = list(
  #   alternatives  = c(bus="Bus", metro="Metro",others="None"),
  #   avail         = list(bus=av_bus, metro=av_metro,others=av_others),
  #   choiceVar     = Choice,
  #   V             = V
  # )
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Comment out as necessary
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION & OUTPUT                                   ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(writeIter=FALSE))


apollo_modelOutput(model)


apollo_saveOutput(model, list(saveEst=FALSE, saveCov=FALSE, 
                              saveCorr=FALSE, saveModelObject=FALSE))


apollo_deltaMethod(model, list(operation='ratio', parName1='bTInc', 
                               parName2='bCost')) # 1.87 INR/min


# # # # # # #
#### WTP ####
# # # # # # #
alts <- c("Bus", "Metro", "None")
inc  <- c(low=18000, mid=50000, upp=118750)
inc  <- round(inc/mean(database_PT$HH_Inc_num),4)

B <- names(apollo_beta)
B <- B[!(B %in% c(apollo_fixed, "bCost", grep("^[m|l]", B, value=TRUE)))]
M <- length(B)*length(inc)
M <- data.frame(expression= rep("", M), 
                q.025= rep(0, M), mean= rep(0, M), q.975= rep(0, M))
dbP <- data.frame(attribute=rep("", 10), value=rep(0, 10), sd=rep(0, 10))

for(i in 1:length(inc)){
  e <- paste0("-1*", B, "/(bCost*(","0.5/", inc[i], "))")
  e <- paste0(ifelse(grepl("TI", B), "60*", ""), e)
  e <- paste0(ifelse(grepl("Cro", B), paste0(inc[i], "*"), ""), e)
  e <- apollo_deltaMethod(model, list(expression=e))
  tmp <- (i-1)*length(B)
  tmp <- (tmp + 1):(tmp + length(B))
  M[tmp, "expression"] <- paste0(B, ".", names(inc)[i])
  M[tmp, "mean"      ] <- e$Value
  M[tmp, "q.025"     ] <- e$Value - 1.96*e$s.e.
  M[tmp, "q.975"     ] <- e$Value + 1.96*e$s.e.
}
print(M[order(M$expression),], digits=4)

write.csv(M,"./results/excel_outputs/WTP.csv")
