# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #


### Initialise
require(apollo)
require(dplyr)
require(reshape2)
require(ggplot2)
apollo_initialise()


### Set core controls
apollo_control = list(
  modelName  ="int_mnl_nonPT_522025",
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
              bWaitEnv1= 0,bStop1 = 0, bSafety1 = 0,
              bSafety2=0)

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
apollo_inputs = apollo_validateInputs(database = database_nonPT)


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
  # tInc <- relInc^3
  # V = list(
  #   bus = asc_bus + bTInc*tInc*t_bus + bCost*(0.5/relInc)*tc_bus+bCro*relInc*(sboal_bus==2)+
  #     bWaitEnv1*(swaitenv_bus ==1) + bWaitEnv2*(swaitenv_bus ==2)+
  #     bStop1*(saccstop_bus==1) + bStop2*(saccstop_bus==2) +
  #     bSafety1*(safety_bus==1) + bSafety2*(safety_bus==2),
  #   metro = asc_metro + bTInc*tInc*t_metro + bCost*(0.5/relInc)*tc_metro+ bCro*relInc*(sboal_metro==2) +
  #     bWaitEnv1*(swaitenv_metro ==1) + bWaitEnv2*(swaitenv_metro ==2)+
  #     bStop1*(saccstop_metro ==1) + bStop2*(saccstop_metro ==2) +
  #     bSafety1*(safety_metro==1) + bSafety2*(safety_metro==2),
  #   others = asc_others)
  
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
  
  tInc <- relInc^3
  V = list(
    bus = asc_bus + bTInc*log(tInc)*t_bus + bCost*log(relInc**0.5)*tc_bus+bCro*(sboal_bus==2) +
      bWaitEnv1*(swaitenv_bus ==1) + bWaitEnv1*(swaitenv_bus ==2)+
      bStop1/relInc*(saccstop_bus==1) + bStop1/relInc*(saccstop_bus==2) +
      bSafety1*(safety_bus==1) + bSafety2*(safety_bus==2),
    metro = asc_metro + bTInc*log(tInc)*t_metro + bCost*log(relInc**0.5)*tc_metro+ bCro*(sboal_metro==2) +
      bWaitEnv1*(swaitenv_metro ==1) + bWaitEnv1*(swaitenv_metro ==2)+
      bStop1/relInc*(saccstop_metro ==1) + bStop1/relInc*(saccstop_metro ==2) +
      bSafety1*(safety_metro==1) + bSafety2*(safety_metro==2),
    others = asc_others)

  
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
# inc  <- c(low=120000, mid=150000, upp=180000)
inc  <- c(low=150000, mid=175000, upp=200000)
inc  <- round(inc/mean(database_nonPT$HH_Inc_num),4)

B <- names(apollo_beta)
B <- B[!(B %in% c(apollo_fixed, "bCost", grep("^[m|l]", B, value=TRUE)))]
M <- length(B)*length(inc)
M <- data.frame(expression= rep("", M), 
                q.025= rep(0, M), mean= rep(0, M), q.975= rep(0, M))
dbP <- data.frame(attribute=rep("", 10), value=rep(0, 10), sd=rep(0, 10))

for(i in 1:length(inc)){
  e <- paste0("-1*", B, "/(bCost*(","log(", inc[i],"**0.5", ")))")
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

write.csv(M,"./results/excel_outputs/WTP_nonPT.csv")

# # # # # # #
#### AME ####
# # # # # # #

pred <- apollo_prediction(model, apollo_probabilities, apollo_inputs)
pred <- pred[,-which(colnames(pred)=='chosen'),]
pred$hInc <- database_nonPT$HH_Inc_num>((150000+180000)/2)
hist(pred$metro[pred$hInc])
round(quantile(pred$metro[pred$hInc], probs=c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, .95)), 2)
mean(pred$metro[pred$hInc])
sd(pred$metro[pred$hInc])

ame <- function(db0, db1){
  apIn <- apollo_validateInputs(database=db0, silent=TRUE)
  pred0 <- apollo_prediction(model, apollo_probabilities, apIn,
                             prediction_settings=list(runs=100))[['draws']]
  pred0 <- pred0[,-which(colnames(pred0)=='chosen'),]
  apIn <- apollo_validateInputs(database=db1, silent=TRUE)
  pred1 <- apollo_prediction(model, apollo_probabilities, apIn,
                             prediction_settings=list(runs=100))[['draws']]
  pred1 <- pred1[,-which(colnames(pred1)=='chosen'),]
  av <- pred0[,,1]!=0 & pred1[,,1]!=0
  tmp <- matrix(0, nrow=dim(pred0)[3], ncol=dim(pred0)[2], 
                dimnames=list(NULL, colnames(pred0)))
  for(k in 1:nrow(tmp)) for(j in colnames(tmp)) tmp[k,j] <- mean(pred1[av[,j],j,k] - pred0[av[,j],j,k], na.rm=TRUE)
  ans <- matrix(0, nrow=3, ncol=dim(pred0)[2],
                dimnames=list(c('q.025', 'mean', 'q.975'), colnames(pred0)))
  for(j in colnames(tmp)) ans[,j] <- quantile(tmp[,j], probs=c(0.025, 0.5, 0.975), na.rm=TRUE)
  ans['mean',] <- colMeans(tmp, na.rm=TRUE)
  return(100*ans)
}

alts <- c("bus", "metro", "others")
inc  <- c(low=150000, mid=175000, high=200000)
M <- matrix(0, nrow=length(inc), ncol=3*length(alts), 
            dimnames=list(names(inc), 
                          paste0(rep(alts, each=3), ".q", c("L", "M", "H"))))

### AME of access time 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db1 <- db0
    db1[,paste0("t_", a)] <- db1[,paste0("t_", a)] + 1.49*5
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

### AME of waiting time 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db1 <- db0
    db1[,paste0("t_", a)] <- db1[,paste0("t_", a)] + 1.83*10
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)


### AME of travel time 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db1 <- db0
    db1[,paste0("t_", a)] <- db1[,paste0("t_", a)] + 10
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

### AME of access to stop = 1 (Active and well-lit streets) 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db0[, paste0("saccstop_", a)] <- 3
    db1 <- db0
    db1[, paste0("saccstop_", a)] <- 1
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

### AME of access to stop = 2 (clear signages & boards) 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db0[, paste0("saccstop_", a)] <- 3
    db1 <- db0
    db1[, paste0("saccstop_", a)] <- 2
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

### AME of boarding/alighting = 2 (Boarding/Alighting in queuing order) 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db0[, paste0("sboal_", a)] <- 1
    db1 <- db0
    db1[, paste0("sboal_", a)] <- 2
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)


### AME of waiting environment = 1 ("Other people present at the stop") 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db0[, paste0("swaitenv_", a)] <- 3
    db1 <- db0
    db1[, paste0("swaitenv_", a)] <- 1
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

### AME of waiting environment = 2 ("Staff/Police Presence") 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db0[, paste0("swaitenv_", a)] <- 3
    db1 <- db0
    db1[, paste0("swaitenv_", a)] <- 2
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

### AME of on-board safety = 1 ("Seating Space available") 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db0[, paste0("safety_", a)] <- 3
    db1 <- db0
    db1[, paste0("safety_", a)] <- 1
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

### AME of on-board safety = 2 ("Comfortable Standing") 
for(i in names(inc)){
  db0 <- database_nonPT
  db0$relInc <- inc[i]/mean(database_nonPT$HH_Inc_num)
  for(a in alts[1:2]){
    db0[, paste0("safety_", a)] <- 3
    db1 <- db0
    db1[, paste0("safety_", a)] <- 2
    M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
  }
}; round(M, 3)

########################################
#### AME Estimation & Visualisation ####
########################################

# Function to calculate AME for continuous variable increases
calculate_ame <- function(attribute="t_",value=1,increase) {
  M <- matrix(0, nrow=length(inc), ncol=3*length(alts[1:2]), 
              dimnames=list(names(inc), 
                            paste0(rep(alts[1:2], each=3), ".q", c("L", "M", "H"))))
  for (i in names(inc)) {
    db0 <- database_nonPT
    db0$relInc <- inc[i] / mean(database_nonPT$HH_Inc_num)
    for (a in alts[1:2]) {
      db1 <- db0
      db1[, paste0(attribute, a)] <- db1[, paste0(attribute, a)] + value * increase
      M[i, paste0(a, ".q", c("L", "M", "H"))] <- ame(db0, db1)[, a]
    }
  }
  return(round(M, 3))
}

# Function to calculate AME for factor variable increases
calculate_ame_factor <- function(attribute="t_",value=1,increase) {
  M <- matrix(0, nrow=length(inc), ncol=3*length(alts[1:2]), 
              dimnames=list(names(inc), 
                            paste0(rep(alts[1:2], each=3), ".q", c("L", "M", "H"))))
  for (i in names(inc)) {
    db0 <- database_nonPT
    db0$relInc <- inc[i] / mean(database_nonPT$HH_Inc_num)
    for (a in alts[1:2]) {
      db0[, paste0(attribute, a)] <- value
      db1 <- db0
      db1[, paste0(attribute, a)] <- increase
      M[i, paste0(a,  ".q", c("L", "M", "H"))] <- ame(db0, db1)[,a]
    }
  }
  return(round(M, 3))
}

# Calculate AME for 5 mins, 10 mins, 15 mins and 20 mins increases in access time
ame_5 <- calculate_ame(attribute="t_",value=1.49,5)
ame_10 <- calculate_ame(attribute="t_",value=1.49,10)
ame_15 <- calculate_ame(attribute="t_",value=1.49,15)
ame_20 <- calculate_ame(attribute="t_",value=1.49,20)

# Plot the results
ame_5 <- as.data.frame(ame_5)
ame_5$Access_Time <- 5
ame_10 <- as.data.frame(ame_10)
ame_10$Access_Time <- 10
ame_15 <- as.data.frame(ame_15)
ame_15$Access_Time <- 15
ame_20 <- as.data.frame(ame_20)
ame_20$Access_Time <- 20

ame_all <- rbind(ame_5, ame_10, ame_15,ame_20)
ame_all$Income_Group <- rep(c("Low", "Medium", "High"), times=4)

# Melt the data for ggplot2
ame_melted <- melt(ame_all, id.vars = c("Income_Group", "Access_Time"), 
                   variable.name = "Alternative", value.name = "Effect")

# Extract alternative and income group from the 'Alternative' column
ame_melted$Quantile <- sub(".*\\.q", "", ame_melted$Alternative)
ame_melted$Alternative <- sub("\\.q.*", "", ame_melted$Alternative)

# Pivot the data to get lower, mean, and upper quantiles in separate columns
ame_pivot <- dcast(ame_melted, Income_Group + Access_Time + Alternative ~ Quantile, value.var = "Effect")
facet_labels <- c(bus = "PT Option 1", metro = "PT Option 2")

# Plot the results using ggplot2
access_ame<- ggplot(ame_pivot, aes(x = Access_Time, y = M, color = Income_Group, group = Income_Group)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar(aes(ymin = L, ymax = H), width = 0.6) +
  facet_wrap(~ Alternative,labeller = labeller(Alternative=facet_labels)) +
  labs(title = "Average Marginal Effect of Access Time Increase",
       x = "Access Time Increase (mins)",
       y = "Average Marginal Effect") +
  theme_minimal()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face = "bold"))

print(access_ame)

ggsave("../Gender_study/results/images/access_ame.png",plot = access_ame,width = 8,height = 4)

# Calculate AME for 5 mins, 10 mins, 15 mins and 20 mins increases in waiting time
wame_5 <- calculate_ame(attribute="t_",value=1.83,5)
wame_10 <- calculate_ame(attribute="t_",value=1.83,10)
wame_15 <- calculate_ame(attribute="t_",value=1.83,15)
wame_20 <- calculate_ame(attribute="t_",value=1.83,20)

# Plot the results
wame_5 <- as.data.frame(wame_5)
wame_5$Waiting_Time <- 5
wame_10 <- as.data.frame(wame_10)
wame_10$Waiting_Time <- 10
wame_15 <- as.data.frame(wame_15)
wame_15$Waiting_Time <- 15
wame_20 <- as.data.frame(wame_20)
wame_20$Waiting_Time <- 20

wame_all <- rbind(wame_5, wame_10, wame_15,wame_20)
wame_all$Income_Group <- rep(c("Low", "Medium", "High"), times=4)

# Melt the data for ggplot2
wame_melted <- melt(wame_all, id.vars = c("Income_Group", "Waiting_Time"), 
                   variable.name = "Alternative", value.name = "Effect")

# Extract alternative and income group from the 'Alternative' column
wame_melted$Quantile <- sub(".*\\.q", "", wame_melted$Alternative)
wame_melted$Alternative <- sub("\\.q.*", "", wame_melted$Alternative)

# Pivot the data to get lower, mean, and upper quantiles in separate columns
wame_pivot <- dcast(wame_melted, Income_Group + Waiting_Time + Alternative ~ Quantile, value.var = "Effect")
facet_labels <- c(bus = "PT Option 1", metro = "PT Option 2")

# Plot the results using ggplot2
waiting_ame<- ggplot(wame_pivot, aes(x = Waiting_Time, y = M, color = Income_Group, group = Income_Group)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar(aes(ymin = L, ymax = H), width = 0.6) +
  facet_wrap(~ Alternative,labeller = labeller(Alternative=facet_labels)) +
  labs(title = "Average Marginal Effect of Waiting Time Increase",
       x = "Waiting Time Increase (mins)",
       y = "Average Marginal Effect") +
  theme_minimal()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face = "bold"))

print(waiting_ame)

ggsave("../Gender_study/results/images/waiting_ame.png",plot = waiting_ame,width = 8,height = 4)

# Calculate AME for 5 mins, 10 mins, 15 mins and 20 mins increases in travel time
ttame_5 <- calculate_ame(attribute="t_",value=1,5)
ttame_10 <- calculate_ame(attribute="t_",value=1,10)
ttame_15 <- calculate_ame(attribute="t_",value=1,15)
ttame_20 <- calculate_ame(attribute="t_",value=1,20)

# Plot the results
ttame_5 <- as.data.frame(ttame_5)
ttame_5$Travel_Time <- 5
ttame_10 <- as.data.frame(ttame_10)
ttame_10$Travel_Time <- 10
ttame_15 <- as.data.frame(ttame_15)
ttame_15$Travel_Time <- 15
ttame_20 <- as.data.frame(ttame_20)
ttame_20$Travel_Time <- 20

ttame_all <- rbind(ttame_5, ttame_10, ttame_15,ttame_20)
ttame_all$Income_Group <- rep(c("Low", "Medium", "High"), times=4)

# Melt the data for ggplot2
ttame_melted <- melt(ttame_all, id.vars = c("Income_Group", "Travel_Time"), 
                    variable.name = "Alternative", value.name = "Effect")

# Extract alternative and income group from the 'Alternative' column
ttame_melted$Quantile <- sub(".*\\.q", "", ttame_melted$Alternative)
ttame_melted$Alternative <- sub("\\.q.*", "", ttame_melted$Alternative)

# Pivot the data to get lower, mean, and upper quantiles in separate columns
ttame_pivot <- dcast(ttame_melted, Income_Group + Travel_Time + Alternative ~ Quantile, value.var = "Effect")
facet_labels <- c(bus = "PT Option 1", metro = "PT Option 2")

# Plot the results using ggplot2
travel_ame<- ggplot(ttame_pivot, aes(x = Travel_Time, y = M, color = Income_Group, group = Income_Group)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar(aes(ymin = L, ymax = H), width = 0.6) +
  facet_wrap(~ Alternative,labeller = labeller(Alternative=facet_labels)) +
  labs(title = "Average Marginal Effect of Travel Time Increase",
       x = "Travel Time Increase (mins)",
       y = "Average Marginal Effect") +
  theme_minimal()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face = "bold"))

print(travel_ame)

ggsave("../Gender_study/results/images/travel_ame.png",plot = travel_ame,width = 8,height = 4)

# Calculate AME for access to stop
accstop_ame_3 <- calculate_ame_factor(attribute="saccstop_",value=3,3)
accstop_ame_2 <- calculate_ame_factor(attribute="saccstop_",value=3,2)
accstop_ame_1 <- calculate_ame_factor(attribute="saccstop_",value=3,1)

# Plot the results
accstop_ame_3 <- as.data.frame(accstop_ame_3)
accstop_ame_3$Access_stop <- "Access infrastructure\n as of now"
accstop_ame_2 <- as.data.frame(accstop_ame_2)
accstop_ame_2$Access_stop <- "Clear Signages &\n Boards"
accstop_ame_1 <- as.data.frame(accstop_ame_1)
accstop_ame_1$Access_stop <- "Active and well-lit\n streets and footpaths"

accstopame_all <- rbind(accstop_ame_1,accstop_ame_2,accstop_ame_3)
accstopame_all$Income_Group <- rep(c("Low", "Medium", "High"), times=3)

# Melt the data for ggplot2
accstopame_melted <- melt(accstopame_all, id.vars = c("Income_Group", "Access_stop"), 
                     variable.name = "Alternative", value.name = "Effect")

# Extract alternative and income group from the 'Alternative' column
accstopame_melted$Quantile <- sub(".*\\.q", "", accstopame_melted$Alternative)
accstopame_melted$Alternative <- sub("\\.q.*", "", accstopame_melted$Alternative)

# Pivot the data to get lower, mean, and upper quantiles in separate columns
accstopame_pivot <- dcast(accstopame_melted, Income_Group + Access_stop + Alternative ~ Quantile, value.var = "Effect")
facet_labels <- c(bus = "PT Option 1", metro = "PT Option 2")
accstopame_pivot<- accstopame_pivot %>%
  mutate(Access_stop = factor(Access_stop,levels = c("Access infrastructure\n as of now", 
                                                  "Clear Signages &\n Boards", 
                                                  "Active and well-lit\n streets and footpaths"),
                                       ordered = TRUE))

# Plot the results using ggplot2
accstop_ame<- ggplot(accstopame_pivot, aes(x = factor(Access_stop, levels = c("Access infrastructure\n as of now", 
                                                                              "Clear Signages &\n Boards", 
                                                                              "Active and well-lit\n streets and footpaths")), 
                                           y = M, color = Income_Group, group = Income_Group)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = L, ymax = H), width = 0.2) +
  scale_color_manual(values = c("Low" = "#FF4500",  
                                "Medium" = "#00CED1",
                                "High" = "#FFD700"))+
  facet_wrap(~ Alternative,labeller = labeller(Alternative=facet_labels)) +
  labs(title = "Average Marginal Effect of Access Stop Changes",
       x = "Access Stop Change",
       y = "Average Marginal Effect") +
  theme_minimal()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face = "bold"))

print(accstop_ame)

ggsave("../Gender_study/results/images/accstop_ame.png",plot = accstop_ame,width = 10,height = 4)

# Calculate AME for boarding/alighting
sboal_ame_2 <- calculate_ame_factor(attribute="sboal_",value=1,2)
sboal_ame_1 <- calculate_ame_factor(attribute="sboal_",value=1,1)

# Plot the results
sboal_ame_2 <- as.data.frame(sboal_ame_2)
sboal_ame_2$Board_alight <- "Passengers following queue\n behaviour while boarding"
sboal_ame_1 <- as.data.frame(sboal_ame_1)
sboal_ame_1$Board_alight <- "Boarding/alighting in\n crowded conditions"

sboalame_all <- rbind(sboal_ame_1,sboal_ame_2)
sboalame_all$Income_Group <- rep(c("Low", "Medium", "High"), times=2)

# Melt the data for ggplot2
sboalame_melted <- melt(sboalame_all, id.vars = c("Income_Group", "Board_alight"), 
                          variable.name = "Alternative", value.name = "Effect")

# Extract alternative and income group from the 'Alternative' column
sboalame_melted$Quantile <- sub(".*\\.q", "",sboalame_melted$Alternative)
sboalame_melted$Alternative <- sub("\\.q.*", "", sboalame_melted$Alternative)

# Pivot the data to get lower, mean, and upper quantiles in separate columns
sboalame_pivot <- dcast(sboalame_melted, Income_Group + Board_alight + Alternative ~ Quantile, value.var = "Effect")
facet_labels <- c(bus = "PT Option 1", metro = "PT Option 2")


# Plot the results using ggplot2
sboal_ame<- ggplot(sboalame_pivot, aes(x = factor(Board_alight, levels = c("Boarding/alighting in\n crowded conditions", 
                                                                             "Passengers following queue\n behaviour while boarding")), 
                                           y = M, color = Income_Group, group = Income_Group)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = L, ymax = H), width = 0.2) +
  scale_color_manual(values = c("Low" = "#FF4500",  
                                "Medium" = "#00CED1",
                                "High" = "#FFD700"))+
  facet_wrap(~ Alternative,labeller = labeller(Alternative=facet_labels)) +
  labs(title = "Average Marginal Effect of Boarding/Alighting Changes",
       x = "Boarding/Alighting Change",
       y = "Average Marginal Effect") +
  theme_minimal()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face = "bold"))

print(sboal_ame)

ggsave("../Gender_study/results/images/sboal_ame.png",plot = sboal_ame,width = 10,height = 4)


# Calculate AME for onboard safety
safety_ame_3 <- calculate_ame_factor(attribute="safety_",value=3,3)
safety_ame_2 <- calculate_ame_factor(attribute="safety_",value=3,2)
safety_ame_1 <- calculate_ame_factor(attribute="safety_",value=3,1)

# Plot the results
safety_ame_3 <- as.data.frame(safety_ame_3)
safety_ame_3$Safety <- "Crowded\n standing"
safety_ame_2 <- as.data.frame(safety_ame_2)
safety_ame_2$Safety <-"Comfortable standing with\n adequate grab handles\n for support"
safety_ame_1 <- as.data.frame(safety_ame_1)
safety_ame_1$Safety <- "Seating space\n available"

safetyame_all <- rbind(safety_ame_1,safety_ame_2,safety_ame_3)
safetyame_all$Income_Group <- rep(c("Low", "Medium", "High"), times=3)

# Melt the data for ggplot2
safetyame_melted <- melt(safetyame_all, id.vars = c("Income_Group", "Safety"), 
                        variable.name = "Alternative", value.name = "Effect")

# Extract alternative and income group from the 'Alternative' column
safetyame_melted$Quantile <- sub(".*\\.q", "",safetyame_melted$Alternative)
safetyame_melted$Alternative <- sub("\\.q.*", "", safetyame_melted$Alternative)

# Pivot the data to get lower, mean, and upper quantiles in separate columns
safetyame_pivot <- dcast(safetyame_melted, Income_Group + Safety + Alternative ~ Quantile, value.var = "Effect")
facet_labels <- c(bus = "PT Option 1", metro = "PT Option 2")


# Plot the results using ggplot2
safety_ame<- ggplot(safetyame_pivot, aes(x = factor(Safety, levels = c("Crowded\n standing", 
                                                                       "Comfortable standing with\n adequate grab handles\n for support",
                                                                       "Seating space\n available")), 
                                       y = M, color = Income_Group, group = Income_Group)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = L, ymax = H), width = 0.2) +
  scale_color_manual(values = c("Low" = "#FF4500",  
                                "Medium" = "#00CED1",
                                "High" = "#FFD700"))+
  facet_wrap(~ Alternative,labeller = labeller(Alternative=facet_labels)) +
  labs(title = "Average Marginal Effect of On-board Safety Changes",
       x = "On-board Safety Change",
       y = "Average Marginal Effect") +
  theme_minimal()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face = "bold"))

print(safety_ame)

ggsave("../Gender_study/results/images/safety_ame.png",plot = safety_ame,width = 10,height = 4)

# Calculate AME for waiting environment
swaitenv_ame_3 <- calculate_ame_factor(attribute="swaitenv_",value=3,3)
swaitenv_ame_2 <- calculate_ame_factor(attribute="swaitenv_",value=3,2)
swaitenv_ame_1 <- calculate_ame_factor(attribute="swaitenv_",value=3,1)

# Plot the results
swaitenv_ame_3 <- as.data.frame(swaitenv_ame_3)
swaitenv_ame_3$Wait_env <- "Waiting alone"
swaitenv_ame_2 <- as.data.frame(swaitenv_ame_2)
swaitenv_ame_2$Wait_env <- "Staff/police presence\n or help booths in\n public transport stops"
swaitenv_ame_1 <- as.data.frame(swaitenv_ame_1)
swaitenv_ame_1$Wait_env <- "Other people present\n at the stop"

swaitenvame_all <- rbind(swaitenv_ame_1,swaitenv_ame_2,swaitenv_ame_3)
swaitenvame_all$Income_Group <- rep(c("Low", "Medium", "High"), times=3)

# Melt the data for ggplot2
swaitenvame_melted <- melt(swaitenvame_all, id.vars = c("Income_Group", "Wait_env"), 
                           variable.name = "Alternative", value.name = "Effect")

# Extract alternative and income group from the 'Alternative' column
swaitenvame_melted$Quantile <- sub(".*\\.q", "",swaitenvame_melted$Alternative)
swaitenvame_melted$Alternative <- sub("\\.q.*", "", swaitenvame_melted$Alternative)

# Pivot the data to get lower, mean, and upper quantiles in separate columns
swaitenvame_pivot <- dcast(swaitenvame_melted, Income_Group + Wait_env + Alternative ~ Quantile, value.var = "Effect")
facet_labels <- c(bus = "PT Option 1", metro = "PT Option 2")


# Plot the results using ggplot2
swaitenv_ame<- ggplot(swaitenvame_pivot, aes(x = factor(Wait_env, levels = c("Waiting alone", 
                                                                             "Other people present\n at the stop",
                                                                             "Staff/police presence\n or help booths in\n public transport stops")), 
                                             y = M, color = Income_Group, group = Income_Group)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = L, ymax = H), width = 0.2) +
  scale_color_manual(values = c("Low" = "#FF4500",  
                                "Medium" = "#00CED1",
                                "High" = "#FFD700"))+
  facet_wrap(~ Alternative,labeller = labeller(Alternative=facet_labels)) +
  labs(title = "Average Marginal Effect of Waiting Environment Changes",
       x = "Waiting Environment Change",
       y = "Average Marginal Effect") +
  theme_minimal()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face = "bold"))

print(swaitenv_ame)

ggsave("../Gender_study/results/images/swaitenv_ame.png",plot = swaitenv_ame,width = 10,height = 4)