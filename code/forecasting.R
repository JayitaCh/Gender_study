
# Load the script containing the Apollo model
source("./code/modelling.R")
source("./code/modelling_nonPT.R")


#### Forecasting ####


### Function for forecasting

forecastRoute <- function(modelPT, modelNonPT, db, db1, MSObj=c(metro=19, bus=13, oth=70)){
  ### Function to update market share and forecast
  updateForecast <- function(model, dbrow, MSObj){
    # Extract model-specific inputs
    apollo_beta <- model$estimate
    apollo_fixed <- if (!is.null(model$apollo_fixed)) model$apollo_fixed else character(0)
    apollo_control <- model$apollo_control
    apollo_control$panelData <- FALSE
    apollo_randCoeff <- model$apollo_randCoeff
    apollo_lcPars <- model$apollo_lcPars
    apollo_probabilities <- model$apollo_probabilities

    # Assign into global environment temporarily (not ideal, but works with apollo)
    assign("apollo_control", apollo_control, envir = .GlobalEnv)
    assign("apollo_randCoeff", apollo_randCoeff, envir = .GlobalEnv)
    assign("apollo_lcPars", apollo_lcPars, envir = .GlobalEnv)
    assign("apollo_probabilities", apollo_probabilities, envir = .GlobalEnv)

    # Validate inputs
    apIn <- apollo_validateInputs(
      apollo_beta = apollo_beta,
      apollo_fixed = apollo_fixed,
      database = dbrow,
      silent = TRUE
    )
    
    # apIn <- apollo_validateInputs(database = dbrow,silent = TRUE)
    
    alts <- c("bus", "metro","others")
    aName <- c("asc_metro", "asc_others")
    nIter <- 0
    pred <- colSums(apollo_prediction(model, apollo_probabilities, apIn)[alts])
    while(any( abs(pred - MSObj) > .5) && nIter<=39){
      nIter <- nIter + 1
      if ("asc_metro" %in% aName) {
        ratio <- MSObj["metro"] / pred["metro"]
        if (ratio > 0) {
          model$estimate["asc_metro"] <- model$estimate["asc_metro"] - 1/(1.5*log(ratio)^nIter)
        }
      }

      if ("asc_others" %in% aName) {
        ratio <- MSObj["oth"] / pred["others"]
        if (is.finite(ratio) && ratio > 0) {
          model$estimate["asc_others"] <- model$estimate["asc_others"] + 1/ (log(ratio)*15)
        }
      }
      
      # model$estimate[aName] <- model$estimate[aName] + 1/(200*log(MSObj[-2]/pred[-2]))
      pred <- colSums(apollo_prediction(model, apollo_probabilities, apIn)[alts])
      # if(nIter==1) cat("...", alts, "\n")
      # cat(ifelse(nIter<10, " ", ""), nIter, ") ", paste(round(pred,1), collapse=" "), "\n", sep="")
    }
    
    ### Forecast for base scenario
    # apIn <- apollo_validateInputs(database=db, silent=TRUE)
    # pred0 <- apollo_prediction(model, apollo_probabilities, apIn)[alts]
    return(pred)
  }
  
  ### Initialize combined predictions
  combinedPred <- data.frame(bus_base = numeric(nrow(db)),
                             metro_base = numeric(nrow(db)),
                             others_base = numeric(nrow(db)),
                             bus_mod = numeric(nrow(db)),
                             metro_mod = numeric(nrow(db)),
                             others_mod = numeric(nrow(db)))
  
  ### Apply appropriate model based on user type
  for (i in 1:nrow(db)) {
    if (db$PT_users[i] == 1) {
      pred <- updateForecast(modelPT, db[i, , drop=FALSE], MSObj=c(metro=22, bus=15, oth=63))
      ### Forecast for improved scenario
      pred1 <- updateForecast(modelPT, db1[i, , drop=FALSE], MSObj=c(metro=22, bus=15, oth=63))
    } else {
      pred <- updateForecast(modelNonPT, db[i, , drop=FALSE], MSObj=c(metro=5, bus=7, oth=88))
      ### Forecast for improved scenario
      pred1 <- updateForecast(modelNonPT, db1[i, , drop=FALSE], MSObj=c(metro=5, bus=7, oth=88))
    }
    combinedPred[i, ] <- cbind(pred,pred1)
  }
  
  ### Combine predictions
  M <- rowsum(combinedPred, group=db$relInc)/as.numeric(table(db$relInc))
  M <- rbind(M, colSums(combinedPred)/100)
  rownames(M) <- c(paste0("inc", c(20, 37.5, 87.5, 137.5,160,200)), "Total")
  print(round(M, 4))
  # return(combinedPred)
}

database <- read.csv('data/SP_Gendersafety.csv')

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
database$PT_users <- ifelse(database$Primary_mode %in% c(1,2),1,0)
database$av_bus <- TRUE
database$av_metro <- TRUE
database$av_others <- TRUE
database$t_bus <- 1.49*database$at_bus + 1.83*database$wt_bus + database$tt_bus
database$t_metro <- 1.49*database$at_metro + 1.83*database$wt_metro + database$tt_metro

### Generate database with representative 100 indivs
columns <- c("tc_bus","tc_metro","t_bus","t_metro","saccstop_bus","saccstop_metro",
         "swaitenv_bus" ,"swaitenv_metro" ,"sboal_bus","sboal_metro","safety_bus",    
         "safety_metro","Choice","PT_users","relInc")


db0 <- data.frame(matrix(1, nrow=102, ncol=length(columns)))
names(db0) <- columns

num_alts <- length(unique(database$Choice))
n <- nrow(db0) / num_alts

db0$id <- rep(1:n, each = num_alts)
db0$relInc[  1: 11] <- 20000/mean(database$HH_Inc_num) # Reproduce income distribution
db0$relInc[ 12: 25] <- 37500/mean(database$HH_Inc_num) # Reproduce income distribution
db0$relInc[ 26: 41] <- 87500/mean(database$HH_Inc_num) # Reproduce income distribution
db0$relInc[ 42: 85] <- 137500/mean(database$HH_Inc_num) # Reproduce income distribution
db0$relInc[ 86: 101] <- 160000/mean(database$HH_Inc_num) # Reproduce income distribution
db0$relInc[102:102] <-200000/mean(database$HH_Inc_num) # Reproduce income distribution
# db0$PT_users[52:102] <- 0 # defining non-PT users
db0$PT_users <- sample(rep(c(0, 1), each = nrow(db0)/2)) # defining non-PT users
db0$Choice <- sample(c("bus", "metro", "other"), size = nrow(db0), replace = TRUE)

db0$saccstop_bus <- 3
db0$saccstop_metro <- 3

db0$swaitenv_bus <- 3
db0$swaitenv_metro <- 3

db0$sboal_bus <- 1
db0$sboal_metro <- 1

db0$safety_bus <- 3
db0$safety_metro <- 3

## Route: Dlf City 2 to Medical College Near Mg Road
# Base scenario
db0$t_bus <- 1.49*16 + 1.83*6 + 20 # weighted time
db0$t_metro <- 1.49*15 + 1.83*5 + 10 # weighted time
db0$tc_bus  <- 5 # fixed cost
db0$tc_metro <- 10 # fixed cost
db0$av_bus <- TRUE
db0$av_metro <- TRUE
db0$av_others <- TRUE

# Modified Scenario
db1 <- db0
# db1$swaitenv_metro <- 1 #"Other people present at the stop"
db1$swaitenv_metro <- 2 # "Staff/police presence or help booths in public transport stops"
# db1$saccstop_metro <- 2 # "Clear signages and boards"
db1$saccstop_metro <- 1 # "Active and well-lit streets and footpaths"
db1$sboal_metro <- 2 # "Passengers following queue behaviour while boarding"
# db1$safety_metro <- 2 # "Comfortable standing with adequate grab handles for support"
db1$safety_metro <- 1 # "Seating space available"


# Predict
forecastRoute(modelPT = model_PT, modelNonPT=model_nPT, db=db0,db1=db1, MSObj=c(metro=19, bus=13, oth=70))