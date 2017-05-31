#' 
rm(list=ls())

# Net benefit optimization function
source("R/maxnb.R")

# Decision function
source("R/decision.R")

# Dynamic cherry pricing function
source("R/cherrypricing.R")

# Initiate parameters
source("1-parameters.R")

# Get calibrated markov chains
calibration_type <- "field"
source("2-calibrate_markov_chains.R")

# Cherry growth 
source("R/cherrygrowth.R")
cherryonfarm <- cherrygrowth(-12:12, acres*cherry_per_acre, beta = 1, r = .3)
cherryonfarm <- cherryonfarm[3:12]

#plot(cherryonfarm) 

# Convert to field level


cv <- c(0, 0.08, 0.01, .01)  
cv[1] <- 1 - sum(cv)
totalnb <- data.frame()
#---------------------------------------
# Dynamic optimization problem

for (i in 1:8){
  
  # Dynamic cherry pricing
  p <- cherrypricing(cv[4])
  
  # Calculate decision and infestation values
  #decsion_type <- "cost"
  #choice <- decision(cost_s, cherryonfarm[i], nsp_mcListFit$estimate[[i]][], cv)
  choice <- 1
  # Get new current infestation values based on spray decision
  new_cv <- choice * (cv %*% sp_mcListFit$estimate[[i]][]) + (1 - choice) * (cv %*% nsp_mcListFit$estimate[[i]][])
  
  # C/D damage
  d <- new_cv[4] - cv[4]
  
  # Update dynamic cherry pricing
  p <- cherrypricing(new_cv[4])
  
  # Optimize Net Benefit (NB)
  nb <- maxnb(p = p, 
  cost_s = cost_s, 
  cost_h = cost_h, 
  cherry = cherryonfarm[i], 
  h = harvestschedule[i], 
  harvestedcherry =  harvestedcherry,
  cv = new_cv,
  d = d)
  
  # Build data frame with results
  totalnb <- rbind(totalnb, nb)
  
  # New infestation levels for next period
  cv <- new_cv
  harvestedcherry <- sum(totalnb$harvest_c)
}

# totalnb <- round(totalnb, 2)
# saveRDS(totalnb, "data/totalnb.rds")
totalnb
sum(totalnb$nb)
