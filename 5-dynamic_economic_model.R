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
cherryonfarm <- cherrygrowth(-10:10, acres*cherry_per_acre, beta = 1, r = .3)
cherryonfarm <- cherryonfarm[3:12]

#plot(cherryonfarm) 

# Model parameters
{
cost_s <- 180    # Cost to spray
cost_h <- .5     # Cost to harvest per pound of cherry
cv <- c(0.08, 0.02, 0.13, 0) # Initial infestation
cv[4] <- 1 - sum(cv)
new_cv <- cv     # Set new infestation to initial infestation
#harvestschedule <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)    # Harvest schedule 0-No Harvest  1-Harvest
harvestschedule <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)    # Harvest schedule 0-No Harvest  1-Harvest
harvestpercentages <- c(0, 0, 0, 0, 0, 0, .32, .48, .12, .08)
acres <- 2      # Farm acres
cherry_per_acre <- 7500     # Estimated cherry per acres
harvestedcherry <- 0        # Initial harvested cherry
decision_type <- "cost"     # TYpe of decision "cost" or "infestation"
totalnb <- data.frame()
}

# Convert to field level
#---------------------------------------
# Dynamic optimization problem

for (i in 1:9){
  
  # Dynamic cherry pricing
  p <- cherrypricing(cv[3])
  
  # Calculate decision and infestation values
  #decsion_type <- "cost"
  #choice <- decision(cost_s, cherryonfarm[i], nsp_mcListFit$estimate[[i]][], cv)
  choice <- 1
  # Get new current infestation values based on spray decision
  new_cv <- choice * (cv %*% sp_mcListFit$estimate[[i]][]) + (1 - choice) * (cv %*% nsp_mcListFit$estimate[[i]][])
  
  # C/D damage
  #d <- new_cv[3] - cv[3]
  nonmarket <- cherryonfarm[i] * cv[3]
  
  # Update dynamic cherry pricing
  p <- cherrypricing(cv[3])
  
  # Optimize Net Benefit (NB)
  nb <- maxnb(p = p, 
  cost_s = cost_s, 
  cost_h = cost_h, 
  cherryforharvest = acres*cherry_per_acre*harvestpercentages[i],
  cherry_on_farm = cherryonfarm[i], 
  h = harvestschedule[i], 
  harvestedcherry =  harvestedcherry,
  #cv = new_cv,
  cv = cv,
  i = i)
  
  # Build data frame with results
  totalnb <- rbind(totalnb, nb)
  
  # New infestation levels for next period
  cv <- new_cv
  harvestedcherry <- sum(totalnb$harvest_c)
  
  if (i == 9){
    
      nb <- maxnb(p = p, 
            cost_s = cost_s, 
            cost_h = cost_h, 
            cherryforharvest = acres*cherry_per_acre*harvestpercentages[i+1],
            cherry_on_farm = cherryonfarm[i+1], 
            h = harvestschedule[i+1], 
            harvestedcherry =  harvestedcherry,
            #cv = new_cv,
            cv = cv,
            i = i+1)
      totalnb <- rbind(totalnb, nb)
  }
}

#totalnb <- round(totalnb, 2)
# saveRDS(totalnb, "data/totalnb.rds")
totalnb
sum(totalnb$nb)
sum(totalnb$harvest_c)
15000*harvestpercentages
