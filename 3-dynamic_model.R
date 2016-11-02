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
source("2-calibrate_markov_chains.R")

# Ripe cherry growth 
source("R/cherrygrowth.R")
cherry <- cherrygrowth(-12:12, acres*cherry_per_acre, beta = 6, r = .8)

#---------------------------------------
# Dynamic optimization problem

for (i in 1:12){
  
  # Calculate decision and infestation values
  choice <- decision(p, cost_s, cherry, nsp_mcListFit$estimate[[i]][], cv)
  
  # Get new current infestation values based on spray decision
  new_cv <- choice * (cv %*% sp_mcListFit$estimate[[i]][]) + (1 - choice) * (cv %*% nsp_mcListFit$estimate[[i]][])
  
  # C/D damage
  d <- new_cv[4] - cv[4]
  
  # Dynamic cherry pricing
  p <- cherrypricing(new_cv[4])
  
  # Optimize Net Benefit (NB)
  nb <- maxnb(p = p, 
  cost_s = cost_s, 
  cost_h = cost_h, 
  cherry = 7500, 
  h = harvest[i], 
  cv = new_cv,
  d = d)
  
  # Build data frame with results
  totalnb <- rbind(totalnb, nb)
  
  # New infestation levels for next period
  cv <- new_cv
}

library(ggplot2)
library(directlabels)
print(totalnb)  
ggplot(totalnb, aes(month, cd)) + geom_line()
sum(totalnb$nb)
