# rm(list=ls())

library(tidyverse)
library(ggthemes)
library(markovchain)

# Net benefit optimization function
source("R/maxnb.R")

# Decision function
source("R/decision.R")

# Dynamic cherry pricing function
source("R/cherrypricing.R")

# Initiate parameters
source("1-parameters.R")

# Did not follow IPM
# Initial dissect levels
# Field-level AB live: 13.5%
#             AB Dead: 22.5%
#                  CD: 9%
# 
cv <- c(0.33, .01, .10)
cv[4] <- 1 - sum(cv)

# Get calibrated markov chains
calibration_type <- "field"
source("2-calibrate_markov_chains.R")

# Cherry growth 
source("R/cherrygrowth.R")

# Jan - Dec
cherryonfarm <- cherrygrowth(-10:10, 600, beta = 1, r = .3)
# cherryonfarm <- cherryonfarm + 1200
cherryonfarm[3:12] <- cherryonfarm[3:12] - 120
# lgf <- function(x) exp(x-6)/(1+exp(x-6)
# y = lgf(1:12)
# y <- y*15000
# y <- y + 1200
# y <- y - 100
# plot(x = 1:12, y = y)

cherryonfarm <- cherrygrowth(-10:10, 15000, beta = 1, r = .3)
# cherryonfarm <- cherrygrowth(-12:12, 15000, beta = 1, r = .3)
# cherryonfarm[1] <- cherryonfarm[1] + 1200
# cherryonfarm[9] <- cherryonfarm[9] - 4800
# cherryonfarm[10] <- cherryonfarm[10] - 7200 - 4800
# cherryonfarm[11] <- cherryonfarm[11] - 1800 - 7200 - 4800
# cherryonfarm[12] <- cherryonfarm[12]  - 1200 - 1800 - 7200 - 4800
# ggplot(NULL, aes(cherryonfarm, x = 1:12)) + geom_line() + xlab("Month") +
#    scale_x_continuous(breaks = 1:12) +
#   # geom_point(aes(x=1, y = 1200)) +
#    ylab("lbs. of cherry") + theme_tufte(base_size = 14) + 
#   # ylim(0, 16000) +
#    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#    ggtitle("Cherry Growth on 2-acre Farm from January to December \n Did not harvest in December")
#    # geom_label(NULL, aes(x = 8, y = -1, label = c("asdf")))
# 
# 
# geomc_herryonfarm <- cherryonfarm[3:12]

# Convert to field level
#---------------------------------------
# Dynamic optimization problem
for (i in 1:9){
  
  # Dynamic cherry pricing
  p <- cherrypricing(cv[3])
  
  # Calculate decision and infestation values
  #decsion_type <- "cost"
  choice <- decision(acres, cost_s, cherryonfarm[i], nsp_mcListFit$estimate[[i]][], 
                     sp_mcListFit$estimate[[i]][], cv)

  # Get new current infestation values based on spray decision
  new_cv <- choice * (cv %*% sp_mcListFit$estimate[[i]][]) + ((1 - choice) * (cv %*% nsp_mcListFit$estimate[[i]][]))
  
  # C/D damage
  #d <- new_cv[3] - cv[3]
  nonmarket <- cherryonfarm[i] * cv[3]
  
  # Update dynamic cherry pricing
  p <- cherrypricing(cv[3])
  
  # Optimize Net Benefit (NB)
  nb <- maxnb(p = p, 
              cost_s = cost_s*acres, 
              cost_h = cost_h, 
              cherryforharvest = acres*cherry_per_acre*harvestpercentages[i],
              cherry_on_farm = cherryonfarm[i], 
              h = harvestschedule[i], 
              harvestedcherry =  harvestedcherry,
              cv = cv,
              i = i)
  
  # Build data frame with results
  totalnb <- rbind(totalnb, nb)
  
  # New infestation levels for next period
  cv <- new_cv
  harvestedcherry <- sum(totalnb$harvestcherry)
  
  if (i == 9){
      choice <- 0
      nb <- maxnb(p = p, 
            cost_s = cost_s*acres, 
            cost_h = cost_h, 
            cherryforharvest = acres*cherry_per_acre*harvestpercentages[i+1],
            cherry_on_farm = cherryonfarm[i+1], 
            h = harvestschedule[i+1], 
            harvestedcherry =  harvestedcherry,
            cv = cv,
            i = i+1)
      totalnb <- rbind(totalnb, nb)
  }
}


totalnb
sum(totalnb$nb)
finalresults <- totalnb
write_csv(totalnb, "results/poorly_managed_main_results.csv")


saveRDS(totalnb, "results/poorly_managed_main_results.rds")

totalnb$spray
