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
cv <- c(0.135, .225, .09)
cv[4] <- 1 - sum(cv)

# Get calibrated markov chains
calibration_type <- "field"
source("2-calibrate_markov_chains.R")

# Cherry growth 
source("R/cherrygrowth.R")

# Jan - Dec
cherryonfarm <- cherrygrowth(-10:10, acres*cherry_per_acre, beta = 1, r = .3)
# ggplot(NULL, aes(cherryonfarm, x = 1:12)) + geom_line() + xlab("Month") +
#   scale_x_continuous(breaks = 1:12) +
#   ylab("lbs. of cherry") + theme_tufte(base_size = 14) + 
#     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   ggtitle("Cherry Growth on 2-acre Farm from January to December")


cherryonfarm <- cherryonfarm[3:12]

# Convert to field level
#---------------------------------------
# Dynamic optimization problem
for (i in 1:9){
  
  # Dynamic cherry pricing
  p <- cherrypricing(cv[3])
  
  # Calculate decision and infestation values
  #decsion_type <- "cost"
  choice <- decision(acres, cost_s, cherryonfarm[i]*cv[4], nsp_mcListFit$estimate[[i]][], 
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
#totalnb <- round(totalnb, 2)
# saveRDS(totalnb, "data/totalnb.rds")
# totalnb
# sum(totalnb$nb)
# sum(totalnb$harvest_c)
# 15000*harvestpercentages
# totalnb$model <- "Economic Model"
# totalnb$field_ablive <- totalnb$field_ablive*100
# totalnb$field_abdead <- totalnb$field_abdead*100
# totalnb$field_cd <- totalnb$field_cd*100
# 
# saveRDS(totalnb, "results/dynamicmodel.rds")
# 
# thold$Month <- 3:11
# thold
# 
# ggplot(thold, aes(Month, CD_threshold*100)) + 
#   geom_line(data = totalnb, aes(Month, field_cd*100), color = "red") +
#   geom_line(linetype = "dashed") + 
#   geom_point(data = filter(totalnb, spray == 1), aes(Month, field_cd*100)) +
#   theme_tufte() +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 3:12) +
#   scale_y_continuous(breaks = 0:12) +
#   ylab("Field-level \n CD Infestation") +
#   scale_colour_manual(name = 'the colour', 
#          values =c('black'='black','red'='red'), labels = c('c2','c1')) +
#   theme(legend.position = c(0,1), 
#         legend.justification = c("left", "top"), 
#         legend.box.background = element_rect(colour = "grey"), 
#         legend.title = element_blank(), legend.key = element_blank())  +
#   annotate("text", x = 4, y = 10, label = "Threshold") +
#   annotate("text", x = 3.5, y = 5, label = "Farm Infestation", color = "red")
#   

totalnb
finalresults <- totalnb
write_csv(totalnb, "results/poorly_managed_main_results.csv")


saveRDS(totalnb, "results/poorly_managed_main_results.rds")