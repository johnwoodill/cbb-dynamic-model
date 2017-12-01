rm(list=ls())

library(tidyverse)
library(ggthemes)
library(markovchain)

# Decision function
source("R/decision.R")

# Cherry growth 
source("R/cherrygrowth.R")

# Dynamic cherry pricing function
source("R/cherrypricing.R")

# Initiate parameters
source("1-parameters.R")

# Get calibrated markov chains
calibration_type <- "field"

source("2-calibrate_markov_chains.R")



# Followed IPM
# Initial dissect levels
# Field-level AB live: 5.5%
#             AB Dead: 2.5%
#                  CD: 1%


#
# cv <- c(0.055, 0.025, .01)
cv <- c(0.0001, 0.0001, 0.0001)
cv[4] <- 1 - sum(cv)

# Logistic function for cherry growth
cherryonfarm <- cherrygrowth(-10:10, acres*cherry_per_acre, beta = 1, r = .3)
cherryonfarm <- cherryonfarm[3:12]

# Range of values to search
srg <- seq(0, 1, by = 0.01)
  
tdat <- data.frame()
thold <- data.frame()

gridd <- expand.grid(seq(0, .2, by = 0.01), seq(0, 0.1, by = 0.01), seq(0, 1, by = 0.001))
gridd$Var4 <- 1 - rowSums(gridd[, 1:3])
gridd <- filter(gridd, Var4 >= 0 & Var3 > 0)
head(gridd)
for(i in 1:9){
  tdat <- data.frame()
  for (j in 1:nrow(gridd)){
    threshold <- c(gridd[j, 1], gridd[j, 2], gridd[j, 3], gridd[j, 4])
    # 
    # threshold <- c(0, 0, 0)
    # threshold[1] <- gridd[j, 1]
    # threshold[3] <- gridd[j, 2]
    # threshold[4] <- 1 - sum(threshold[1:3])
    # 
    nspray <- threshold %*% nsp_mcListFit$estimate[[i]][]
    nspray_growth <- nspray[3] - threshold[3]
    nsp_damage <- nspray_growth * (cherryonfarm[i+1]*threshold[4]) * cherrypricing(nspray[3])
  
    # Get decision : 1 (spray), 0 (no spray)
    dec <- ifelse(nsp_damage >= cost_s*acres, 1, 0)
    
    #threshold_choice <- decision(acres, cost_s, cherryonfarm[i], nsp_mcListFit$estimate[[i]][], threshold)
    mdat <- data.frame(month = i+2,
                       choice = dec,
                       nspray_growth = nspray_growth,
                       cherry_on_farm = cherryonfarm[i]*cv[4],
                       nspray_damage = nsp_damage,
                       ABL_threshold = threshold[1],
                       ABD_threshold = threshold[2],
                       CD_threshold = threshold[3],
                       NI_threshold = threshold[4])
    tdat <- rbind(tdat, mdat)
    }
  
  tdat <- arrange(tdat, choice, CD_threshold)
  tdat <- filter(tdat, NI_threshold >= 0 & nspray_growth >= 0)
  tdat <- arrange(tdat, choice, CD_threshold)
  #tdat
  loc <- which(tdat$choice == 1)[1]
  loc
  thold <- rbind(thold, tdat[loc, ])
  thold
  print(i)
}
thold
#thold$CD_threshold <- ifelse(is.na(thold$CD_threshold), 0, thold$CD_threshold)

saveRDS(thold, "results/threshold_search.rds")

ggplot(thold, aes(month, 100*CD_threshold)) + 
  geom_line(linetype = "dashed", color = "grey") + 
  geom_line(data = finalresults, aes(Month, 100*field_cd)) +
  scale_x_continuous(breaks = 3:12) + 
  ylim(0, 20)
