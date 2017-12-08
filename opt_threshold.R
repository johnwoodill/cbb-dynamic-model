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

# Logistic function for cherry growth
cherryonfarm <- cherrygrowth(-10:10, acres*cherry_per_acre, beta = 1, r = .3)
cherryonfarm <- cherryonfarm[3:12]

# tdat <- data.frame()
# thold <- data.frame()

# gridd <- expand.grid(seq(0, .2, by = 0.01), seq(0, 0.1, by = 0.01), seq(0, 1, by = 0.001))
# gridd$Var4 <- 1 - rowSums(gridd[, 1:3])
# gridd <- filter(gridd, Var4 >= 0 & Var3 > 0)
# head(gridd)

# infdat <- structure(list(abl = c(0.055, 0.04640625, 0.0378125, 0.0346614583333333, 
# 0.0315104166666667, 0.028359375, 0.0220572916666667, 0.0157552083333333, 
# 0.009453125, 0.00630208333333333), abd = c(0.025, 0.0034375, 
# 0.00171875, 0.00372395833333333, 0.00501302083333333, 0.00315104166666667, 
# 0.00315104166666667, 0.00315104166666667, 0.00630208333333333, 
# 0.00630208333333333), cd = c(0.01, 0.0384895833333333, 0.0274189814814815, 
# 0.0285648148148148, 0.0304267939814815, 0.0354398148148148, 0.0366790674603175, 
# 0.0383962673611111, 0.0415473090277778, 0.0557889093137255), 
#     nin = c(0.91, 0.911666666666667, 0.933049768518519, 0.933049768518519, 
#     0.933049768518519, 0.933049768518519, 0.938112599206349, 
#     0.942697482638889, 0.942697482638889, 0.931606924019608)), .Names = c("abl", 
# "abd", "cd", "nin"), row.names = c(NA, -10L), class = "data.frame")

source("3-dynamic_economic_model.R")

infdat <- data.frame(abl = totalnb$field_ablive,
                     abd = totalnb$field_abdead,
                     cd = totalnb$field_cd)

thold_search <- function(x){
      threshold <- c(x[1], infdat[i, 2], infdat[i, 3])
      threshold[4] <- 1 - sum(threshold[1:3])
      nspray <- threshold %*% nsp_mcListFit$estimate[[i]][]
      nspray_growth <- nspray[3] - threshold[3]
      
      nsp_damage <- nspray_growth * (cherryonfarm[i]*threshold[4]) * cherrypricing(nspray[3])
      
      # Get decision : 1 (spray), 0 (no spray)
      ifelse(nsp_damage >= cost_s*acres, 
             ifelse(sum(threshold[1:3]) <= 1, nsp_damage - cost_s*acres, 99999), 99999)
      #nsp_damage - (cost_s*acres)
      #ifelse(ret >= 0, ret, 9999)
}

dat <- data.frame()
for (i in 1:9){
  optvalue <- optim(c(infdat[i, 1]), thold_search,
      lower = 0, upper = 1, method = "L-BFGS-B")
  optvalue
  thold <- data.frame(month = i + 2,
                      ABLive = optvalue$par[1],
                      ABDead =  infdat[i, 2],
                      CD = infdat[i, 3])
  dat <- rbind(dat, thold)
}
dat
