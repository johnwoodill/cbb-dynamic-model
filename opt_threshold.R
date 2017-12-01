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


thold_search <- function(x){
      threshold <- c(x, 0, 0)
      threshold[4] <- 1 - sum(threshold[1:3])
      nspray <- threshold %*% nsp_mcListFit$estimate[[i]][]
      nspray_growth <- nspray[3] - threshold[3]
      nsp_damage <- nspray_growth * (cherryonfarm[i]*threshold[4]) * cherrypricing(nspray[3])
      
      # Get decision : 1 (spray), 0 (no spray)
      ifelse(nsp_damage >= cost_s*acres, ifelse(sum(threshold[1:3]) <= 1, nsp_damage - cost_s*acres, 99999), 99999)
      # ret <- nsp_damage - (cost_s*acres)
      # ifelse(ret >= 0, ret, 9999)
}

dat <- data.frame()
for (i in 1:9){
  optvalue <- optim(c(0.00001), thold_search,
      lower = 0, upper = 1, method = "L-BFGS-B")
  optvalue
  thold <- data.frame(month = i + 3,
                      ABLive = optvalue$par[1],
                      ABDead = 0,
                      CD = 0)
  dat <- rbind(dat, thold)
}
dat
