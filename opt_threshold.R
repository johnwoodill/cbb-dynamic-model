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
                     cd = totalnb$field_cd,
                     ni = totalnb$ni)

decisions <- totalnb$spray




thold_search <- function(x){
      # threshold <- as.numeric(c(x[1]))
      # threshold[4] <- 1 - sum(threshold[1:3])
      # nspray <- threshold %*% nsp_mcListFit$estimate[[i]][]
      # spray <- threshold %*% sp_mcListFit$estimate[[i]][]
      # B <- nspray[3] - spray[3]
      # nspray_growth <- nspray[3] - threshold[3]
      # nsp_damage <- nspray_growth * (cherryonfarm[i]*threshold[4]) * cherrypricing(nspray[3])
      
      # damage <- B * ( cherryonfarm[i] * threshold[4] ) * cherrypricing(threshold[3])
      
      # Get decision : 1 (spray), 0 (no spray)
      # if(nspray[1] <= infdat[i, 1]*1.20){
      # sum(ifelse(nsp_damage >= cost_s*acres,
             # ifelse(sum(threshold[1:3]) <= 1, nsp_damage - cost_s*acres, 99999), 99999))
      # }
      # ifelse(damage >= cost_s*acres, 
             # ifelse(sum(threshold[1:3]) <= 1, damage - cost_s*acres, 99999 ), 99999 )
      
      #nsp_damage - (cost_s*acres)
      #ifelse(ret >= 0, ret, 9999)
      
      damage <- (x[1] - infdat[i, 3])* cherryonfarm[i] * infdat[i, 4] * totalnb$price[i]
      sum(abs(damage - cost_s*acres))
      
       # if(decisions[i] == 0){
      #   damage <- (x[1] - infdat[i, 3])* cherryonfarm[i] * infdat[i, 4] * totalnb$price[i]
      #   sum(abs(damage - cost_s*acres))
      #   # ifelse(damage == cost_s*acres, 0, abs(damage - cost_s*acres))
      # }
      # 
      # if(decisions[i] == 1){
      #   damage <- (infdat[i, 3] - x[1])* cherryonfarm[i] * infdat[i, 4] * totalnb$price[i]
      #   sum(abs(damage - cost_s*acres))
      #   # ifelse(damage == cost_s*acres, 0, abs(damage - cost_s*acres))
      # }
}

# Threshold from USDA data
dat <- data.frame()
for (i in 1:10){
  
  if(decisions[i] == 0){
  optvalue <- optim(c(infdat[i, 3]), thold_search,
      lower = infdat[i, 3], upper = 1, method = "L-BFGS-B" )
  thold <- data.frame(month = i + 2,
                      ABLive = infdat[i, 1],
                      ABDead =  infdat[i, 2],
                      CD = optvalue$par[1] + infdat[i, 3],
                      Base_CD = infdat[i, 3])
  } 
  
  if(decisions[i] == 1){
  optvalue <- optim(c(infdat[i, 3]), thold_search,
      lower = 0, upper = infdat[i, 3], method = "L-BFGS-B" )
  thold <- data.frame(month = i + 2,
                      ABLive = infdat[i, 1],
                      ABDead =  infdat[i, 2],
                      CD = optvalue$par[1] - infdat[i, 3],
                      Base_CD = infdat[i, 3])
  } 
  
  optvalue

  dat <- rbind(dat, thold)
}
dat



# dat

# sdat <- data.frame()
# for(j in seq(-0.10,0.10, 0.001)){
#   dinfdat <- data.frame(abl = infdat$abl*j,
#                         abd = infdat$abd*j,
#                         cd = infdat$cd*j)
# 
# for (i in 1:9){
#   optvalue <- optim(c(dinfdat[i, 1]), thold_search,
#       lower = 0, upper = 1, method = "L-BFGS-B")
#   optvalue
#   thold <- data.frame(month = i + 2,
#                       ABLive = optvalue$par[1],
#                       ABDead =  infdat[i, 2],
#                       CD = infdat[i, 3])
#   sdat <- rbind(dat, thold)
# }}
# dat
dat



ggplot(dat, aes(month, 100*CD)) + geom_line(linetype = "dashed") + 
  # ylim(0, 15) +
  geom_line(data = totalnb, aes(Month, 100*field_cd)) +
  theme_tufte(base_size = 11) + 
  ylab("% C/D Infestation") +
  xlab("Month") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 3:12) +
  # geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 3, y = dat[1, 4]*100 + 2, label = "Threshold") +
  annotate("text", x = 3.2, y = dat[1, 5]*100 + 3, label = "Farm-level \n Infestation") +
  annotate("text", x = 3, y = -1, label = "No Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 4, y = -1, label = "No Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 5, y = -1, label = "Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 6, y = -1, label = "Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 7, y = -1, label = "Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 8, y = -1, label = "Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 9, y = -1, label = "Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 10, y = -1, label = "Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 11, y = -1, label = "Spray", alpha = 0.5, size = 3) +
  annotate("text", x = 12, y = -1, label = "No Spray", alpha = 0.5, size = 3)
  
  

# theme(legend.position = "top", 
       #legend.justification = c("left", "top"), 
       # legend.box.background = element_rect(colour = "grey"), 
       # legend.title = element_blank(), legend.key = element_blank()) +
  #theme(legend.position = c(.85,1), 
  #     legend.justification = c("left", "top"), 
  #     legend.box.background = element_rect(colour = "grey"), 
  #     legend.title = element_blank(), legend.key = element_blank()) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5)
