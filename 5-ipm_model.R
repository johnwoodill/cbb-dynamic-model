rm(list=ls())

library(tidyverse)
library(markovchain)

source("R/markovcalibration.R")

# Calibrate matrices
source("2-calibrate_markov_chains.R")

# Call dynamic pricing
source("R/cherrypricing.R")

# Get model parameters
source("1-parameters.R")

# decision function
source("R/decision.R")

# Add $100 per acre due to sampling each month
cost_s <- 214


# Convert to dissect level
# dat3$A <- (dat3$A/rowSums(dat3[, c("A", "B", "C")]))*100
# dat3$B <- (dat3$B/rowSums(dat3[, c("A", "B", "C")]))*100
# dat3$C <- (dat3$C/rowSums(dat3[, c("A", "B", "C")]))*100
# 
# dat4$A <- (dat4$A/rowSums(dat4[, c("A", "B", "C")]))*100
# dat4$B <- (dat4$B/rowSums(dat4[, c("A", "B", "C")]))*100
# dat4$C <- (dat4$C/rowSums(dat4[, c("A", "B", "C")]))*100



# IPM Model setup
##############################################

# Did not follow IPM
# Initial dissect levels
# Field-level AB live: 13.5%
#             AB Dead: 22.5%
#                  CD: 9%

# cv <- c(30, 50, 20)
# il <- c(45, 55)

cv <- c(0.055, 0.025, .03)
cv[4] <- 1 - sum(cv)


# Followed IPM
# Initial dissect levels
# Field-level AB live: 5.5%
#             AB Dead: 2.5%
#                  CD: 2%

##############################
# Infestation levels
#cv <- c(55, 25, 20) 
#il <- c(10, 90)

# IPM Model
mat <- data.frame(Month = c(3:12),
                  ab_live = rep(0, 10), 
                  ab_dead = rep(0, 10), 
                  cd = rep(0, 10), 
                  spray = rep(0, 10), 
                  inf = rep(0, 10), 
                  dissect_ab_live = rep(0, 10),
                  chart = rep(0, 10))

mat[1, 2:4] <- cv[1:3]
mat$inf[1] <- sum(cv[1:3])

# Calculate dissect level and chart color
mat$chart[1] <- ((mat[1, 2]/mat$inf[1])) * mat$inf[1] * 100

mat$dissect_ab_live[1] <- ((mat[1, 2]/mat$inf[1])) 

# 1-2 Consider spraying 
# 2-5 Critical level to start spraying 
# 5-10 You are starting to lose money, but may still want to spray
# 10-19.99 You are losing money
# 20 - Processors may reject your harvest

chart_decision <- 1

# Decision based on sampling
for (i in 1:9){
  # if (i == 1){
  # # Check infestation values = 100
  # if (sum(cv) != 100) stop ("Infestation Levels do not equal 100")
  # if (sum(il) != 100) stop ("Field Infestation values do not equal 100")
  # }
  
  
  mat$chart[i] <- ((mat[i, 2]/mat$inf[i])) * mat$inf[i] * 100
  # mat$field_ablive[i] <- (cv[1])*(mat$inf[i])/100
  # mat$field_abdead[i] <- (cv[2])*(mat$inf[i])/100
  # mat$field_cd[i] <- (cv[3])*(mat$inf[i])/100
  
  choice <- ifelse(mat$chart[i] >= chart_decision, 1, 0)
  cv <- choice*cv %*% sp_mcListFit$estimate[[i]][] + (1 - choice)*cv %*% nsp_mcListFit$estimate[[i]][]
  
  # if (length(spi_mcListFit$estimate[[i]][]) == 1) {
  #   il <- il
  # } else {
  #   il <- choice*il %*% spi_mcListFit$estimate[[i]][] + (1 - choice)*il %*% nspi_mcListFit$estimate[[i]][]
  #   }
  
  mat[i+1, 2:4] <- cv[1:3]
  mat[i+1, 6] <- sum(cv[1:3])
  mat$dissect_ab_live[i] <- ((mat[i, 2]/mat$inf[i])) 
  
  mat$spray[i] <- choice
  mat$price[i] <- cherrypricing(mat$cd[i]/100)
  mat$harvest_s[i] <- harvestschedule[i]
  mat$harvest_c[i] <- cherry_per_acre*acres*harvestpercentages[i]
  mat$cd_damage[i] <- mat$harvest_s[i]*(mat$cd[i])*harvestpercentages[i]*cherry_per_acre*acres
  mat$nb[i] <- (mat$harvest_c[i] - mat$cd_damage[i])*mat$price[i] - mat$spray[i]*cost_s - cost_h*mat$harvest_c[i]
  
  if (i == 9){
    mat$chart[i+1] <- ((mat[i, 2]/mat$inf[i])) * mat$inf[i] * 100
    choice <- ifelse(mat$chart[i+1] >= chart_decision, 1, 0)
    mat$ab_live[i+1] <- (cv[1])
    mat$ab_dead[i+1] <- (cv[2])
    mat$dissect_ab_live[i+1] <- ((mat[i+1, 2]/mat$inf[i+1])) 
    mat$cd[i+1] <- (cv[3])
    mat$spray[i+1] <- choice
    
    mat$price[i+1] <- cherrypricing(mat$cd[i]/100)
    mat$harvest_s[i+1] <- harvestschedule[i+1]
    mat$harvest_c[i+1] <- cherry_per_acre*acres*harvestpercentages[i+1]
    mat$cd_damage[i+1] <- mat$harvest_s[i+1]*(mat$cd[i+1])*harvestpercentages[i+1]*cherry_per_acre*acres
    mat$nb[i+1] <- (mat$harvest_c[i+1] - mat$cd_damage[i+1])*mat$price[i+1] - mat$spray[i+1]*cost_s - cost_h*mat$harvest_c[i+1]

  }
}

mat

# sum(mat$nb)

# mat$model <- "IPM Choice"
write_csv(mat, "results/ipmspray.csv")
