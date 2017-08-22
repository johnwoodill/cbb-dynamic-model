rm(list=ls())

library(tidyverse)
library(markovchain)

source("R/markovcalibration.R")

# Calibrate matrices
calibration_type <- "dissect"
source("2-calibrate_markov_chains.R")

# Call cherry pricing function
source("R/cherrypricing.R")

# Get model parameters
source("1-parameters.R")


# Did not follow IPM
# cv <- c(30, 50, 20)
# il <- c(45, 55)

# Set up initial values
# Followed IPM

# Initial dissect levels
cv <- c(55, 25, 20)  # Infestation levels

# Initial Infestation levels
il <- c(10, 90)

# Always Spray Model setup
##############################################



# Always Spray Model
{
mat <- data.frame(Month = c(3:12),
                  ab_live = rep(0, 10), ab_dead = rep(0, 10), cd = rep(0, 10), spray = rep(0, 10), inf = rep(0, 10), chart = rep(0, 10))
mat[1,2:4] <- cv
mat$inf[1] <- il[1]
mat$ni[1] <- 100 - il[1]
mat$chart[1] <- (mat$inf[1])*(mat[1,2])/100

for (i in 1:9){
  if (i == 1){
  # Check infestation values = 100
  if (sum(cv) != 100) stop ("Infestation Levels do not equal 100")
  if (sum(il) != 100) stop ("Field Infestation values do not equal 100")
  }
  mat$chart[i] <- (mat$inf[i])*(mat[i,2])/100
  mat$field_ablive[i] <- (cv[1])*(mat$inf[i])/100
  mat$field_abdead[i] <- (cv[2])*(mat$inf[i])/100
  mat$field_cd[i] <- (cv[3])*(mat$inf[i])/100
  
  # Always spray
  choice <- 1
  cv <- choice*cv %*% sp_mcListFit$estimate[[i]][] 
  if (length(spi_mcListFit$estimate[[i]][]) == 1) {
    il <- il
  } else {
    il <- choice*il %*% spi_mcListFit$estimate[[i]][] 
    }
  mat[i+1, 2:4] <- cv
  mat[i+1, 6] <- il[1]
  mat$spray[i] <- choice
  mat$price[i] <- cherrypricing(mat$field_cd[i]/100)
  mat$harvest_s[i] <- harvestschedule[i]
  mat$harvest_c[i] <- cherry_per_acre*acres*harvestpercentages[i]
  mat$cd_damage[i] <- mat$harvest_s[i]*(mat$field_cd[i]/100)*harvestpercentages[i]*cherry_per_acre*acres
  mat$nb[i] <- (mat$harvest_c[i] - mat$cd_damage[i])*mat$price[i] - mat$spray[i]*cost_s - cost_h*mat$harvest_c[i]
  
  if (i == 9){
    mat$chart[i+1] <- (mat$inf[i+1])*(mat[i+1,2])/100
    choice <- 1
    mat$field_ablive[i+1] <- (cv[1])*(mat$inf[i+1])/100
    mat$field_abdead[i+1] <- (cv[2])*(mat$inf[i+1])/100
    mat$field_cd[i+1] <- (cv[3])*(mat$inf[i+1])/100
    mat$spray[i+1] <- choice  
    mat$price[i+1] <- cherrypricing(mat$field_cd[i]/100)
    mat$harvest_s[i+1] <- harvestschedule[i+1]
    mat$harvest_c[i+1] <- cherry_per_acre*acres*harvestpercentages[i+1]
    mat$cd_damage[i+1] <- mat$harvest_s[i+1]*(mat$field_cd[i+1]/100)*harvestpercentages[i+1]*cherry_per_acre*acres
    mat$nb[i+1] <- (mat$harvest_c[i+1] - mat$cd_damage[i+1])*mat$price[i+1] - mat$spray[i+1]*cost_s - cost_h*mat$harvest_c[i+1]
  
  }
}

mat
sum(mat$nb)

}
mat$model <- "AS"
saveRDS(mat, "results/alwaysspray.rds")
