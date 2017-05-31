library(tidyr)
library(ggplot2)
library(dplyr)
library(markovchain)
library(readr)
library(lubridate)
library(cowplot)
source("R/markovcalibration.R")
calibration_type <- "dissect"
source("2-calibrate_markov_chains.R")

# IPM Model setup
##############################################

# Did not follow IPM
cv <- c(30, 50, 20)
il <- c(45, 55)

# Followed IPM
cv <- c(55, 25, 20)  # Infestation levels
il <- c(10, 90)

# Dynamic Model
{
mat <- data.frame(Month = c("March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"),
                  ab_live = rep(0, 10), ab_dead = rep(0, 10), cd = rep(0, 10), spray = rep(0, 10), inf = rep(0, 10), chart = rep(0, 10))
mat[1,2:4] <- cv
mat$inf[1] <- il[1]
mat$chart[1] <- (mat$inf[1])*(mat[1,2])/100

mat
# 1-2 Consider spraying 
# 2-5 Critical level to start spraying 
# 5-10 You are starting to lose money, but may still want to spray
# 10-19.99 You are losing money
# 20 - Processors may reject your harvest

decision <- 2

# Decision based on sampling
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
  choice <- ifelse(mat$chart[i] >= decision, 1, 0)
  cv <- choice*cv %*% sp_mcListFit$estimate[[i]][] + (1 - choice)*cv %*% nsp_mcListFit$estimate[[i]][]
  if (length(spi_mcListFit$estimate[[i]][]) == 1) {
    il <- il
  } else {
    il <- choice*il %*% spi_mcListFit$estimate[[i]][] + (1 - choice)*il %*% nspi_mcListFit$estimate[[i]][]
    }
  mat[i+1, 2:4] <- cv
  mat[i+1, 6] <- il[1]
  mat$spray[i] <- choice
  if (i == 9){
    mat$chart[i+1] <- (mat$inf[i+1])*(mat[i+1,2])/100
    choice <- ifelse(mat$chart[i+1] >= decision, 1, 0)
    mat$field_ablive[i+1] <- (cv[1])*(mat$inf[i+1])/100
    mat$field_abdead[i+1] <- (cv[2])*(mat$inf[i+1])/100
    mat$field_cd[i+1] <- (cv[3])*(mat$inf[i+1])/100
    mat$spray[i+1] <- choice
  }
}

mat
}
