library(tidyr)
library(ggplot2)
library(dplyr)
library(markovchain)
library(readr)
library(lubridate)
library(cowplot)
source("R/markovcalibration.R")

# Markov calibration
#-------------------------------------------------------

# Based on field level data and Luis understand meaning calibration based on USDA data
# Spray calibration
# Dissect level 
dat1 <- data.frame(month = 3:12,
                   A = c(58, 69, 70, 48, 22, 5, 16, 10, 5, 3 ),      # A/B Live
                   B = rep(0, 1),      # A/B Dead
                   C = c(20, 22, 24, 45, 48, 73, 64, 73, 84, 86),    # C/D Position
                  INF = c(21, 13, 5, 3, 1, 2, 4, 3, 11, 13))
dat1$NI <- 100 - dat1$INF
dat1$B <- 100-(dat1$A+dat1$C)

# No spray calibration (From luis)
# Dissect level
dat2 <- data.frame(month = 3:12,
                   A = c(74, 69, 66, 60, 48, 30, 22, 16, 10, 3),      # A/B Live
                   B = rep(0, 10),             # A/B Dead
                   C = c(25, 27, 33, 39, 48, 66, 77, 82, 89, 93),     # C/D Position
                  INF = c(43, 38, 33, 40, 44, 48, 53, 59, 66, 80))
dat2$B <- 100-(dat2$A+dat2$C)
dat2$NI <- 100 - dat2$INF

# Dissect level
# dat2 <- data.frame(month = 3:12,
#                    A = c(79, 73, 76, 60, 48, 20, 22, 16, 10, 3),      # A/B Live
#                    B = rep(0, 10),             # A/B Dead
#                    C = c(20, 22, 23, 39, 48, 76, 77, 82, 89, 93),     # C/D Position
#                   INF = c(43, 38, 33, 40, 44, 48, 53, 59, 66, 80))
# dat2$B <- 100-(dat2$A+dat2$C)
# dat2$NI <- 100 - dat2$INF

# Plot
newdat1 <- dat1[, 1:4]
names(newdat1) <- c("month", "AB_LIVE", "AB_DEAD", "CD")
newdat1 <- gather(newdat1, key, value, -month)
newdat1$key <- factor(newdat1$key)
p1 <- ggplot(newdat1, aes(month, value, color = key)) + geom_line() + ggtitle("Spray") + ylab("% Dissected Berries") + scale_x_continuous(breaks = c(3:12))
p1

newdat2 <- dat2[, 1:4]
names(newdat2) <- c("month", "AB_LIVE", "AB_DEAD", "CD")
newdat2 <- gather(newdat2, key, value, -month)
newdat2$key <- factor(newdat2$key)
p2 <- ggplot(newdat2, aes(month, value, color = key)) + geom_line() + ggtitle("No Spray") + ylab("% Dissected Berries")+ scale_x_continuous(breaks = c(3:12))
p2

newdat3 <- dat1[, c(1,5)]
p3 <- ggplot(newdat3, aes(month, INF)) + geom_line() + ggtitle("Spray \n Field Level Infestation") + ylab("% Berries in Field") + scale_x_continuous(breaks = c(3:12))
p3

newdat4 <- dat2[, c(1,5)]
p4 <- ggplot(newdat4, aes(month, INF)) + geom_line() + ggtitle("No Spray - Field Level Infestation") + ylab("% Berries in Field") + scale_x_continuous(breaks = c(3:12))
p4

plot_grid(p1, p2, ncol = 1)
plot_grid(p3, p4, ncol = 1)

#########################################
#
# Spray matrix for infestation rate
#
spi_mat <- dat1[,5:6]
names(spi_mat) <- c("I", "NI")
spi_mat
spi_mat <- as.data.frame(apply(spi_mat, 2, function(x) floor(x)))
spi_mat
spi_mcListFit <- markovcalibration(spi_mat)
spi_mcListFit

# Spray Infestation levels
sp_mat <- dat1
sp_mat <- sp_mat[,2:4]
sp_mat <- as.data.frame(apply(sp_mat, 2, function(x) floor(x)))
sp_mat
sp_mcListFit <- markovcalibration(sp_mat)
sp_mcListFit

#########################################
#
# No Spray matrix for infestation rate
#
nspi_mat <- dat2[,5:6]
names(nspi_mat) <- c("I", "NI")
nspi_mat
nspi_mat <- as.data.frame(apply(nspi_mat, 2, function(x) floor(x)))
nspi_mat
nspi_mcListFit <- markovcalibration(nspi_mat)
nspi_mcListFit

# No Spray Infestation levels
nsp_mat <- dat2
nsp_mat <- nsp_mat[,2:4]
nsp_mat <- as.data.frame(apply(nsp_mat, 2, function(x) floor(x)))
nsp_mat
nsp_mcListFit <- markovcalibration(nsp_mat)
nsp_mcListFit

##############################################
# Dynamic Model setup
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
