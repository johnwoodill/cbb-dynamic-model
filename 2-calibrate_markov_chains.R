source("R/markovcalibration.R")

# Called from each model. Be sure to set 'calibration_type' to 'dissect' or field

# Markov calibration
#-------------------------------------------------------

# Original matrices
# Based on field level data and Luis understand meaning calibration based on USDA data
# Spray calibration
# Dissect level 
# dat1 <- data.frame(month = 3:12,
#                    A = c(58, 69, 70, 48, 22, 5, 16, 10, 6, 3),      # A/B Live
#                    B = rep(0, 10),                                   # A/B Dead
#                    C = c(20, 22, 24, 45, 48, 73, 64, 73, 84, 86),   # C/D Position
#                    INF = c(21, 13, 5, 3, 1, 2, 4, 3, 11, 13))
# dat1$NI <- 100 - dat1$INF
# dat1$B <- 100-(dat1$A+dat1$C)
# 
# # No spray calibration (From luis)
# # Dissect level
# dat2 <- data.frame(month = 3:12,
#                    A = c(74, 69, 66, 60, 48, 30, 22, 16, 10, 3),      # A/B Live
#                    B = rep(0, 10),                                    # A/B Dead
#                    C = c(25, 27, 33, 39, 48, 66, 77, 82, 88, 90),     # C/D Position
#                    INF = c(43, 38, 33, 37, 41, 45, 48, 52, 56, 60))
# dat2$B <- 100-(dat2$A+dat2$C)
# dat2$NI <- 100 - dat2$INF

# Check
dat1 <- data.frame(month = 3:12,
                   A = c(58, 69, 70, 48, 22, 5, 16, 10, 7, 4 ),      # A/B Live
                   B = rep(0, 1),                                    # A/B Dead
                   C = c(20, 22, 24, 45, 48, 73, 64, 73, 84, 86),    # C/D Position
                   INF = c(21, 17, 12, 8, 5, 6, 9, 10, 14, 17))
dat1$NI <- 100 - dat1$INF
dat1$B <- 100-(dat1$A+dat1$C)

# No spray calibration (From luis)
# Dissect level
dat2 <- data.frame(month = 3:12,
                   A = c(74, 69, 66, 60, 48, 30, 22, 16, 10, 3),      # A/B Live
                   B = rep(0, 10),                                    # A/B Dead
                   C = c(25, 27, 33, 39, 48, 66, 77, 82, 89, 93),     # C/D Position
                   INF = c(43, 38, 33, 40, 44, 48, 53, 59, 66, 80))
dat2$B <- 100-(dat2$A+dat2$C)
dat2$NI <- 100 - dat2$INF


# Based on field level data and Luis understand meaning calibration based on USDA data
# Spray calibration
# Dissect level 
# dat1 <- data.frame(month = 3:12,
#                    A = c(58, 69, 70, 48, 22, 5, 16, 10, 5, 3 ),      # A/B Live
#                    B = rep(0, 1),      # A/B Dead
#                    C = c(20, 22, 24, 45, 48, 73, 64, 73, 84, 86),    # C/D Position
#                   INF = c(21, 13, 5, 3, 1, 2, 4, 3, 11, 13))
# dat1$NI <- 100 - dat1$INF
# dat1$B <- 100-(dat1$A+dat1$C)
# 
# # No spray calibration (From luis)
# # Dissect level
# dat2 <- data.frame(month = 3:12,
#                    A = c(74, 69, 66, 60, 48, 30, 22, 16, 10, 3),      # A/B Live
#                    B = rep(0, 10),             # A/B Dead
#                    C = c(25, 27, 33, 39, 48, 66, 77, 82, 89, 93),     # C/D Position
#                   INF = c(43, 38, 33, 40, 44, 48, 53, 59, 66, 80))
# dat2$B <- 100-(dat2$A+dat2$C)
# dat2$NI <- 100 - dat2$INF

# if (calibration_type  == "dissect"){
#   
#   
# # Dissect level
# #########################################
# #
# # Spray matrix for infestation rate
# #
# spi_mat <- dat1[,5:6]
# names(spi_mat) <- c("I", "NI")
# spi_mat
# spi_mat <- as.data.frame(apply(spi_mat, 2, function(x) floor(x)))
# spi_mat
# spi_mcListFit <- markovcalibration(spi_mat)
# spi_mcListFit
# 
# # Spray Infestation levels
# sp_mat <- dat1
# sp_mat <- sp_mat[,2:4]
# sp_mat <- as.data.frame(apply(sp_mat, 2, function(x) floor(x)))
# sp_mat
# sp_mcListFit <- markovcalibration(sp_mat)
# sp_mcListFit
# 
# #########################################
# #
# # No Spray matrix for infestation rate
# #
# nspi_mat <- dat2[,5:6]
# names(nspi_mat) <- c("I", "NI")
# nspi_mat
# nspi_mat <- as.data.frame(apply(nspi_mat, 2, function(x) floor(x)))
# nspi_mat
# nspi_mcListFit <- markovcalibration(nspi_mat)
# nspi_mcListFit
# 
# # No Spray Infestation levels
# nsp_mat <- dat2
# nsp_mat <- nsp_mat[,2:4]
# nsp_mat <- as.data.frame(apply(nsp_mat, 2, function(x) floor(x)))
# nsp_mat
# nsp_mcListFit <- markovcalibration(nsp_mat)
# nsp_mcListFit
# }

#-----------------------------
# Field Level Calibration

#if (calibration_type  == "field"){
# Field Level
# #   
dat3 <- data.frame(A = (dat1$A*dat1$INF)/100,
                   B = (dat1$B*dat1$INF)/100,
                   C = (dat1$C*dat1$INF)/100)

dat4 <- data.frame(A = (dat2$A*dat2$INF)/100,
                   B = (dat2$B*dat2$INF)/100,
                   C = (dat2$C*dat2$INF)/100)
# 
# dat3 <- data.frame(A = c(12.18, 11.97, 11.50, 10.44, 9.22, 8.10, 6.64, 4.30, 2.66, 1.39),    # A/B Live
#                    B = c(4.62, 3.17, 2.30, 1.61, 1.30, 0.44, 0.80, 0.51, 1.10, 1.43),     # A/B Dead
#                    C = c(0.20, 1.46, 1.90, 3.35, 4.48, 6.46, 7.56, 8.19, 9.24, 11.18))     # C/D Position
# 
# 
# dat4 <- data.frame(A = c(31.82, 26.22, 21.78, 22.20, 19.68, 18.50, 16.56, 15.32, 13.60, 11.80),      # A/B Live
#                    B = c(0.43, 1.52, 0.33, 0.37, 1.64, 1.80, 0.48, 1.04, 0.56, 2.40),                # A/B Dead
#                    C = c(5.75, 8.26, 9.89, 13.43, 21.68, 29.70, 36.96, 44.64, 50.84, 60))     # C/D Position

#########################################
#
# Spray matrix for infestation rate Ab live AB dead CD and NI
#
sp_mat <- dat3
sp_mat
sp_mat <- as.data.frame(apply(sp_mat, 2, function(x) ceiling(x)))
sp_mat$D <- 100 - (sp_mat$A + sp_mat$B + sp_mat$C)
sp_mat

sp_mcListFit <- markovcalibration(sp_mat)
sp_mcListFit


#########################################
#
# No Spray matrix for infestation rate
#
nsp_mat <- dat4
nsp_mat
nsp_mat <- as.data.frame(apply(nsp_mat, 2, function(x) ceiling(x)))
nsp_mat$D <- 100 - (nsp_mat$A + nsp_mat$B + nsp_mat$C)
nsp_mat
nsp_mcListFit <- markovcalibration(nsp_mat)
nsp_mcListFit

#}

