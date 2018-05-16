source("R/markovcalibration.R")

# Called from each model. Be sure to set 'calibration_type' to 'dissect' or field

# Markov calibration
#-------------------------------------------------------

# From USDA calibration
# dat3 <- structure(list(percent_ab_live = c(11.3965602251124, 4.86294082354106,
# 2.69607629980722, 0.359477124183007, 0.283276384661366, 0.115273098776565,
# 0.507092077478162, 0.18316266780577, 0.107342864291601), percent_ab_dead = c(0,
# 0, 0, 0, 0.297524490141022, 0.383707788205918, 1.05803134390251,
# 0.381172514557036, 1.08504111506613), percent_cd_live = c(0,
# 0, 0, 0, 0.384317510598303, 1.21981331951074, 2.64082405717489,
# 2.31680426543307, 10.1716918515493)), .Names = c("percent_ab_live",
# "percent_ab_dead", "percent_cd_live"), row.names = c(NA, -9L), class = c("tbl_df",
# "tbl", "data.frame"))
#  
# dat3$inf <- rowSums(dat3)
# head(dat3)
# dat3$month <- c(4:12)
# dat3 <- gather(dat3, key = position, value = value, -month)
# ggplot(dat3, aes(x = month, y =value, color = position)) + geom_line()
# #------------------------------------------------------------
# # Original matrices
# # Based on field level data from Luis
# # Spray calibration
# dat1 <- data.frame(month = 3:12,
#                    A = c(58, 69, 70, 48, 22, 5, 16, 10, 5, 3 ),      # A/B Live
#                    B = rep(0, 1),                                    # A/B Dead
#                    C = c(20, 22, 24, 45, 48, 73, 64, 73, 84, 86),    # C/D Position
#                   INF = c(21, 13, 5, 3, 1, 2, 4, 3, 11, 13))
# dat1$NI <- 100 - dat1$INF
# dat1$B <- 100-(dat1$A+dat1$C)
# 
# # No spray calibration (From luis)
# # Dissect level
# dat2 <- data.frame(month = 3:12,
#                    A = c(74, 69, 66, 60, 48, 30, 22, 16, 10, 3),      # A/B Live
#                    B = rep(0, 10),                                    # A/B Dead
#                    C = c(25, 27, 33, 39, 48, 66, 77, 82, 89, 93),     # C/D Position
#                   INF = c(43, 38, 33, 40, 44, 48, 53, 59, 66, 80))
# dat2$B <- 100-(dat2$A+dat2$C)
# dat2$NI <- 100 - dat2$INF
# 
# dat3 <- data.frame(A = (dat1$A*dat1$INF)/100,
#                    B = (dat1$B*dat1$INF)/100,
#                    C = (dat1$C*dat1$INF)/100)
# 
# dat4 <- data.frame(A = (dat2$A*dat2$INF)/100,
#                    B = (dat2$B*dat2$INF)/100,
#                    C = (dat2$C*dat2$INF)/100)
# dat3$D <- rowSums(dat3)
# names(dat3) <- c("AB_LIVE", "AB_DEAD", "CD", "INF")
# 
# dat4$D <- rowSums(dat4)
# names(dat4) <- c("AB_LIVE", "AB_DEAD", "CD", "INF")

# head(dat3)

# FINAL RESULTS FOR PAPER FOR DRAFT DO NOT DELETE
# dat3 <- data.frame(A = c(12.18, 11.97, 11.50, 10.44, 9.22, 8.10, 6.64, 4.30, 2.66, 1.39), # A/B Live
#                     B = c(4.62, 3.17, 2.30, 1.61, 1.30, 0.44, 0.80, 0.51, 1.10, 1.43),     # A/B Dead
#                     C = c(0.20, 1.46, 1.90, 3.35, 4.48, 6.46, 7.56, 8.19, 9.24, 11.18))    # C/D Position
# 
# 
# dat4 <- data.frame(A = c(33.82, 26.22, 21.78, 22.20, 19.68, 13.50, 10.56, 5.32, 3.60, 1.80),      # A/B Live
#                     B = c(0.43, 1.52, 0.33, 0.37, 1.64, 1.80, 0.48, 1.04, 0.56, 2.40),                # A/B Dead
#                     C = c(5.75, 8.26, 9.89, 13.43, 21.68, 29.70, 36.96, 44.64, 50.84, 60))


# Sent to Stuart and PingSun (2/28/18)
# Confirmed with Stuart and PingSun as valid
dat3 <- data.frame(A = c(12.18, 11.97, 9.50, 6.44, 5.22, 5.10, 3.64, 2.30, 1.66, 1.39), # A/B Live
                    B = c(3.62, 2.17, 1.30, 1.61, 1.30, 0.44, 0.80, 0.51, 1.10, 1.43),     # A/B Dead
                    C = c(3.20, 2.46, 1.90, 1.35, 1.48, 2.46, 4.56, 5.19, 9.24, 11.18))    # C/D Position


dat4 <- data.frame(A = c(33.82, 30.22, 25.78, 22.20, 19.68, 15.50, 10.56, 7.32, 6.60, 5.80),      # A/B Live
                    B = c(0.43, 1.52, 0.33, 0.37, 1.64, 1.80, 0.48, 1.04, 0.56, 1.40),                # A/B Dead
                    C = c(12.75, 10.26, 9.89, 10.43, 15.68, 23.70, 32.96, 40.64, 44.84, 54))

# dat3 <- data.frame(A = c(12.18, 11.97, 9.50, 6.44, 5.22, 5.10, 3.64, 2.30, 1.66, 1.39), # A/B Live
#                     B = c(3.62, 2.17, 1.30, 1.61, 1.30, 0.44, 0.80, 0.51, 1.10, 1.43),     # A/B Dead
#                     C = c(3.20, 2.46, 1.90, 1.35, 0.48, 2.46, 4.56, 5.19, 9.24, 11.18))    # C/D Position
# 
# 
# dat4 <- data.frame(A = c(33.82, 30.22, 25.78, 22.20, 19.68, 15.50, 10.56, 7.32, 5.60, 3.80),      # A/B Live
#                     B = c(1.43, 1.52, 0.33, 0.37, 1.64, 1.80, 0.48, 1.04, 0.56, 2.40),                # A/B Dead
#                     C = c(10.75, 8.26, 7.89, 8.43, 15.68, 23.70, 32.96, 40.64, 47.84, 56))



#   
# inf <- rowSums(dat3)
#########################################
#
# Spray matrix for infestation rate Ab live AB dead CD and NI
#
sp_mat <- dat3
sp_mat
sp_mat <- as.data.frame(apply(sp_mat, 2, function(x) ceiling(x)))
sp_mat$D <- 100 - (sp_mat$A + sp_mat$B + sp_mat$C)
sp_mat
sp_mat$A + sp_mat$B + sp_mat$C

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

