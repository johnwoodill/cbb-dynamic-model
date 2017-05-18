#' 2-calibrate_markov_chains
#' 
#' 
# Load function for time-inhomogenous Markov chain estimation
source("R/markovcalibration.R")

# Non-monotonic calibration from SHAC data (from shac_data_calibration.R)

# Spray matrix
# sp_mat <- data.frame(A = c(rep(0,9)),
#                      B = c(56, 52.80000, 54.44000, 27.70755, 21.34375, 15.38372, 15.65060, 21.33333, 19.92308),
#                      C = c(18, 20.40000, 23.40000, 44.79245, 34.88542, 47.74419, 22.97590, 37.00000, 29.46154),
#                      D = c(12, 15.000000,  5.040000,  9.323810, 21.197917,  8.430233, 12.180723, 17.277778, 12.692308))
# 
# sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))   
# sp_mat <- round(sp_mat, 0)
# sp_mcListFit <- markovcalibration(sp_mat)
# 
# 
# # No spray calibration
# nsp_mat <- data.frame(A = c(rep(0, 9)),                                        # Not Infested
#                   B = c(50, 45, 35, 30, 25, 20, 18, 20, 10),    # A/B Live
#                   C = c(3, 5, 8, 10, 8, 5, 6, 8, 10),      # A/B Dead
#                   D = c(6, 8, 11, 15, 20, 15, 25, 40, 45))       # C/D Position
# 
# nsp_mat$A <- apply(nsp_mat, 1, function(x) 100 - sum(x))    
# 
# nsp_mcListFit <- markovcalibration(nsp_mat)

# ggplot
# p <- nsp_mat
# p$ month <- 1:nrow(p)
# p <- gather(p, key = month, value = value)
# p$position <- factor(p$position, levels = c("A", "B", "C", "D"))
# names(p) <- c("month", "position", "value")
# ggplot(p, aes(month, value, color = position)) + geom_smooth(se = FALSE)


#-------------------------------------------------------
#Monotonically increasing C/D position
#------------------------------------------------------
# Spray calibration
# sp_mat <- data.frame(A = c(rep(0, 13)),                                       # Not Infested
#                   B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15, 14),  # A/B Live
#                   C = c(10, 15, 20, 23, 30, 34, 34, 37, 40, 41, 43, 44, 43),  # A/B Dead
#                   D = c(0, 1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 8))         # C/D Position

# sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))                        # Not Infested
# 
# sp_mcListFit <- markovcalibration(sp_mat)
# 
# # No spray calibration
# nsp_mat <- data.frame(A = c(rep(0, 13)),                                        # Not Infested
#                   B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15, 14),    # A/B Live
#                   C = c(5, 7, 10, 12, 15, 17, 18, 19, 20, 22, 23, 24, 25),      # A/B Dead
#                   D = c(1, 5, 8, 11, 15, 20, 25, 35, 40, 50, 55, 60, 61))       # C/D Position
# 
# nsp_mat$A <- apply(nsp_mat, 1, function(x) 100 - sum(x))                        # Not Infested
# 
# nsp_mcListFit <- markovcalibration(nsp_mat)
# ------------------------------------------------------


# From Luis's data
#-------------------------------------------------------
dat2 <- structure(list(month = c(5, 6, 7, 8, 9, 10, 11, 12), ab_live = c(5.3284558625, 
2.25167900789474, 0.8076295045, 0.344990258095238, 0.28138321, 
0.58584996, 1.019458864, 6.61812641428571), ab_dead = c(1.48799575666667, 
0.936575646315789, 1.2895422885, 0.772706474285714, 0.854565458, 
0.417742084545455, 0.486056308, 0.694683657142857), cd = c(1.110119095, 
0.402585240526316, 0.948419032, 1.25611749904762, 1.32305669, 
2.29149948681818, 3.11510732, 7.15907681428572)), class = "data.frame", row.names = c(NA, 
-8L), spec = structure(list(cols = structure(list(Sampling = structure(list(), class = c("collector_integer", 
"collector")), Distric = structure(list(), class = c("collector_character", 
"collector")), Elevation = structure(list(), class = c("collector_character", 
"collector")), Farm = structure(list(), class = c("collector_character", 
"collector")), Evaluation = structure(list(), class = c("collector_integer", 
"collector")), Date = structure(list(), class = c("collector_character", 
"collector")), CBB_Inf = structure(list(), class = c("collector_double", 
"collector")), B.bassiana = structure(list(), class = c("collector_double", 
"collector")), AB = structure(list(), class = c("collector_double", 
"collector")), CD = structure(list(), class = c("collector_double", 
"collector")), Mortality = structure(list(), class = c("collector_double", 
"collector")), Absent = structure(list(), class = c("collector_double", 
"collector"))), .Names = c("Sampling", "Distric", "Elevation", 
"Farm", "Evaluation", "Date", "CBB_Inf", "B.bassiana", "AB", 
"CD", "Mortality", "Absent")), default = structure(list(), class = c("collector_guess", 
"collector"))), .Names = c("cols", "default"), class = "col_spec"), .Names = c("month", 
"ab_live", "ab_dead", "cd"))

# # Spray calibration
# sp_mat <- data.frame(A = c(rep(0, 8)),
#                      B = dat2$ab_live,
#                      C = dat2$ab_dead,
#                      D = dat2$cd)
# 
# sp_mat <- as.data.frame(apply(sp_mat, 2, function(x) ceiling(x)))
# sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))                        # Not Infested
# sp_mcListFit <- markovcalibration(sp_mat)
# 
# # No spray calibration
# 
# nsp_mat <- data.frame(A = c(rep(0, 8)),
#                       B = 5*sp_mat$B,
#                       C = sp_mat$C,
#                       D = 5*sp_mat$D)
# 
# nsp_mat <- as.data.frame(apply(nsp_mat, 2, function(x) ceiling(x)))
# nsp_mat$A <- apply(nsp_mat, 1, function(x) 100 - sum(x))         
# nsp_mcListFit <- markovcalibration(nsp_mat)

# -------------------------------------------
# 12 month

# Spray calibration
# sp_mat <- data.frame(A = c(rep(0, 13)),
#                      B = c(8, 7, 7, 5, 5, 3, 1, 1, 1, 2, 3, 6, 7),
#                      C = c(4, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1),
#                      D = c(6, 5, 3, 3, 2, 1, 1, 1, 1, 2, 3, 6, 7))

# U Curve data
# sp_mat <- data.frame(A = c(rep(0, 13)),
#                       B = c(6.5, 6, 5.8, 5.5, 5.33, 2.25, 0.80, 0.34, 0.28, 0.59, 1.02, 6.62, 7),
#                       C = c(2.2, 2.1, 1.9, 1.7, 1.49, 0.94, 1.29, 0.77, 0.85, 0.42, 0.49, 0.69, 1),
#                       D = c(6, 5, 3.5, 3, 1.11, 0.40, 0.95, 1.26, 1.32, 2.29, 3.11, 7.16, 7.4))

# Only increasing CD
 sp_mat <- data.frame(A = c(rep(0, 13)),
                       B = c(6.5, 6, 5.8, 5.5, 5.33, 2.25, 0.80, 0.34, 0.28, 0.59, 1.02, 6.62, 7),
                       C = c(2.2, 2.1, 1.9, 1.7, 1.49, 0.94, 1.29, 0.77, 0.85, 0.42, 0.49, 0.69, 1),
                       D = c(.1, .1, .2, .3, .34, 0.4, 0.95, 1.26, 1.32, 2.29, 3.11, 7.16, 7.4))

sp_mat <- as.data.frame(apply(sp_mat, 2, function(x) ceiling(x)))
sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))                        # Not Infested
sp_mcListFit <- markovcalibration(sp_mat)

# No spray calibration
nsp_mat <- data.frame(A = c(rep(0, 13)),
                      B = 6*sp_mat$B,
                      C = 6*sp_mat$C,
                      D = 6*sp_mat$D)

nsp_mat <- as.data.frame(apply(nsp_mat, 2, function(x) ceiling(x)))
nsp_mat$A <- apply(nsp_mat, 1, function(x) 100 - sum(x))         
nsp_mcListFit <- markovcalibration(nsp_mat)
# -----------------------------------------------------------------------------
  sp_mat <- data.frame(A = c(rep(0, 13)),
                        B = c(6.5, 6, 5.8, 5.5, 5.33, 2.25, 0.80, 0.34, 0.28, 0.59, 1.02, 6.62, 7),
                        C = c(2.2, 2.1, 1.9, 1.7, 1.49, 0.94, 1.29, 0.77, 0.85, 0.42, 0.49, 0.69, 1),
                        D = c(6, 5, 3.5, 3, 1.11, 0.40, 0.95, 1.26, 1.32, 2.29, 3.11, 7.16, 7.4))

 sp_mat <- data.frame(A = c(rep(0, 13)),
                       B = c(6.5, 6, 5.8, 5.5, 5.33, 2.25, 0.80, 0.34, 0.28, 0.59, 1.02, 6.62, 7),
                       C = c(2.2, 2.1, 1.9, 1.7, 1.49, 0.94, 1.29, 0.77, 0.85, 0.42, 0.49, 0.69, 1),
                       D = c(.1, .1, .2, .3, .34, 0.4, 0.95, 1.26, 1.32, 2.29, 3.11, 7.16, 7.4))
# 
# # Graphics of calibrated data
  cd_data <- data.frame(month = dat2$month, cd = dat2$cd, data = "Data")
  cd_calib <- data.frame(month = seq(1,13, 1), cd = sp_mat$D, data = "Calibration")
  cd <- rbind(cd_data, cd_calib)
  cd_p <- ggplot(cd, aes(month, cd, group = data, linetype = data)) + geom_line() + geom_point() + ggtitle("Spray - Position C/D")
  cd_nospray <- data.frame(month = seq(1,13, 1), cd = nsp_mat$D)
  cd_nsp_p <- ggplot(cd_nospray, aes(month, cd)) + geom_line() + ggtitle("No Spray - Position C/D")
  plot_grid(cd_p, cd_nsp_p, ncol = 1)
  # #
# # #
  ablive_data <- data.frame(month = dat2$month, ablive = dat2$ab_live, data = "Data")
  ablive_calib <- data.frame(month = seq(1, 13, 1), ablive = sp_mat$B, data = "Calibration")
  ablive <- rbind(ablive_data, ablive_calib)
  ablive_p <- ggplot(ablive, aes(month, ablive, group = data, linetype = data)) + geom_line() + geom_point() + ggtitle("Spray Position A/B Live")
  ablive_nospray <- data.frame(month = seq(1,13, 1), ablive = nsp_mat$B)
  ablive_nsp_p <- ggplot(ablive_nospray, aes(month, ablive)) + geom_line() + ggtitle("No Spray - Position A/B Live")
  plot_grid(ablive_p, ablive_nsp_p, ncol = 1)
# #
# #
  abdead_data <- data.frame(month = dat2$month, abdead = dat2$ab_dead, data = "Data")
  abdead_calib <- data.frame(month = seq(1, 13, 1), abdead = sp_mat$C, data = "Calibration")
  abdead <- rbind(abdead_data, abdead_calib)
  abdead_p <- ggplot(abdead, aes(month, abdead, group = data, linetype = data)) + geom_line() + geom_point() + ggtitle("Spray Position A/B Dead")
  abdead_nospray <- data.frame(month = seq(1,13, 1), abdead = nsp_mat$C)
  abdead_nsp_p <- ggplot(abdead_nospray, aes(month, abdead)) + geom_line() + ggtitle("No Spray - Position A/B Dead")
  plot_grid(abdead_p, abdead_nsp_p, ncol = 1)
