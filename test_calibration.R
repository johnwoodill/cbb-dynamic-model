library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

library(markovchain)

markovcalibration <- function(dat){
    
  # Build sequence from mat
  dat_tseq <- apply(t(dat), 2, function(x) rep(row.names(t(dat)), x))
  
  # Fit Markov Matrices to sequences
  dat_mcListFit <- markovchainListFit(data = dat_tseq[,1:ncol(dat_tseq)])
  
  return(dat_mcListFit)
}


# 
 dat <- read.csv("cbb dynamic model/data/luis_data.csv", stringsAsFactors = FALSE)
# dat$Date <- as.Date(dat$Date, format ="%b %d/%y")
# dat$month <- month(dat$Date)
# 
# dat <- filter(dat, Distric == "Kona")
# dat <- filter(dat, Elevation == "Med")
# newdat <- dat %>% 
#   group_by(month) %>% 
#   summarize(CBB_Inf = mean(CBB_Inf, na.rm = TRUE),
#             AB = mean(AB, na.rm = TRUE),
#             AB_DEAD = mean(Mortality, na.rm = TRUE),
#             CD = mean(CD, na.rm = TRUE),
#             Absent = mean(Absent, na.rm = TRUE))
# 
# newdat <- data.frame(month = c(1:12),
#                      CBB_INF = c(15,11,10,8,6.22,3.38,3.5,1.91,2.29,2.79,4.16,11),
#                      AB_LIVE = c(53,52,50,47,40,33.68,16.87,13.12,12.20,10.97,14.57,16.13),
#                      AB_DEAD = c(7,9,11,13,14.27,16.05,30.78,41.52,50.97,52.28,64.9,49.63),
#                      CD = c(7,9,11,13,14.27,16,30.75,41.52,50.97,52.28,56.9,60),
#                      ABSENT = c(38,35,33,35,27,32,33,24,17,23.8,14.5,6.7))
# newdat <- rbind(data.frame(month = 4, CBB_Inf = 8, AB = 47, AB_DEAD = 5, CD = 13, Absent = 35), newdat)
# newdat <- rbind(data.frame(month = 3, CBB_Inf = 10, AB = 50, AB_DEAD = 6, CD = 11, Absent = 33), newdat)
# newdat <- rbind(data.frame(month = 2, CBB_Inf = 11, AB = 52, AB_DEAD = 4, CD = 9, Absent = 35), newdat)
# newdat <- rbind(data.frame(month = 1, CBB_Inf = 15, AB = 53, AB_DEAD = 2, CD = 7, Absent = 38), newdat)
# #newdat[12,5] <- 
# # Spray Matrix
# sp_mat <- newdat
# library(cowplot)
# p1 <- ggplot(sp_mat, aes(month, AB_LIVE)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p2 <- ggplot(sp_mat, aes(month, AB_DEAD)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p3 <- ggplot(sp_mat, aes(month, CD)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p4 <- ggplot(sp_mat, aes(month, CBB_INF)) + geom_line() + scale_x_continuous(breaks = 1:12)
# plot_grid(p1, p2, p3, p4, ncol = 2)
# 
# # No Spray Matrix
# nsp_mat <- newdat
# 
# nsp_mat$AB_LIVE[1:6] <- nsp_mat$AB_LIVE[1:6]*1.50
# nsp_mat$AB_LIVE[7:12] <- nsp_mat$AB_LIVE[7:12]*1.35
# 
# nsp_mat$AB_DEAD[1:6] <- nsp_mat$AB_DEAD[1:6]*(.10)
# nsp_mat$AB_DEAD[7:12] <- nsp_mat$AB_DEAD[7:12]*(.1)
# 
# nsp_mat$CD[1:6] <- nsp_mat$CD[1:6]*1.20
# nsp_mat$CD[7:12] <- nsp_mat$CD[7:12]*1.35
# 
# nsp_mat$ABSENT <- 100 - (nsp_mat$AB_LIVE + nsp_mat$AB_DEAD + nsp_mat$CD)
# 
# p5 <- ggplot(nsp_mat, aes(month, AB_LIVE)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p6 <- ggplot(nsp_mat, aes(month, AB_DEAD)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p7 <- ggplot(nsp_mat, aes(month, CD)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p8 <- ggplot(nsp_mat, aes(month, ABSENT)) + geom_line() + scale_x_continuous(breaks = 1:12)
# plot_grid(p5, p6, p7, p8, ncol = 2)
# 
# # Combined plots
# nsp_mat$type <- "No Spray"
# sp_mat$type <- "Spray"
# plotdat <- rbind(nsp_mat, sp_mat)
# p9 <- ggplot(plotdat, aes(month, AB_LIVE, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p10 <- ggplot(plotdat, aes(month, AB_DEAD, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p11 <- ggplot(plotdat, aes(month, CD, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
# p12 <- ggplot(plotdat, aes(month, ABSENT, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
# plot_grid(p9, p10, p11, p12, ncol = 2)

#------------------------------------------------------
# Second Calibration
newdat <- data.frame(month = c(1:12),
                     CBB_INF = c(15,11,10,8,6.22,3.38,3.5,1.91,2.29,2.79,4.16,11),
                     AB_LIVE = c(53,52,50,47,40,33.68,16.87,13.12,10.20,12.97,13,20.5),
                     AB_DEAD = c(8.5,9,11,13,14.27,16.05,21,25,27.5,30.5,32,35),
                     CD = c(7,9,11,13,14.27,16,30.75,35.52,39.97,40.28,45,38),
                     ABSENT = c(38,35,33,35,27,32,33,24,17,23.8,14.5,6.7))
#newdat <- rbind(data.frame(month = 4, CBB_INF = 8, AB_LIVE = 47, AB_DEAD = 7, CD = 13, ABSENT = 35), newdat)
#newdat <- rbind(data.frame(month = 3, CBB_INF = 10, AB_LIVE = 50, AB_DEAD = 6, CD = 11, ABSENT = 33), newdat)
#newdat <- rbind(data.frame(month = 2, CBB_INF = 11, AB_LIVE = 52, AB_DEAD = 4, CD = 9, ABSENT = 35), newdat)
#newdat <- rbind(data.frame(month = 1, CBB_INF = 15, AB_LIVE = 53, AB_DEAD = 2, CD = 7, ABSENT = 38), newdat)

newdat$ABSENT <- 100 - (newdat$AB_LIVE + newdat$AB_DEAD + newdat$CD)
newdat
sp_mat <- newdat

# No spray matrix

nsp_mat <- data.frame(month = c(1:12),
                     CBB_INF = c(15, 11, 10, 8, 6.22, 3.38, 3.5, 1.91, 2.29, 2.79, 4.16, 11),
                     AB_LIVE = c(75.5, 72, 68, 65.5, 60, 54.52, 49.3, 41.68, 34.3, 30.455, 26.5, 40.5),
                     AB_DEAD = c(0.85, 0.9, 1.1, 1.3, 1.427, 1.605, 2.1, 2.5, 2.75, 3.05, 3.2, 3.5),
                     CD = c(15.5, 18.5, 22.5, 25.5, 29.405, 37, 44.5875, 50.204, 56.6565, 65.456, 70, 55.1))
                     


# nsp_mat <- newdat
# nsp_mat$AB_LIVE[1:6] <- nsp_mat$AB_LIVE[1:6]*1.50
# nsp_mat$AB_LIVE[7:12] <- nsp_mat$AB_LIVE[7:12]*1.50
# 
# nsp_mat$AB_DEAD[1:6] <- nsp_mat$AB_DEAD[1:6]*(.10)
# nsp_mat$AB_DEAD[7:12] <- nsp_mat$AB_DEAD[7:12]*(.10)
# 
# nsp_mat$CD[1:6] <- nsp_mat$CD[1:6]*1.5
# nsp_mat$CD[7:12] <- nsp_mat$CD[7:12]*1.45

nsp_mat$ABSENT <- 100 - (nsp_mat$AB_LIVE + nsp_mat$AB_DEAD + nsp_mat$CD)
nsp_mat

# Combined plots
nsp_mat$type <- "No Spray"
sp_mat$type <- "Spray"
plotdat <- rbind(nsp_mat, sp_mat)
p9 <- ggplot(plotdat, aes(month, AB_LIVE, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
p10 <- ggplot(plotdat, aes(month, AB_DEAD, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
p11 <- ggplot(plotdat, aes(month, CD, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
p12 <- ggplot(plotdat, aes(month, ABSENT, color = type)) + geom_line() + scale_x_continuous(breaks = 1:12)
plot_grid(p9, p10, p11, p12, ncol = 2)

sp_mat <- newdat
sp_mat <- select(sp_mat, AB_LIVE, AB_DEAD, CD, ABSENT)
names(sp_mat) <- c("A", "B", "C", "D")
sp_mat
sp_mat <- as.data.frame(apply(sp_mat, 2, function(x) round(x)))
sp_mat$D <- 100 - (sp_mat$A + sp_mat$B + sp_mat$C)
dat_tseq <- apply(t(sp_mat), 2, function(x) rep(row.names(t(sp_mat)), x))
sp_mcListFit <- markovcalibration(sp_mat)
sp_mcListFit

nsp_mat <- select(nsp_mat, AB_LIVE, AB_DEAD, CD, ABSENT)
nsp_mat
names(nsp_mat) <- c("A", "B", "C", "D")
nsp_mat <- as.data.frame(apply(nsp_mat, 2, function(x) round(x)))
nsp_mat$D <- 100 - (nsp_mat$A + nsp_mat$B + nsp_mat$C)

nsp_mcListFit <- markovcalibration(nsp_mat)

# Followed IPM
cv <- c(50, 10, 5, 35)

# Did not follow IPM
cv <- c(70, 1, 15, 14)

mat <- data.frame(A = rep(0, 11), B = rep(0, 11), C = rep(0, 11), D = rep(0, 11), spray = rep(0, 11))
mat[1,1:4] <- cv

# Decision based on sampling
for (i in 1:11){
  choice <- ifelse(cv[1] >= 25, 1, 0)
  cv <- choice*cv %*% sp_mcListFit$estimate[[i]][] + (1 - choice)*cv %*% nsp_mcListFit$estimate[[i]][]
  mat[i+1,1:4] <- cv
  mat$spray[i] <- choice
}
mat$inf <- ((mat$A + mat$C)/1500)*100
mat$chart <- mat$A/100*mat$inf
mat


# Followed IPM
cv <- c(50, 10, 5, 35)

# Did not follow IPM
cv <- c(80, 1, 10, 9)

mat <- data.frame(A = rep(0, 11), B = rep(0, 11), C = rep(0, 11), D = rep(0, 11), spray = rep(0, 11))
mat[1,1:4] <- cv

mat[2, 1:4] <- as.numeric(mat[1, 1:4]) %*% nsp_mcListFit$estimate[[2]][]
mat[3, 1:4] <- as.numeric(mat[2, 1:4]) %*% nsp_mcListFit$estimate[[3]][]


for (i in 3:11){
  choice <- ifelse(mat[i, 1] >= 25, 1, 0)
  cv <- choice*cv %*% sp_mcListFit$estimate[[i]][] + (1 - choice)*cv %*% nsp_mcListFit$estimate[[i]][]
  mat[i+1,1:4] <- cv
  mat$spray[i] <- choice
  if (i == 11){
    mat$spray[i+1] <- ifelse(cv[1] >= 25, 1, 0)
  }
}
mat$inf <- ((mat$A + mat$C)/1500)*100
mat$chart <- mat$A/100*mat$inf
mat

# Single farm calibration

