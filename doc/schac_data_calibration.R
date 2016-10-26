#' Author  : A. John Woodill
#' Date    : 10/26/2016
#' Code    : shac_data_calibration.R
#' Descrip.: calibrate nonhomogenous markov chains for economic damage estimation for Kona from SHAC data set
#' 

library(tidyr)
library(ggplot2)
library(markovchain)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load data from SHAC
dat <- read_csv("data/DO_NOT_DISTRIBUTE-SHAC CBB 12 Trees Database 2015 Anon.csv")
names(dat) <- c("Acreage", "O_I", "Region", "FarmName", "LastSpray", "SampleDate", "OtherProducts", "CBBinf", "AB_Live", "AB_Dead", "CD", "Absent")

# Data setup
dat <- select(dat, Acreage, Region, SampleDate, CBBinf, AB_Live, AB_Dead, CD, Absent)
dat$Acreage <- as.numeric(dat$Acreage)
dat$AB_Live <- as.numeric(dat$AB_Live)
dat$CBBinf <- as.numeric(dat$CBBinf)
dat$AB_Dead <- as.numeric(dat$AB_Dead)
dat$CD <- as.numeric(dat$CD)
dat$Absent <- as.numeric(dat$Absent)

# Remove all with sample dates of NA
dat <- filter(dat, SampleDate != "NA")

# Date setup
dat$SampleDate <- mdy(dat$SampleDate)
dat$SampleDate <- as.Date(dat$SampleDate, "%m/%d/%Y")
dat$month <- month(dat$SampleDate)
dat$year <- year(dat$SampleDate)

# Subset only Kona region
kona <- filter(dat, Region == "Kona")
kona <- filter(kona, year == "2015")

# Average monthly data
kona2 <- kona %>% 
  group_by(month) %>% 
  summarise(avg_AB_Dead = mean(AB_Dead, na.rm = TRUE),
            avg_AB_Live = mean(AB_Live, na.rm = TRUE),
            avg_Absent = mean(Absent, na.rm = TRUE),
            avg_CD = mean(CD, na.rm = TRUE))

kona2 <- as.data.frame(kona2)
  
# Wide to long format
kona2 <- gather(kona2, key = month, value = value)

# Data setup
names(kona2) <- c("month", "position", "value")

# Plot
ggplot(kona2, aes(month, value, color = position)) + geom_smooth(se = FALSE)

# Get data frame
kona2 <- filter(kona2, month != 2)
kona2$month <- NULL
test <- spread(kona2, key = value, value = value)
as.matrix(kona2)

#              [,1]     [,2]     [,3]     [,4]      [,5]     [,6]     [,7]     [,8]
# avg_AB_Dead 20.4 23.40000 44.79245 34.88542 47.744186 22.97590 37.00000 29.46154
# avg_AB_Live 52.8 54.44000 27.70755 21.34375 15.383721 15.65060 21.33333 19.92308
# avg_Absent  11.8 15.78261 18.12500 21.22917 28.435294 49.06173 22.40000 42.63636
# avg_CD      15.0  5.04000  9.32381 21.19792  8.430233 12.18072 17.27778 12.69231


kona2 <- as.data.frame(kona2)
names(kona2) <- c("ni", "ab_live", "ab_dead", "cd")
kona2$month <- c(1:8)

newdat <- gather(kona2, stage, value, -month)
newdat$stage <- factor(newdat$stage, levels = c("ni", "ab_live", "ab_dead", "cd"))
ggplot(newdat, aes(x=month, y=value, color = stage)) + ggtitle("SHAC Calibration Data - Kona 2015") + geom_smooth(se = FALSE) + theme_bw()

trans.matrix <- function(X, prob=T)
{
    tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
    if(prob) tt <- tt / rowSums(tt)
    tt
}

# Always spraying
#----------------------------------------------------------------------------------------
# Establish projected levels of CBB from SHAC data
sp_mat <- data.frame(A = c(rep(0, 12)),                                                 # Not Infested
                  B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15),  # A/B Live
                  C = c(10, 15, 20, 23, 30, 34, 34, 37, 40, 41, 43, 44),  # A/B Dead
                  D = c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14))       # C/D Position

sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))                                     # Not Infested

# Build sequence from mat
sp_tseq <- apply(t(sp_mat), 2, function(x) rep(row.names(t(sp_mat)), x))

# Fit Markov Matrices to sequences
sp_mcListFit <- markovchainListFit(data = sp_tseq)


iv <- c(75, 20, 4, 1)
newdat <- data.frame()
newdat <- rbind(newdat, iv)
names(newdat) <- c("A", "B", "C", "D")

for (i in 1:9){
  mcfitt <- mcListFit$estimate[[i]][]
  iv <- iv %*% mcfitt
  newdat <- rbind(newdat, as.data.frame(iv))
}

newdat$month <- seq(1, nrow(newdat))
names(newdat) <- c("Inf", "AB_Live", "AB_Dead", "CD", "month")
newdat <- gather(newdat, key = month, value = value)
names(newdat) <- c("month", "position", "value")

newdat$position <- factor(newdat$position, levels = c("Inf", "AB_Live", "AB_Dead", "CD"))
ggplot(newdat, aes(month, value, color = position)) + geom_smooth(se = FALSE) + theme_bw() + ggtitle("Expected Infestation Levels - Always spray") + 
  scale_x_continuous(breaks = 1:12) 

# Never spraying
#----------------------------------------------------------------------------------------
# Establish projected levels of CBB from SHAC data
nsp_mat <- data.frame(A = c(rep(0, 12)),                                                 # Not Infested
                  B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15),  # A/B Live
                  C = c(5, 7, 10, 12, 15, 17, 18, 19, 20, 22, 23, 24),  # A/B Dead
                  D = c(1, 5, 8, 11, 15, 20, 25, 35, 40, 50, 55, 60))       # C/D Position

nsp_mat$A <- apply(nsp_mat, 1, function(x) 100 - sum(x))                                     # Not Infested

# Build sequence from mat
nsp_tseq <- apply(t(nsp_mat), 2, function(x) rep(row.names(t(nsp_mat)), x))

# Fit Markov Matrices to sequences
nsp_mcListFit <- markovchainListFit(data = nsp_tseq)


iv <- c(75, 20, 4, 1)
newdat <- data.frame()
newdat <- rbind(newdat, iv)
names(newdat) <- c("A", "B", "C", "D")

for (i in 1:11){
  mcfitt <- nsp_mcListFit$estimate[[i]][]
  iv <- iv %*% mcfitt
  newdat <- rbind(newdat, as.data.frame(iv))
}

newdat$month <- seq(1, nrow(newdat))
names(newdat) <- c("Inf", "AB_Live", "AB_Dead", "CD", "month")
newdat <- gather(newdat, key = month, value = value)
names(newdat) <- c("month", "position", "value")

newdat$position <- factor(newdat$position, levels = c("Inf", "AB_Live", "AB_Dead", "CD"))
ggplot(newdat, aes(month, value, color = position)) + geom_line() + theme_bw() + ggtitle("Expected Infestation Levels - Never spray") + 
  scale_x_continuous(breaks = 1:12) 
