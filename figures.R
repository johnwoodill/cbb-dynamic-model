#' 
#' 
#' 
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)

#' Run model
source("3-dynamic_model.R")
#rm(list=setdiff(ls(), c("totalnb", "cherryonfarm")))

#' Get Markov matrices
source("2-calibrate_markov_chains.R")

# Get Markov calibrated matrices data formatting

# No spray
names(nsp_mat) <- c("ni", "ab_live", "ab_dead", "cd")
nsp_mat$month <- seq(1,13)  
nsp <- gather(nsp_mat, key = month, value = value)
names(nsp) <- c("month", "Infestation", "value")
nsp$Infestation <- factor(nsp$Infestation, levels = c("ni", "ab_live", "ab_dead", "cd"))
nsp

# Spray
names(sp_mat) <- c("ni", "ab_live", "ab_dead", "cd")
sp_mat$month <- seq(1,13)  
sp <- gather(sp_mat, key = month, value = value)
names(sp) <- c("month", "Infestation", "value")
sp$Infestation <- factor(sp$Infestation, levels = c("ni", "ab_live", "ab_dead", "cd"))
sp
   
# Get infestation levels and melt df
dat <- select(totalnb, month, ni, ab_live, ab_dead, cd)
dat <- gather(dat, key = month, value = value)
names(dat) <- c("month", "Infestation", "value")
dat$Infestation <- factor(dat$Infestation, levels = c("ni", "ab_live", "ab_dead", "cd"))
dat

# Get aggregated nb
nb_s <- c(rep(0,12))
for (k in 1:12){
  nb_s[k] <- sum(totalnb$nb[1:k])
}
nb_s

print(round(totalnb,2))  

nsp_p <- ggplot(nsp, aes(month, value, color = Infestation)) + geom_line() + ylab("Infestation Level (%)") + xlab("Month") + scale_x_continuous(breaks = 1:12) +
  ggtitle("No Spray Markov Calibration")

sp_p <- ggplot(sp, aes(month, value, color = Infestation)) + geom_line() + ylab("Infestation Level (%)") + xlab("Month") + scale_x_continuous(breaks = 1:12) +
  ggtitle("Spray Markov Calibration")

spnsp_p <- ggplot(totalnb, aes(month, spray)) + geom_point() + ylab("Choice") + xlab("Month") + scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = c(0,1), labels = c("No spray", "Spray"), limits = c(-1,2)) + ggtitle("Spray / No Spray Choice")

cherryonfarm_p <- qplot(1:12,cherryonfarm, geom = "line", xlab = "", ylab = "Cherry on farm") + scale_x_continuous(breaks = 1:12) +
  ggtitle("Logistic Cherry Growth") + scale_y_continuous(limits = c(0, acres*cherry_per_acre))
cherryonfarm_p

inf_p <- ggplot(dat, aes(month, value*100, color = Infestation)) + geom_line() + ylab("Infestation (%)") + xlab("Month") + scale_x_continuous(breaks = 1:12) +
  ggtitle("Dynamic Model Infestation Levels")

nb_p <- ggplot(totalnb, aes(month, nb_s)) + geom_line() + ylab("Cumulative Net Benefit ($)") + xlab("Month") + scale_x_continuous(breaks = 1:12) +
  ggtitle("Cumulative Net Benefit")

ggplot2::ggsave("figures/nsp_p.png", plot = nsp_p, device = "png")
ggplot2::ggsave("figures/sp_p.png", plot = sp_p, device = "png")
ggplot2::ggsave("figures/spnsp_p.png", plot = spnsp_p, device = "png")
ggplot2::ggsave("figures/cherryfarm_p.png", plot = cherryfarm_p, device = "png")
ggplot2::ggsave("figures/inf_p.png", plot = inf_p, device = "png")
ggplot2::ggsave("figures/nb_p.png", plot = nb_p, device = "png")
ggplot2::ggsave("figures/cherryonfarm.png", plot = cherryonfarm_p, device = "png")


# Dynamic Cherry pricing table
dcp <- data.frame(infestation_level = seq(0,1,.01))
dcp$price <- apply(dcp, 1, cherrypricing)
dcp <- dcp[!duplicated(dcp$price),]
saveRDS(dcp, "data/dynamiccherrypricing.rds")

#plot_grid(plot1, plot2, plot3, plot4, ncol = 2)

#########################################
# Spray matrices graphs
source("2-calibrate_markov_chains.R")
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

