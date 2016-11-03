#' 
#' 
#' 
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)

#' Run model
source("3-dynamic_model.R")
rm(list=setdiff(ls(), c("totalnb", "cherryonfarm")))
   
# Get infestation levels and melt df
dat <- select(totalnb, month, ni, ab_live, ab_dead, cd)
dat <- gather(dat, key = month, value = value)
names(dat) <- c("month", "inf", "value")
dat

# Get aggregated nb
nb_s <- c(rep(0,12))
for (k in 1:12){
  nb_s[k] <- sum(totalnb$nb[1:k])
}
nb_s

print(round(totalnb,2))  

plot1 <- ggplot(totalnb, aes(month, spray)) + geom_point() + ylab("Spray / No Spray") + xlab(NULL) + scale_x_continuous(breaks = 1:12)
plot2 <- qplot(1:12,cherryonfarm, geom = "line", xlab = "", ylab = "Cherry on farm") + scale_x_continuous(breaks = 1:12)
plot3 <- ggplot(dat, aes(month, value, color = inf)) + geom_line() + ylab("Infestation") + xlab("Month") + scale_x_continuous(breaks = 1:12)
plot4 <- ggplot(totalnb, aes(month, nb_s)) + geom_line()

sum(totalnb$nb)


plot_grid(plot1, plot2, plot3, plot4, ncol = 2)
