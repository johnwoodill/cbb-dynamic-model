library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(cowplot)
library(scales)
library(ggthemes)

#' Run model
source("3-dynamic_economic_model.R")
#rm(list=setdiff(ls(), c("totalnb", "cherryonfarm")))

#' Get Markov matrices
source("2-calibrate_markov_chains.R")

# Get Markov calibrated matrices data formatting


# Cherry growth 
source("R/cherrygrowth.R")

plot( acres*cherry_per_acre / (1 + beta * exp(-.5 * c(-10:10))))

# Jan - Dec
cherryonfarm <- cherrygrowth(-10:10, acres*cherry_per_acre, beta = 2, r = .5)

cherryonfarm <- cherryonfarm[3:12]
cherryonfarm <- cherryonfarm - lag(cumsum(totalnb$harvest_cherry), 1)


fieldcd <- dat3$C
fieldablive <- dat3$A
fieldabdead <- dat3$B
fieldinf <- c(dat3$A + dat3$B + dat3$C)

p1 <- ggplot(NULL, aes(cherryonfarm, x = 3:12)) + 
  geom_line() + 
  xlab("Month") +
  scale_x_continuous(breaks = 3:12) +
  geom_line(data = NULL, aes(y = fieldinf*200, x = 3:12), color = "blue") +
  geom_line(data = NULL, aes(y = fieldablive*200, x = 3:12), color = "red") +
  geom_line(data = NULL, aes(y = fieldabdead*200, x = 3:12), color = "green") +
  geom_line(data = NULL, aes(y = fieldcd*200, x = 3:12), color = "orange") +
  xlab(NULL) +
  ylab("lbs. of Available Cherry") + theme_tufte(base_size = 14) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Field-level Infestation (%)")) +
  geom_vline(xintercept = 9, linetype = "dashed", color = "grey") +
  annotate("text", x = 9.5, y = 17500, label = "Start of Harvest", color = "grey" ) +
  annotate("text", x = 3.5, y = 17500, label = "Infestation", color = "blue" ) +
  annotate("text", x = 4.5, y = 17500, label = "AB Live", color = "red") +
  annotate("text", x = 5.5, y = 17500, label = "AB Dead", color = "green") +
  annotate("text", x = 6.5, y = 17500, label = "CD", color = "orange") +
  annotate("text", x = 11.5, y = 16000, label = "Available Cherry", color = "black") +
  ggtitle("Always Spray Farm Infestation Levels") 
p1

nfieldcd <- dat4$C
nfieldablive <- dat4$A
nfieldabdead <- dat4$B
nfieldinf <- c(dat4$A + dat4$B + dat4$C)

p2 <- ggplot(NULL, aes(cherryonfarm, x = 3:12)) + 
  geom_line() + 
  xlab("Month") +
  scale_x_continuous(breaks = 3:12) +
  geom_line(data = NULL, aes(y = nfieldinf*200, x = 3:12), color = "blue") +
  geom_line(data = NULL, aes(y = nfieldablive*200, x = 3:12), color = "red") +
  geom_line(data = NULL, aes(y = nfieldabdead*200, x = 3:12), color = "green") +
  geom_line(data = NULL, aes(y = nfieldcd*200, x = 3:12), color = "orange") +
  ylab("lbs. of Available Cherry") + theme_tufte(base_size = 14) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_y_continuous(sec.axis = sec_axis(~./200, name = "Field-level Infestation (%)")) +
  geom_vline(xintercept = 9, linetype = "dashed", color = "grey") +
  # annotate("text", x = 3.5, y = 17500, label = "Infestation", color = "blue") +
  # annotate("text", x = 4.5, y = 17500, label = "AB Live", color = "red") +
  # annotate("text", x = 5.5, y = 17500, label = "AB Dead", color = "green") +
  # annotate("text", x = 6.5, y = 17500, label = "CD", color = "orange") +
  annotate("text", x = 11, y = 15500, label = "Available Cherry", color = "black") +
  ggtitle("Never Spray Farm Infestation Levels")

plot_grid(p1, p2, ncol = 1)
ggsave("figures/calibrated_infestation_data.pdf", width = 6, height = 6)

# Results from model









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
library(ggrepel)
source("2-calibrate_markov_chains.R")
# Plot
newdat1 <- dat1[, 1:4]
names(newdat1) <- c("month", "AB Live", "AB Dead", "CD")
newdat1 <- gather(newdat1, key, value, -month)
newdat1$key <- factor(newdat1$key)
p1 <- ggplot(newdat1, aes(month, value, color = key)) + 
  geom_line() + 
  ggtitle("Spray Calibration Data") + 
  ylab("% Dissected Berries") + 
  scale_x_continuous(breaks = c(3:12)) + 
    geom_label_repel(data = filter(newdat1, month == 10), aes(label = key),
                     na.rm = TRUE, show.legend = FALSE, check_overlap = TRUE) +
  theme(legend.position = "none")
p1

directnewdat2 <- dat2[, 1:4]
names(newdat2) <- c("month", "AB Live", "AB Dead", "CD")
newdat2 <- gather(newdat2, key, value, -month)
newdat2$key <- factor(newdat2$key)
p2 <- ggplot(newdat2, aes(month, value, color = key)) + 
  geom_line() + 
  ggtitle("No Spray Calibration Data") + 
  ylab("% Dissected Berries")+ 
  scale_x_continuous(breaks = c(3:12)) +
      geom_label_repel(data = filter(newdat2, month == 10), aes(label = key),
                     na.rm = TRUE) +
  theme(legend.position = "none")
p2

plot_grid(p1, p2, ncol = 1)

newdat3 <- dat1[, c(1,5)]
p3 <- ggplot(newdat3, aes(month, INF)) + geom_line() + ggtitle("Spray \n Field Level Infestation") + ylab("% Berries in Field") + scale_x_continuous(breaks = c(3:12))
p3

newdat4 <- dat2[, c(1,5)]
p4 <- ggplot(newdat4, aes(month, INF)) + geom_line() + ggtitle("No Spray - Field Level Infestation") + ylab("% Berries in Field") + scale_x_continuous(breaks = c(3:12))
p4

plot_grid(p1, p2, ncol = 1)
plot_grid(p3, p4, ncol = 1)

# Bar charts of results
dat1 <- read_csv("results/well_managed_main_results.csv")
dat2 <- read_csv("results/poorly_managed_main_results.csv")
dat3 <- read_csv("results/ipmspray.csv")
dat4 <- read_csv("results/pm_ipmspray.csv")
dat5 <- read_csv("results/alwaysspray.csv")
dat6 <- read_csv("results/neverspray.csv")
dat7 <- read_csv("results/pm_alwaysspray.csv")
dat8 <- read_csv("results/pm_neverspray.csv")

dat <- data.frame(model = rep(c("Economic Model", "IPM Choice", "Always Spray", "Never Spray"), 1,  each = 4),
                  variable = rep(c("Final Net-benefit ($)", "Marketable Cherry (Lbs.)", "Final CD (%)", "CD Damage (Lbs.)"), 4),
                  value = c(sum(dat1$nb), sum(dat1$harvest_cherry - dat1$harvest_damage), dat1$field_cd[10]*100, sum(dat1$harvest_damage),
                            sum(dat3$nb), sum(dat3$harvest_c - dat3$cd_damage), dat3$cd[10]*100, sum(dat3$cd_damage),
                            sum(dat5$nb), sum(dat5$harvest_c - dat5$cd_damage), dat5$cd[10]*100, sum(dat5$cd_damage),
                            sum(dat6$nb), sum(dat6$harvest_c - dat6$cd_damage), dat6$cd[10]*100, sum(dat6$cd_damage)))

dat$model <- factor(dat$model, levels = c("Never Spray", "Always Spray",  "IPM Choice", "Economic Model"))
dat$variable <- factor(dat$variable, levels = c("Final CD (%)", "Marketable Cherry (Lbs.)", "CD Damage (Lbs.)", "Final Net-benefit ($)"))

ggplot(dat, aes(x = factor(model), y = value)) + 
  geom_bar(stat = "identity", width = .75) + 
  theme_tufte(base_size = 10) +
  geom_text(aes(label = comma(round(value, 2))), vjust=1.5, size = 2, color = "white") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  facet_wrap(~variable, scales = "free") +
  ylab(NULL) +
  xlab(NULL) +
  theme(strip.background = element_blank(),
           strip.placement = "outside") +
  scale_x_discrete(breaks = unique(dat$model),
                   labels = c("Economic \n Model", "IPM \n Choice", "Always \n Spray", "Never \n Spray")) +
  scale_y_continuous(labels = comma)
ggsave("figures/final_plot_well_managed.pdf", width = 6, height = 4)


dat <- data.frame(model = rep(c("Economic Model", "IPM Choice", "Always Spray", "Never Spray"), 1,  each = 4),
                  variable = rep(c("Final Net-benefit ($)", "Marketable Cherry (Lbs.)", "Final CD (%)", "CD Damage (Lbs.)"), 4),
                  value = c(sum(dat2$nb), sum(dat2$harvest_cherry - dat2$harvest_damage), dat2$field_cd[10]*100, sum(dat2$harvest_damage),
                            sum(dat4$nb), sum(dat4$harvest_c - dat4$cd_damage), dat4$cd[10]*100, sum(dat4$cd_damage),
                            sum(dat7$nb), sum(dat7$harvest_c - dat7$cd_damage), dat7$cd[10]*100, sum(dat7$cd_damage),
                            sum(dat8$nb), sum(dat8$harvest_c - dat8$cd_damage), dat8$cd[10]*100, sum(dat8$cd_damage)))

dat$model <- factor(dat$model, levels = c("Never Spray", "Always Spray",  "IPM Choice", "Economic Model"))
dat$variable <- factor(dat$variable, levels = c("Final CD (%)", "Marketable Cherry (Lbs.)", "CD Damage (Lbs.)", "Final Net-benefit ($)"))

ggplot(dat, aes(x = factor(model), y = value)) + 
  geom_bar(stat = "identity", width = .75) + 
  theme_tufte(base_size = 10) +
  geom_text(aes(label = comma(round(value, 2))), vjust=1.5, size = 2, color = "white") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  facet_wrap(~variable, scales = "free") +
  ylab(NULL) +
  xlab(NULL) +
  theme(strip.background = element_blank(),
           strip.placement = "outside") +
  scale_x_discrete(breaks = unique(dat$model),
                   labels = c("Economic \n Model", "IPM \n Choice", "Always \n Spray", "Never \n Spray")) +
  scale_y_continuous(labels = comma)
ggsave("figures/final_plot_poorly_managed.pdf", width = 6, height = 4)


