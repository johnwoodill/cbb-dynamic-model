library(tidyverse)
library(ggthemes)
library(cowplot)

setwd("/run/media/john/1TB/SpiderOak/Projects/cbb-dynamic-model/")

# Cherry growth 
source("R/cherrygrowth.R")

# Initiate parameters
source("1-parameters.R")

# pdat1 <- readRDS("results/well_managed_main_results.rds")
# pdat2 <- readRDS("results/poorly_managed_main_results.rds")
# 
# pdat1$farm <- "Well Managed"
# pdat2$farm <- "Poorly Managed"
# 
# pdat <- rbind(pdat1, pdat2)
# pdat <- select(pdat, Month, field_ablive, field_abdead, field_cd, farm)
# pdat <- gather(pdat, key = inf, value = value, -Month, -farm)
# pdat$inf <- factor(pdat$inf, levels = c("field_ablive", "field_abdead", "field_cd"), 
#                    labels = c("AB Live", "AB Dead", "CD"))
# plott <- ggplot(pdat, aes(Month, 100*value, color = farm)) + geom_line() + facet_wrap(~inf) +
#   scale_x_continuous(breaks = c(3:12)) + ylab("% Field-level Infestated")
#   
# john_ggplot(plott) + theme(legend.position = "top", legend.title = element_blank())

# Jan - Dec
cherryonfarm <- cherrygrowth(-10:10, acres*cherry_per_acre, beta = 1, r = .3)
# ggplot(NULL, aes(cherryonfarm, x = 1:12)) + geom_line() + xlab("Month") +
#   scale_x_continuous(breaks = 1:12) +
#   ylab("lbs. of cherry") + theme_tufte(base_size = 14) + 
#     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   ggtitle("Cherry Growth on 2-acre Farm from January to December")


cherryonfarm <- cherryonfarm[3:12]

# Result infestation levels with cherry growth
pdat1 <- readRDS("results/well_managed_main_results.rds")
pdat2 <- readRDS("results/poorly_managed_main_results.rds")

pdat1$farm <- "Well Managed"
pdat2$farm <- "Poorly Managed"

pdat <- rbind(pdat1, pdat2)
pdat <- select(pdat, Month, field_ablive, field_abdead, field_cd, inf, farm)
pdat1 <- filter(pdat, farm == "Well Managed")
pdat2 <- filter(pdat, farm == "Poorly Managed")

p1 <- ggplot(NULL, aes(cherryonfarm, x = 3:12)) + 
  geom_line() +
  xlab("Month") +
  scale_x_continuous(breaks = 3:12) +
  geom_line(data = pdat1, aes(y = inf*50000, x = Month), color = "blue") +
  geom_line(data = pdat1, aes(y = field_ablive*50000, x = Month), color = "red") +
  geom_line(data = pdat1, aes(y = field_abdead*50000, x = Month), color = "green") +
  geom_line(data = pdat1, aes(y = field_cd*50000, x = Month), color = "orange")  +
  scale_y_continuous(sec.axis = sec_axis(~./500, name = "Field-level Infestation (%)")) +
  geom_vline(xintercept = 9, linetype = "dashed", color = "grey") +
  ylab("lbs. of mature cherry") + theme_tufte(base_size = 14) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("text", x = 9.5, y = 22500, label = "Start of Harvest", color = "grey" ) +
  annotate("text", x = 3.5, y = 22500, label = "Infestation", color = "blue" ) +
  annotate("text", x = 4.5, y = 22500, label = "AB Live", color = "red") +
  annotate("text", x = 5.5, y = 22500, label = "AB Dead", color = "green") +
  annotate("text", x = 6.5, y = 22500, label = "CD", color = "orange") +
  annotate("text", x = 11.5, y = 16000, label = "Mature Cherry", color = "black") +
  ggtitle("Well-managed Farm Infestation Levels") 
p1

p2 <- ggplot(NULL, aes(cherryonfarm, x = 3:12)) + 
  geom_line() +
  xlab("Month") +
  scale_x_continuous(breaks = 3:12) +
  geom_line(data = pdat2, aes(y = inf*50000, x = Month), color = "blue") +
  geom_line(data = pdat2, aes(y = field_ablive*50000, x = Month), color = "red") +
  geom_line(data = pdat2, aes(y = field_abdead*50000, x = Month), color = "green") +
  geom_line(data = pdat2, aes(y = field_cd*50000, x = Month), color = "orange")  +
  scale_y_continuous(sec.axis = sec_axis(~./500, name = "Field-level Infestation (%)")) +
  geom_vline(xintercept = 9, linetype = "dashed", color = "grey") +
  ylab("lbs. of mature cherry") + theme_tufte(base_size = 14) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("text", x = 9.5, y = 22500, label = "Start of Harvest", color = "grey" ) +
  annotate("text", x = 4, y = 22500, label = "Infestation", color = "blue" ) +
  annotate("text", x = 5, y = 22500, label = "AB Live", color = "red") +
  annotate("text", x = 6, y = 22500, label = "AB Dead", color = "green") +
  annotate("text", x = 7, y = 22500, label = "CD", color = "orange") +
  annotate("text", x = 11.5, y = 16000, label = "Mature Cherry", color = "black") +
  ggtitle("Poorly-managed Farm Infestation Levels") 
p2

plot_grid(p1, p2, ncol = 1)




##---
## Markov calibrations
source("2-calibrate_markov_chains.R")

spray <- dat3
nospray <- dat4
spray$type <- "Spray"
nospray$type  <- "No Spray"
spdat <- rbind(spray, nospray)

names(spdat)[1:3] <- c("AB Live", "AB Dead", "CD")
spdat$month <- rep(c(3:12), 2)
spdat <- gather(spdat, key = inf, value = value, -type, -month)
spdat$inf <- factor(spdat$inf, levels = c("AB Dead", "AB Live", "CD", "INF"), 
                    labels = c("AB Dead", "AB Live", "CD", "Infested"))

splot <- ggplot(spdat, aes(month, value, color = type)) + geom_line() + facet_wrap(~inf) +
  ylab("% Field-level Infestation") +
  xlab("Month") +
  scale_x_continuous(breaks = c(3:12))

splot
splot + theme(legend.position = "top", legend.title = element_blank())

