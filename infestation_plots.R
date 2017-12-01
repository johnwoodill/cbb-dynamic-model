library(tidyverse)

pdat1 <- readRDS("results/well_managed_main_results.rds")
pdat2 <- readRDS("results/poorly_managed_main_results.rds")

pdat1$farm <- "Well Managed"
pdat2$farm <- "Poorly Managed"

pdat <- rbind(pdat1, pdat2)
pdat <- select(pdat, Month, field_ablive, field_abdead, field_cd, farm)
pdat <- gather(pdat, key = inf, value = value, -Month, -farm)
pdat$inf <- factor(pdat$inf, levels = c("field_ablive", "field_abdead", "field_cd"), 
                   labels = c("AB Live", "AB Dead", "CD"))
plott <- ggplot(pdat, aes(Month, 100*value, color = farm)) + geom_line() + facet_wrap(~inf) +
  scale_x_continuous(breaks = c(3:12)) + ylab("% Field-level Infestated")
  
john_ggplot(plott) + theme(legend.position = "top", legend.title = element_blank())

# Markov calibrations
source("2-calibrate_markov_chains.R")

spray <- dat3
nospray <- dat4
spray$type <- "Spray"
nospray$type  <- "No Spray"
spdat <- rbind(spray, nospray)
spdat$notinf <- 100 - rowSums(spdat[, c("A", "B", "C")])
spdat$inf <- rowSums(spdat[, c("A", "B", "C")])

names(spdat)[1:3] <- c("AB Live", "AB Dead", "CD")
spdat$month <- rep(c(3:12), 2)
spdat <- gather(spdat, key = inf, value = value, -type, -month)
spdat$inf <- factor(spdat$inf, levels = c("AB Dead", "AB Live", "CD", "inf", "notinf"), 
                    labels = c("AB Dead", "AB Live", "CD", "Infested", "Not Infested"))

splot <- ggplot(spdat, aes(month, value, color = type)) + geom_line() + facet_wrap(~inf) +
  ylab("% Field-level Infestation") +
  xlab("Month") +
  scale_x_continuous(breaks = c(3:12))

john_ggplot(splot) + theme(legend.position = "top", legend.title = element_blank())
