library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(cowplot)

setwd("/run/media/john/1TB/MEGA/Projects/cbb-dynamic-model/")
usda <- read_csv("data/KonaS1_unmanaged.csv")
usda <- read_csv("data/USDA_Kona_damage_S1.csv")
usda <- read_csv("data/Kona_Damage_S2_2017.csv")
#usda$date <- match(usda$date,month.abb)
#spray <- read_csv("data/S1_management.csv")

usda$date <- mdy(usda$date)
usda$date <- as.Date(usda$date, "%Y/%m/%d")
usda$month <- month(usda$date)

#usda[is.na(usda$AB_alive)] <- 1
#usda[is.na(usda$AB_)] <- 1
usda[is.na(usda)] <- 0

# Subset farms that sprayed throughout the year
usda <- filter(usda, site == "Kelleher" | site == "O'Neill")
#usda <- filter(usda, site == "Kelleher Farm" | site == "O'Neill Farm")
#sda <- filter(usda, site == "Kelleher")
#usda <- filter(usda, site == "O'Neill")
usda <- filter(usda, total_dissected != 0)

# Dissect Level
usda$d_ab_live <- usda$AB_alive/usda$total_dissected
usda$d_ab_dead <- (usda$AB_dead_other + usda$AB_dead_Bb + usda$AB_missing)/usda$total_dissected
usda$d_cd <- (usda$CD_alive + usda$CD_dead_Bb + usda$CD_dead_other + usda$CD_missing)/usda$total_dissected
usda$d_inf <- usda$total_infested_berries/usda$total_berry_count

# Field Level
# usda$percent_ab_live <- ((usda$total_infested_berries + usda$total_bb_berries)/usda$total_berry_count)*(usda$AB_alive/usda$total_dissected)*100
# usda$percent_ab_dead <- ((usda$total_infested_berries + usda$total_bb_berries)/usda$total_berry_count)*((usda$AB_dead_other + usda$AB_dead_Bb + usda$AB_missing)/usda$total_dissected)*100
# usda$percent_cd_live <- ((usda$total_infested_berries + usda$total_bb_berries)/usda$total_berry_count)*((usda$CD_alive + usda$CD_dead_Bb + usda$CD_dead_other + usda$CD_missing)/usda$total_dissected)*100
# usda$percent_inf <- ((usda$total_infested_berries + usda$total_bb_berries)/usda$total_berry_count)*100
# usda$percent_ni <- 100 - (usda$percent_ab_live + usda$percent_ab_dead + usda$percent_cd_live)


# ggplot(usda, aes(month, percent_ab_live)) + geom_smooth()
# ggplot(usda, aes(month, percent_ab_dead)) + geom_smooth()
# ggplot(usda, aes(month, percent_cd_live)) + geom_smooth()
# ggplot(usda, aes(month, percent_ni)) + geom_smooth()
# ggplot(usda, aes(month, percent_absent)) + geom_smooth()

# Dissect Level
newdat <- usda %>% 
  group_by(site, month) %>% 
  summarise(d_ab_live = mean(d_ab_live, na.rm = TRUE),
            d_ab_dead = mean(d_ab_dead, na.rm = TRUE),
            d_cd = mean(d_cd, na.rm = TRUE),
            d_inf = mean(d_inf, na.rm = TRUE),
            total_berry_count = mean(total_berry_count, na.rm = TRUE),
            total_infested_berries = mean(total_infested_berries, na.rm = TRUE)) %>% 
  group_by(month) %>% 
  summarise(d_ab_live = mean(d_ab_live, na.rm = TRUE),
            d_ab_dead = mean(d_ab_dead, na.rm = TRUE),
            d_cd = mean(d_cd, na.rm = TRUE),
            d_inf = mean(d_inf, na.rm = TRUE),
            total_berry_count = mean(total_berry_count, na.rm = TRUE),
            total_infested_berries = mean(total_infested_berries, na.rm = TRUE))

# Field Level
newdat <- usda %>% 
  group_by(site, month) %>% 
  summarise(percent_ab_live = mean(percent_ab_live, na.rm = TRUE),
            percent_ab_dead = mean(percent_ab_dead, na.rm = TRUE),
            percent_cd_live = mean(percent_cd_live, na.rm = TRUE),
            percent_inf = mean(percent_inf, na.rm = TRUE)) %>% 
  group_by(month) %>% 
  summarise(percent_ab_live = mean(percent_ab_live, na.rm = TRUE),
            percent_ab_dead = mean(percent_ab_dead, na.rm = TRUE),
            percent_cd_live = mean(percent_cd_live, na.rm = TRUE),
            percent_inf = mean(percent_inf, na.rm = TRUE))

newdat[is.na(newdat)] <- 0

minmax <- usda %>% 
  group_by(month)  %>% 
  summarise(percent_ab_live_high = max(percent_ab_live, na.rm = TRUE),
            percent_ab_live_low = min(percent_ab_live, na.rm = TRUE),
            percent_ab_dead_high = max(percent_ab_dead, na.rm = TRUE),
            percent_ab_dead_low = min(percent_ab_dead, na.rm = TRUE),
            percent_cd_live_high = max(percent_cd_live, na.rm = TRUE),
            percent_cd_live_low = min(percent_cd_live, na.rm = TRUE),
            percent_absent_high = max(percent_absent, na.rm = TRUE),
            percent_absent_low = min(percent_absent, na.rm = TRUE))

minmax[is.na(minmax)] <- 0
#newdat$ni <- 100 - (newdat$percent_ab_dead + newdat$percent_ab_live + newdat$percent_cd_live)

newdat <- left_join(newdat, minmax, by = "month")

g <- gather(newdat, key = position, value, -month)
g
ggplot(g, aes(month, value, color = position)) +geom_line() + ylab("Percent Infestation (Field Level)") + xlab("Month") + scale_x_continuous(breaks = 3:12)

p1 <- ggplot(newdat, aes(month, percent_ab_live)) + geom_line()+ ggtitle("AB LIVE") + scale_x_continuous(breaks = 3:12) + geom_line(aes(month, percent_ab_live_high), linetype = "dotted")+ geom_line(aes(month, percent_ab_live_low), linetype = "dotted")
p1
p2 <- ggplot(newdat, aes(month, percent_ab_dead)) + geom_line() + ggtitle("AB DEAD")+ scale_x_continuous(breaks = 3:12) + geom_line(aes(month, percent_ab_dead_high), linetype = "dotted")+ geom_line(aes(month, percent_ab_dead_low), linetype = "dotted")
p2
p3 <- ggplot(newdat, aes(month, percent_cd_live)) + geom_line() + ggtitle("CD")+ scale_x_continuous(breaks = 3:12)  + geom_line(aes(month, percent_cd_live_high), linetype = "dotted")+ geom_line(aes(month, percent_cd_live_low), linetype = "dotted")
p3
p4 <- ggplot(newdat, aes(month, percent_absent)) + geom_line() + ggtitle("ABSENT")+ scale_x_continuous(breaks = 3:12) + geom_line(aes(month, percent_absent_high), linetype = "dotted")+ geom_line(aes(month, percent_absent_low), linetype = "dotted")
p4
plot_grid(p1,p2,p3,p4, ncol = 2)



# Subset farms that sprayed throughout the year
dat <- filter(usda, site == "Kelleher" | site == "O'Neill")
ggplot(dat, aes(month, CD_alive, color = site)) + geom_point()
