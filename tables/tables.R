 library(stargazer)
 library(tables)
 library(dplyr)
 library(ggrepel)
 library(ggthemes)
 library(tidyverse)


 setwd("/run/media/john/1TB/SpiderOak/Projects/cbb-dynamic-model")

 source("1-parameters.R")
 
  # Well-managed results
 dp <- read_csv("results/well_managed_main_results.csv")
 dp$spray_cost <- ifelse(dp$spray == 1, dp$spray, 0)
 dp$harvest_p <- dp$harvest_p*100
 dp$spray <- ifelse(dp$spray == 0, "No Spray", "Spray")
 dp$sum <- cumsum(dp$nb)
 dp$field_ablive <- dp$field_ablive*100
 dp$field_abdead <- dp$field_abdead*100
 dp$field_cd <- dp$field_cd*100




 dp[, 3:ncol(dp)] <- round(dp[, 3:ncol(dp)], 2)

  # Poorly managed results
 pdp <- read_csv("results/poorly_managed_main_results.csv")
 pdp$spray_cost <- ifelse(pdp$spray == 1, pdp$spray, 0)
 pdp$harvest_p <- pdp$harvest_p*100
 pdp$spray <- ifelse(pdp$spray == 0, "No Spray", "Spray")
 pdp$sum <- cumsum(pdp$nb)
 pdp$field_ablive <- pdp$field_ablive*100
 pdp$field_abdead <- pdp$field_abdead*100
 pdp$field_cd <- pdp$field_cd*100

# IPM Choice Well-managed
ipm <- read.csv("results/ipmspray.csv", stringsAsFactors = FALSE)
ipm$spray_cost <- ifelse(ipm$spray == 1, ipm$spray, 0)
ipm$spray <- ifelse(ipm$spray == 0, "No Spray", "Spray")
ipm$sum <- cumsum(ipm$nb)
ipm$ab_live <- ipm$ab_live*100
ipm$ab_dead <- ipm$ab_dead*100
ipm$cd <- ipm$cd*100
ipm$inf <- ipm$inf*100
ipm$spray_cost <- ipm$spray_cost*-214
ipm$harvest_cost <- ipm$harvest_c*cost_h
ipm[, 6:ncol(ipm)] <- round(ipm[, 6:ncol(ipm)], 2)
ipm

# IPM Choice Well-managed
pipm <- read.csv("results/pm_ipmspray.csv", stringsAsFactors = FALSE)
pipm$spray_cost <- ifelse(pipm$spray == 1, pipm$spray, 0)
pipm$spray <- ifelse(pipm$spray == 0, "No Spray", "Spray")
pipm$sum <- cumsum(pipm$nb)
pipm$ab_live <- pipm$ab_live*100
pipm$ab_dead <- pipm$ab_dead*100
pipm$cd <- pipm$cd*100
pipm$inf <- pipm$inf*100
pipm$spray_cost <- pipm$spray_cost*-214
pipm$harvest_cost <- pipm$harvest_c*cost_h
pipm[, 6:ncol(pipm)] <- round(pipm[, 6:ncol(pipm)], 2)
pipm

# Always spray
aspray <- read.csv("results/alwaysspray.csv", stringsAsFactors = FALSE)
aspray$spray_cost <- ifelse(aspray$spray == 1, aspray$spray, 0)
aspray$spray <- ifelse(aspray$spray == 0, "No Spray", "Spray")
aspray$sum <- cumsum(aspray$nb)
aspray$ab_live <- aspray$ab_live*100
aspray$ab_dead <- aspray$ab_dead*100
aspray$cd <- aspray$cd*100
aspray$inf <- aspray$inf*100
aspray$spray_cost <- aspray$spray_cost*-214
aspray$harvest_cost <- aspray$harvest_c*cost_h
aspray[, 6:ncol(aspray)] <- round(aspray[, 6:ncol(aspray)], 2)
aspray





pdp[, 3:ncol(pdp)] <- round(pdp[, 3:ncol(pdp)], 2)

# Table 1 - Harvest Pricing per lbs of Cherry
tab1 <- data.frame(`Infestation(CD)` = c("0-5%", "6-10%", "11-15%", "16-20%", "21-30%", "31-40%", "41-58%"),
                    `Price(PerPound)` = c("1.80", "1.70", "1.60", "1.45", "1.20", "0.60", "0.59-0.35"))
 
# Table 2 - Well-managed Farm Parameters
tab2 <- data.frame(Parameter = c("Acres", "Projected Cherry", "Farm Labor", "Spray Labor", "Harvest Labor", "Pesticide",
                                 "Pesticide Costs",  "Water", "Water Cost", "Surfactant", "Surfactant Costs"),
                    Divisor = c("--", "Acres", "Hour", "Acre", "Lbs.", "Acre", "Quart", "Acre", "1k Gallon", "Acre", "Quart"),
                    Unit = c("Acres", "Lbs.", "Dollars", "Hours", "Dollars", "Quart", "Dollars", "Gallons", "Dollars", "Ounces", "Dollars"),
                    Estimate = c("2", "7,500", "15", "1", "0.5", "1", "70.35", "100", "1", "45", "8"))
  
# Table 3 - Well-managed Farms Results
tab3 <- select(dp, Month, spray, harvest_cherry, harvest_damage, harvest_cost, spray_cost, price, nb, sum)

# Table 4 - Well-managed Infestation Levels
tab4 <- select(dp, Month, spray, field_ablive, field_abdead, field_cd, inf)
tab4$inf <- tab4$inf*100 

# Table 5 - Poorly Managed Results
tab5 <- select(pdp, Month, spray, harvest_cherry, harvest_damage, harvest_cost, spray_cost, price, nb, sum)
 
# Table 6 - Poorly-managed Infestation Levels
tab6 <- select(pdp, Month, spray, field_ablive, field_abdead, field_cd, inf)
tab6$inf <- tab6$inf*100

# Table 7 - Well-managed Farm Results (IPM Choice)
tab7 <- select(ipm, Month, spray, ab_live, ab_dead, cd, inf)

# Table 8 - Well-Managed Results (IPM Choice)
tab8 <- select(ipm, Month, spray, harvest_c, cd_damage, harvest_cost, spray_cost, price, nb, sum)

# Table 9 - Poorly-managed Farm Results (IPM Choice)
tab9 <- select(pipm, Month, spray, ab_live, ab_dead, cd, inf)

# Table 10 - Poorly-Managed Results (IPM Choice)
tab10 <- select(pipm, Month, spray, harvest_c, cd_damage, harvest_cost, spray_cost, price, nb, sum)

# Table 11 - Well-managed Farms Results
tab11 <- select(aspray, Month, spray, harvest_c, cd_damage, harvest_cost, spray_cost, price, nb, sum)

# Table 12 - Always Spray Main Results 
tab12 <- select(aspray, Month, spray, ab_live, ab_dead, cd, inf)

# 
# tab1 <- data.frame(Position = c("AB Live", "AB Dead", "CD", "Not Infested"),
#                     Percentage = c("5.5%", "2.5%", "1%", "91%"))
# 
#   # Table 6 - Poorly-managed Farm Initial Infestation
#  tab6 <- data.frame(Position = c("AB Live", "AB Dead", "CD", "Not Infested"),
#                     Percentage = c("13.50%", "22.50%", "9%", "55%"))



  # Table 8 - Harvest Pricing
# stargazer(tab1, type = "text", summary = FALSE, rownames = FALSE, title = "Harvest Pricing per lbs of Cherry")
# stargazer(tab2, type = "text", summary = FALSE, rownames = FALSE, title = "Well-managed Farm Parameterts")
# stargazer(tab3, type = "text", summary = FALSE, rownames = FALSE, title = "Well-managed Main Results")
# stargazer(tab4, type = "text", summary = FALSE, rownames = FALSE, title = "Well-managed Infestation Levels")
# stargazer(tab5, type = "text", summary = FALSE, rownames = FALSE, title = "Well-managed Infestation Levels")
# stargazer(tab6, type = "text", summary = FALSE, rownames = FALSE, title = "Poorly-managed Farm Infestation Levels")
# 
# # stargazer(tab2, type = "text", summary = FALSE, rownames = FALSE, title = "Well-managed Farm Initial Infestation")
#   
#  
#  
#  # stargazer(tab6, type = "text", summary = FALSE, rownames = FALSE, title = "Poorly-managed Farm Initial Infestation")
 
stab1 <- stargazer(tab1, summary = FALSE, rownames = FALSE, title = "Harvest Pricing per lbs of Cherry")
stab2 <- stargazer(tab2, summary = FALSE, rownames = FALSE, title = "Typical Farm Parameters")
stab3 <- stargazer(tab3, summary = FALSE, rownames = FALSE, title =  "Economic Model Results")
stab4 <- stargazer(tab4, summary = FALSE, rownames = FALSE, title = "Economic Model Infestation Levels" )
stab5 <- stargazer(tab5, summary = FALSE, rownames = FALSE, title = "Poorly-managed Main results (Econ Model)")
stab6 <- stargazer(tab6, summary = FALSE, rownames = FALSE, title = "Poorly-managed Farm Infestation Levels (Econ Model)")
stab7 <- stargazer(tab7, summary = FALSE, rownames = FALSE, title = "IPM Choice Results", digits = 2)
stab8 <- stargazer(tab8, summary = FALSE, rownames = FALSE, title = "IPM Choice Infestation Levels", digits = 2)
stab9 <- stargazer(tab9, summary = FALSE, rownames = FALSE, title = "Poorly-managed Farm Results (IPM Choice)", digits = 2)
stab10 <- stargazer(tab10, summary = FALSE, rownames = FALSE, title = "Poorly-managed Infestation Levels (IPM Choice)", digits = 2)
stab11 <- stargazer(tab11, summary = FALSE, rownames = FALSE, title = "Always Spray Results", digits = 2)
stab12 <- stargazer(tab12, summary = FALSE, rownames = FALSE, title = "Always Spray Infestation Levels", digits = 2)

# stab1 <- stargazer(tab1, summary = FALSE, rownames = FALSE, title = "Well-managed Farm Initial Infestation")
# stab6 <- stargazer(tab6, summary = FALSE, rownames = FALSE, title = "Poorly-managed Farm Initial Infestation")

# 
# 
centb <- paste("\\begin{center}")
centc <- paste("\\end{center}")
# 
# # Table 2
loc <- which(stab2 == "\\end{tabular} ")
stab2 <- stab2[1:loc-1]
stab2notes <- paste("\\parbox{3.3in}{Notes: Table reports model parameters for a typical farm in Kona, Hawaii. Parameters are taken from field-level knowledge.}")
stab2 <- c(stab2, "\\end{tabular}", centb, stab2notes, centc)
stab2 <- c(stab2, "\\end{table}")

# Table 3
loc <- which(stab3 == "Month & spray & harvest\\_cherry & harvest\\_damage & harvest\\_cost & spray\\_cost & price & nb & sum \\\\ ")
loc
stab3l <- stab3[(loc+1):length(stab3)]
stab3 <- stab3[1:loc-1]

heading <- paste("Month & Spray & Harvested  & Harvested & Harvested & Spray Cost & Price & Net-benefit & Net-benefit \\\\ ")
heading2 <- paste(" &  &  Cherry & Damage & Cost & &  & & (Cum. Sum) \\\\ ")
stab3 <- c(stab3, heading, heading2, stab3l)

loc <- which(stab3 == "\\end{tabular} ")
stab3 <- stab3[1:loc-1]
stab3notes <- paste("\\parbox{6.3in}{Notes: Table reports main results from the dynamic programming model for a typical farm in Kona, Hawaii.}")
stab3 <- c(stab3, "\\end{tabular}", centb, stab3notes, centc)
stab3 <- c(stab3, "\\end{table}")

 
# Table 4
loc <- which(stab4 == "Month & spray & field\\_ablive & field\\_abdead & field\\_cd & inf \\\\ ")
loc
stab4l <- stab4[(loc+1):length(stab4)]
stab4 <- stab4[1:loc-1]

heading <- paste("Month & Spray & AB Live  & AB Dead & CD & Infested \\\\ ")
heading2 <- paste(" &  &  (Field) & (Field) & (Field) & (Field) \\\\ ")
stab4 <- c(stab4, heading, heading2, stab4l)
stab4

loc <- which(stab4 == "\\end{tabular} ")
stab4 <- stab4[1:loc-1]
stab4notes <- paste("\\parbox{4.3in}{Notes: Table reports field-level infestation results from the dynamic programming model}")
stab4 <- c(stab4, "\\end{tabular}", centb, stab4notes, centc)
stab4 <- c(stab4, "\\end{table}")
# 
# # Table 5
loc <- which(stab5 == "Month & spray & harvest\\_cherry & harvest\\_damage & harvest\\_cost & spray\\_cost & price & nb & sum \\\\ ")
loc
stab5l <- stab5[(loc+1):length(stab5)]
stab5 <- stab5[1:loc-1]

heading <- paste("Month & Spray & Harvested & Harvested & Harvested & Spray & Price & Net-benefit & Net-benefit \\\\ ")
heading2 <- paste(" &  & Cherry & Damage & Cost & Cost &  &  & (Cum. Sum) \\\\ ")
stab5 <- c(stab5, heading, heading2, stab5l)
stab5

loc <- which(stab5 == "\\end{tabular} ")
stab5 <- stab5[1:loc-1]
stab5notes <- paste("\\parbox{7in}{Notes: Table reports main results from the dynamic programming model for a poorly-managed farm. }")
stab5 <- c(stab5, "\\end{tabular}", centb, stab5notes, centc)
stab5 <- c(stab5, "\\end{table}")


# Table 6
loc <- which(stab6 == "Month & spray & field\\_ablive & field\\_abdead & field\\_cd & inf \\\\ ")
loc
stab6l <- stab6[(loc+1):length(stab6)]
stab6 <- stab6[1:loc-1]

heading <- paste("Month & Spray & AB Live & AB Dead & CD & Infested \\\\ ")
heading2 <- paste(" &  & (Field) & (Field) & (Field) & (Field) \\\\ ")
stab6 <- c(stab6, heading, heading2, stab6l)
stab6

loc <- which(stab6 == "\\end{tabular} ")
stab6 <- stab6[1:loc-1]
stab6notes <- paste("\\parbox{4in}{Notes: Table reports infestation level results from the dynamic programming model for a poorly-managed farm. }")
stab6 <- c(stab6, "\\end{tabular}", centb, stab6notes, centc)
stab6 <- c(stab6, "\\end{table}")

# Table 7
loc <- which(stab7 == "Month & spray & ab\\_live & ab\\_dead & cd & inf \\\\ ")
loc
stab7l <- stab7[(loc+1):length(stab7)]
stab7 <- stab7[1:loc-1]

heading <- paste("Month & Spray & AB Live & AB Dead & CD & Infested \\\\ ")
heading2 <- paste(" &  & (Field) & (Field) & (Field) & (Field) \\\\ ")
stab7 <- c(stab7, heading, heading2, stab7l)
stab7

loc <- which(stab7 == "\\end{tabular} ")
stab7 <- stab7[1:loc-1]
stab7notes <- paste("\\parbox{4in}{Notes: Table reports main results from the IPM Choice model for a well-managed farm. }")
stab7 <- c(stab7, "\\end{tabular}", centb, stab7notes, centc)
stab7 <- c(stab7, "\\end{table}")

# Table 8
loc <- which(stab8 == "Month & spray & harvest\\_c & cd\\_damage & harvest\\_cost & spray\\_cost & price & nb & sum \\\\ ")
loc
stab8l <- stab8[(loc+1):length(stab8)]
stab8 <- stab8[1:loc-1]

heading <- paste("Month & Spray & Harvested  & Harvested & Harvested & Spray Cost & Price & Net-benefit & Net-benefit \\\\ ")
heading2 <- paste(" &  &  Cherry & Damage & Cost & &  & & (Cum. Sum) \\\\ ")
stab8 <- c(stab8, heading, heading2, stab8l)

loc <- which(stab8 == "\\end{tabular} ")
stab8 <- stab8[1:loc-1]
stab8notes <- paste("\\parbox{6.3in}{Notes: Table reports main results from the dynamic programming model for a well-managed farm. }")
stab8 <- c(stab8, "\\end{tabular}", centb, stab8notes, centc)
stab8 <- c(stab8, "\\end{table}")

# Table 9
loc <- which(stab9 == "Month & spray & ab\\_live & ab\\_dead & cd & inf \\\\ ")
loc
stab9l <- stab9[(loc+1):length(stab9)]
stab9 <- stab9[1:loc-1]

heading <- paste("Month & Spray & AB Live & AB Dead & CD & Infested \\\\ ")
heading2 <- paste(" &  & (Field) & (Field) & (Field) & (Field) \\\\ ")
stab9 <- c(stab9, heading, heading2, stab9l)
stab9

loc <- which(stab9 == "\\end{tabular} ")
stab9 <- stab9[1:loc-1]
stab9notes <- paste("\\parbox{4in}{Notes: Table reports main results from the IPM Choice model for a well-managed farm. }")
stab9 <- c(stab9, "\\end{tabular}", centb, stab9notes, centc)
stab9 <- c(stab9, "\\end{table}")



# Table 10
loc <- which(stab10 == "Month & spray & harvest\\_c & cd\\_damage & harvest\\_cost & spray\\_cost & price & nb & sum \\\\ ")
loc
stab10l <- stab10[(loc+1):length(stab10)]
stab10 <- stab10[1:loc-1]

heading <- paste("Month & Spray & Harvested  & Harvested & Harvested & Spray Cost & Price & Net-benefit & Net-benefit \\\\ ")
heading2 <- paste(" &  &  Cherry & Damage & Cost & &  & & (Cum. Sum) \\\\ ")
stab10 <- c(stab10, heading, heading2, stab10l)

loc <- which(stab10 == "\\end{tabular} ")
stab10 <- stab10[1:loc-1]
stab10notes <- paste("\\parbox{6.3in}{Notes: Table reports main results from the dynamic programming model for a well-managed farm. }")
stab10 <- c(stab10, "\\end{tabular}", centb, stab10notes, centc)
stab10 <- c(stab10, "\\end{table}")


# Table 11
loc <- which(stab11 == "Month & spray & harvest\\_c & cd\\_damage & harvest\\_cost & spray\\_cost & price & nb & sum \\\\ ")
loc
stab11l <- stab11[(loc+1):length(stab11)]
stab11 <- stab11[1:loc-1]

heading <- paste("Month & Spray & Harvested  & Harvested & Harvested & Spray Cost & Price & Net-benefit & Net-benefit \\\\ ")
heading2 <- paste(" &  &  Cherry & Damage & Cost & &  & & (Cum. Sum) \\\\ ")
stab11 <- c(stab11, heading, heading2, stab11l)

loc <- which(stab11 == "\\end{tabular} ")
stab11 <- stab11[1:loc-1]
stab11notes <- paste("\\parbox{6.3in}{Notes: Table reports main results from the dynamic programming model for a well-managed farm. }")
stab11 <- c(stab11, "\\end{tabular}", centb, stab11notes, centc)
stab11 <- c(stab11, "\\end{table}")

# Table 12
loc <- which(stab12 == "Month & spray & ab\\_live & ab\\_dead & cd & inf \\\\ ")
loc
stab12l <- stab12[(loc+1):length(stab12)]
stab12 <- stab12[1:loc-1]

heading <- paste("Month & Spray & AB Live & AB Dead & CD & Infested \\\\ ")
heading2 <- paste(" &  & (Field) & (Field) & (Field) & (Field) \\\\ ")
stab12 <- c(stab12, heading, heading2, stab12l)
stab12


setwd("/run/media/john/1TB/SpiderOak/Projects/cbb-dynamic-model/tables")
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}\n", file = "tables.tex")
cat(stab1, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(stab2, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(stab3, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(stab4, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab5, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab6, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab7, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab8, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab9, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab10, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab11, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab12, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "tables.tex", append = TRUE)

# Compile pdf
system("pdflatex tables.tex")

}


setwd("/run/media/john/1TB/SpiderOak/Projects/cbb-dynamic-model")
