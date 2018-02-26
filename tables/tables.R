 library(stargazer)
 library(tables)
 library(dplyr)
 library(ggrepel)
 library(ggthemes)
 library(tidyverse)


 setwd("/run/media/john/1TB/SpiderOak/Projects/cbb-dynamic-model")

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
 
# Table 5 - Poorly Managed Results
tab5 <- select(pdp, Month, spray, harvest_cherry, harvest_damage, harvest_cost, spray_cost, price, nb, sum)
 
# Table 6 - Poorly-managed Infestation Levels
tab6 <- select(pdp, Month, spray, field_ablive, field_abdead, field_cd, inf)


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
stab3 <- stargazer(tab3, summary = FALSE, rownames = FALSE, title = "Well-managed Main Results")
stab4 <- stargazer(tab4, summary = FALSE, rownames = FALSE, title = "Well-managed Infestation Levels")
stab5 <- stargazer(tab5, summary = FALSE, rownames = FALSE, title = "Poorly-managed Main results")
stab6 <- stargazer(tab6, summary = FALSE, rownames = FALSE, title = "Poorly-managed Farm Infestation Levels")



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
# stab3
# 
loc <- which(stab3 == "\\end{tabular} ")
stab3 <- stab3[1:loc-1]
stab3notes <- paste("\\parbox{6.3in}{Notes: Table reports main results from the dynamic programming model for a well-managed farm. }")
stab3 <- c(stab3, "\\end{tabular}", centb, stab3notes, centc)
stab3 <- c(stab3, "\\end{table}")
# 
# # Table 4
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
stab4notes <- paste("\\parbox{4.3in}{Notes: Table reports field-level infestation results from the dynamic programming model for a well-managed farm. }")
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
stab5notes <- paste("\\parbox{7in}{Notes:Table reports main results from the dynamic programming model for a poorly-managed farm. }")
stab5 <- c(stab5, "\\end{tabular}", centb, stab5notes, centc)
stab5 <- c(stab5, "\\end{table}")


# Table 6
loc <- which(stab6 == "Month & spray & field\\_ablive & field\\_abdead & field\\_cd & inf \\\\ ")
loc
stab6l <- stab6[(loc+1):length(stab6)]
stab6 <- stab6[1:loc-1]

heading <- paste("Month & Spray & AB live & AB Dead & CD & Infested \\\\ ")
heading2 <- paste(" &  & (Field) & (Field) & (Field) & (Field) \\\\ ")
stab6 <- c(stab6, heading, heading2, stab6l)
stab6

loc <- which(stab6 == "\\end{tabular} ")
stab6 <- stab6[1:loc-1]
stab6notes <- paste("\\parbox{4in}{Notes: Table reports infestation level results from the dynamic programming model for a poorly-managed farm. }")
stab6 <- c(stab6, "\\end{tabular}", centb, stab6notes, centc)
stab6 <- c(stab6, "\\end{table}")


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
cat(stab5, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(stab6, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab7, file = "tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "tables.tex", append = TRUE)
# cat(stab8, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "tables.tex", append = TRUE)
# Compile pdf
system("pdflatex tables.tex")

}




# 
# 
# 
# tabular(tab1)
# 
# names(dp)writw
# 
# dp <- select(dp, Month, field_ablive, field_abdead, field_cd, inf, spray, harvest_c, price, model, nb)
# 
# head(results)
# results[,c(1:8, 10)] <- round(results[,c(1:8, 10)], 2)
# 
# latex(tabular((factor(Month))~Format(digits = 5)*Heading()*(Month + spray + field_ablive + Format(digits = 6)*nb)*Heading()*identity, data = dp))
# 
# latex(tabluar(factor(Month))~Format(digist = 5)*Heading()*(Month + spray)*identity, data = dp)
# 
# # Followed IPM Results
# #########################################
# # Latex output followed IPM
# \begin{table}[H]
# \centering
# \begin{tabular}{lcccccc}
# \toprule
#  & \multicolumn{2}{c}{Always Spray} & \multicolumn{2}{c}{Economic Model} & \multicolumn{2}{c}{IPM Choice} \\ \cmidrule(lr){2-3}\cmidrule(lr){4-5}\cmidrule(lr){6-7}
# Month  & Spray & Net-benefit & Spray & Net-benefit & Spray & \multicolumn{1}{c}{Net-benefit} \\ 
# \hline
# 3  & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{000}\phantom{-}0.00$ & $1$ & $\phantom{0}-280.00$ \\
# 4  & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{000}\phantom{-}0.00$ & $1$ & $\phantom{0}-280.00$ \\
# 5  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
# 6  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
# 7  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{0}-100.00$ \\
# 8  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
# 9  & $1$ & $\phantom{-}5698.47$ & $1$ & $\phantom{-}5691.36$ & $1$ & $\phantom{-}4737.74$ \\
# 10  & $1$ & $\phantom{-}7784.56$ & $1$ & $\phantom{-}7750.96$ & $1$ & $\phantom{-}7076.40$ \\
# 11  & $1$ & $\phantom{-}1680.14$ & $1$ & $\phantom{-}1675.00$ & $1$ & $\phantom{-}1272.80$ \\
# 12  & $1$ & $\phantom{-}1001.19$ & $0$ & $\phantom{-}1168.45$ & $1$ & $\phantom{0}\phantom{-}701.13$ \\
# \midrule
# Total Net-benefit  & $$ & $\phantom{-}\$15,084.36$ & $$ & $\phantom{-}\$15,565.77$ & $1$ & $\phantom{0}\phantom{-}\$12,288.07$ \\
# \bottomrule
# \end{tabular}
# \caption{My caption}
# \label{my-label}
# \end{table}
# 
# results
# 
# # Get final infestation levels
# res.plot <- results[,c(1:4, 9)]
# 
# names(res.plot) <- c("month", "AB Live", "AB Dead", "CD", "Model")
# 
# res.plot <- gather(res.plot, key, value, -month, -Model)
# res.plot
# res.plot$key <- factor(res.plot$key)
# p1 <- ggplot(res.plot, aes(month, value, color = Model)) + 
#   geom_line() + theme_tufte(base_size = 14) +
#   ggtitle("Final Field Infestation Level") + 
#   ylab("%") + xlab("Month") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = c(3:12)) + 
#   scale_y_continuous(breaks = c(5, 10, 15, 20)) +
#   geom_label_repel(data = filter(res.plot, month == 12), aes(label = value),
#                      na.rm = TRUE, show.legend = FALSE) +
#   theme(legend.position = "top", legend.title = element_blank()) + facet_wrap(~key)
# p1
# 
# 
# 
# # Did not follow IPM Results
# #############################
# # Latex table output
# \begin{center}
# \begin{tabular}{lcccccc}
# \hline
#  & \multicolumn{2}{c}{Always Spray} & \multicolumn{2}{c}{Economic Model} & \multicolumn{2}{c}{IPM Choice} \\ 
# factor(Month)  & spray & nb & spray & nb & spray & \multicolumn{1}{c}{nb} \\ 
# \hline
# 3  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
# 4  & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{000}\phantom{-}0.00$ & $1$ & $\phantom{0}-280.00$ \\
# 5  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
# 6  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
# 7  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
# 8  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{0}-100.00$ \\
# 9  & $1$ & $\phantom{-}4773.30$ & $1$ & $\phantom{-}5059.13$ & $0$ & $\phantom{-}3101.62$ \\
# 10  & $1$ & $\phantom{-}6378.51$ & $1$ & $\phantom{-}7421.07$ & $0$ & $\phantom{-}2382.74$ \\
# 11  & $1$ & $\phantom{-}1097.27$ & $1$ & $\phantom{-}1441.46$ & $0$ & $\phantom{0}-400.84$ \\
# 12  & $1$ & $\phantom{0}\phantom{-}622.46$ & $0$ & $\phantom{-}1024.83$ & $0$ & $\phantom{0}-463.27$ \\
# \midrule
# Total Net-benefit  & $$ & $\phantom{-}\$11,791.53$ & $$ & $\phantom{-}\$14,046.48$ & $1$ & $\phantom{0}\phantom{-}\$3,120.25$ \\
# \bottomrule
# \end{tabular}
# \end{center}
# 
# 
# # Get final infestation levels
# res.plot <- results[,c(1:4, 9)]
# 
# names(res.plot) <- c("month", "AB Live", "AB Dead", "CD", "Model")
# 
# res.plot <- gather(res.plot, key, value, -month, -Model)
# res.plot
# res.plot$key <- factor(res.plot$key)
# p2 <- ggplot(res.plot, aes(month, value, color = Model)) + 
#   geom_line() + theme_tufte(base_size = 14) +
#   #ggtitle("Final Field Infestation Level") + 
#   ylab("%") + xlab("Month") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = c(3:12)) + 
#   scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
#   geom_label_repel(data = filter(res.plot, month == 12), aes(label = value),
#                      na.rm = TRUE, show.legend = FALSE) +
#   theme(legend.position = "top", legend.title = element_blank()) + facet_wrap(~key)
# p2
# 
