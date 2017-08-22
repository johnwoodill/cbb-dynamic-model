library(tables)
library(dplyr)
library(ggrepel)
library(ggthemes)

# Load results
as <- readRDS("results/alwaysspray.rds")
ipm <- readRDS("results/ipmspray.rds")
dp <- readRDS("results/dynamicmodel.rds")

names(as)
names(ipm)
names(dp)

as <- select(as, Month, field_ablive, field_abdead, field_cd, inf, spray, harvest_c, price, model, nb)
ipm <- select(ipm, Month, field_ablive, field_abdead, field_cd, inf, spray, harvest_c, price, model, nb)
dp <- select(dp, Month, field_ablive, field_abdead, field_cd, inf, spray, harvest_c, price, model, nb)

results <- rbind(as, ipm, dp)

head(results)
results[,c(1:8, 10)] <- round(results[,c(1:8, 10)], 2)

latex(tabular((factor(Month))~Format(digits = 5)* Heading()*factor(model)*(Format(digits = 1)*(spray + Format(digist = 5)*nb)*Heading()*identity), data = results, options = list(titlerule = "\\cmidrule(lr)")))

# Latex output
\begin{table}[H]
\centering
\begin{tabular}{lcccccc}
\toprule
 & \multicolumn{2}{c}{Always Spray} & \multicolumn{2}{c}{Economic Model} & \multicolumn{2}{c}{IPM Choice} \\ \cmidrule(lr){2-3}\cmidrule(lr){4-5}\cmidrule(lr){6-7}
Month  & Spray & Net-benefit & Spray & Net-benefit & Spray & \multicolumn{1}{c}{Net-benefit} \\ 
\hline
3  & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{000}\phantom{-}0.00$ & $1$ & $\phantom{0}-280.00$ \\
4  & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{000}\phantom{-}0.00$ & $1$ & $\phantom{0}-280.00$ \\
5  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
6  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
7  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $0$ & $\phantom{0}-100.00$ \\
8  & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-180.00$ & $1$ & $\phantom{0}-280.00$ \\
9  & $1$ & $\phantom{-}5698.47$ & $1$ & $\phantom{-}5691.36$ & $1$ & $\phantom{-}4737.74$ \\
10  & $1$ & $\phantom{-}7784.56$ & $1$ & $\phantom{-}7750.96$ & $1$ & $\phantom{-}7076.40$ \\
11  & $1$ & $\phantom{-}1680.14$ & $1$ & $\phantom{-}1675.00$ & $1$ & $\phantom{-}1272.80$ \\
12  & $1$ & $\phantom{-}1001.19$ & $0$ & $\phantom{-}1168.45$ & $1$ & $\phantom{0}\phantom{-}701.13$ \\
\midrule
Total Net-benefit  & $$ & $\phantom{-}\$15,084.36$ & $$ & $\phantom{-}\$15,565.77$ & $1$ & $\phantom{0}\phantom{-}\$12,288.07$ \\
\bottomrule
\end{tabular}
\caption{My caption}
\label{my-label}
\end{table}

results

# Get final infestation levels
res.plot <- results[,c(1:4, 9)]

names(res.plot) <- c("month", "AB Live", "AB Dead", "CD", "Model")

res.plot <- gather(res.plot, key, value, -month, -Model)
res.plot
res.plot$key <- factor(res.plot$key)
p1 <- ggplot(res.plot, aes(month, value, color = Model)) + 
  geom_line() + theme_tufte(base_size = 14) +
  ggtitle("Final Field Infestation Level") + 
  ylab("%") + xlab("Month") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = c(3:12)) + 
  scale_y_continuous(breaks = c(5, 10, 15, 20)) +
  geom_label_repel(data = filter(res.plot, month == 12), aes(label = value),
                     na.rm = TRUE, show.legend = FALSE) +
  theme(legend.position = "top", legend.title = element_blank()) + facet_wrap(~key)
p1
