# 1-parameters.R

# Model parameters
#p <- 2           # Price
cost_s <- 180    # Cost to spray
cost_h <- .5     # Cost to harvest per pound of cherry
cv <- c(.90, 0.08, 0.01, .01)
new_cv <- cv
#harvest <- c(0, 0, 0, 0, 0, 0, 0, 0, 0.32, 0.48, 0.12, 0.08)
harvestschedule <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
#cherry <- 7500
acres <- 2
cherry_per_acre <- 7500
harvestedcherry <- 0

# Total Net Benefit data.frame
totalnb <- data.frame()