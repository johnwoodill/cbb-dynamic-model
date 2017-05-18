# 1-parameters.R
# Backup parameters
# harvest <- c(0, 0, 0, 0, 0, 0, 0, 0, 0.32, 0.48, 0.12, 0.08)

# Model parameters
cost_s <- 180    # Cost to spray
cost_h <- .5     # Cost to harvest per pound of cherry
cv <- c(0.80, 0.08, 0.02, .1)   # Initial infestation
new_cv <- cv     # Set new infestation to initial infestation
#harvestschedule <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)    # Harvest schedule 0-No Harvest  1-Harvest
harvestschedule <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)    # Harvest schedule 0-No Harvest  1-Harvest
acres <- 2      # Farm acres
cherry_per_acre <- 7500     # Estimated cherry per acres
harvestedcherry <- 0        # Initial harvested cherry
decision_type <- "cost"              # TYpe of decision "cost" or "infestation"
# Total Net Benefit data.frame
totalnb <- data.frame()