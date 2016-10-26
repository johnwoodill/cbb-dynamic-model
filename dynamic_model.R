# Model parameters
p <- 2            # Price
cost_s <- 180    # Cost to spray
cost_h <- .5     # Cost to harvest per pound of cherry
cv <- c(.90, 0.08, 0.01, .01)
harvest <- c(0, 0, 0, 0, 0, 0, 0, 0, 0.32, 0.48, 0.12, 0.08)

# Load function for time-inhomogenous Markov chain estimation
source("R/markovcalibration.R")

# ------------------------------------------------------
# Spray calibration
sp_mat <- data.frame(A = c(rep(0, 13)),                                       # Not Infested
                  B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15, 14),  # A/B Live
                  C = c(10, 15, 20, 23, 30, 34, 34, 37, 40, 41, 43, 44, 43),  # A/B Dead
                  D = c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15))         # C/D Position

sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))                        # Not Infested

sp_mcListFit <- markovcalibration(sp_mat)

# No spray calibration
nsp_mat <- data.frame(A = c(rep(0, 13)),                                        # Not Infested
                  B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15, 14),    # A/B Live
                  C = c(5, 7, 10, 12, 15, 17, 18, 19, 20, 22, 23, 24, 25),      # A/B Dead
                  D = c(1, 5, 8, 11, 15, 20, 25, 35, 40, 50, 55, 60, 61))       # C/D Position

nsp_mat$A <- apply(nsp_mat, 1, function(x) 100 - sum(x))                        # Not Infested

nsp_mcListFit <- markovcalibration(nsp_mat)
# ------------------------------------------------------

# Load net benefit optimization function
source("R/maxnb.R")

totalnb <- data.frame()

for (i in 1:12){
      nb <- maxnb(p = 2, 
      cost_s = cost_s, 
      cost_h = cost_h, 
      cherry = 7500, 
      h = harvest[i], 
      sp_matrix = sp_mcListFit$estimate[[i]][], 
      nsp_matrix = nsp_mcListFit$estimate[[i]][], 
      cv = cv)
      totalnb <- rbind(totalnb, nb)
}
print(totalnb)  
