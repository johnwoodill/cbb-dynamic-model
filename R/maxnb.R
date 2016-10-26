#' @param p price of cherry
#' @param cost_s cost to spray
#' @param cost_h cost to harvest
#' @param h percentage available to harvest
#' @param cherry cherry available to harvest
#' @param sp_matrix spray transition matrix
#' @param nsp_matrix no spray transitionmatrix
#' @param cv current infestation level values (vector 1x4)

# Decision function for spraying or not spraying
# @param p price of cherry
# @param cost_s cost to spray
# @param sp_matrix spray transition matrix
# @param cv current infestation level values (vector 1x4)
# decision(p, cost, sp_matrix, cv)
source("R/decision.R")

# p = 2
# cost_s = 200
# cost_h = .5
# cherry = 7500
# h = .25
# sp_matrix = sp_mcListFit$estimate[[1]][]
# nsp_matrix = nsp_mcListFit$estimate[[1]][]
# cv = c(.90, .10, 0, .01)


maxnb <- function(p, cost_s, cost_h, h, cherry, sp_matrix, nsp_matrix, cv){
  # Get decision and infestation values
  choice <- decision(p, cherry, cost_s, sp_matrix, cv)
  
  # Get new current infestation values
  new_cv <- choice * (cv %*% sp_matrix) + (1 - choice) * (cv %*% nsp_matrix)
  
  # C/D damage
  d <- new_cv[4] - cv[4]
  
  # Determine optimal level of cherry harvest (%)
  harvest_check <- c(0, 0)     # c(nb, harvest %)
  for (i in unique(seq(0, h, .01))){
    check <- p * (i*cherry) * (1 - d) - cost_s*choice - cost_h*(i*cherry)   
    harvest_check[1] <- ifelse(check > harvest_check[1], check, harvest_check[1])
    harvest_check[2] <- ifelse(check > harvest_check[2], i, harvest_check[2])
  }
  
  h <- harvest_check[2]
  nb <- p * (h*cherry) * (1 - d) - cost_s*choice - cost_h*(h*cherry) 
  
  # Update Access outer scoper values
  cv <<- new_cv
  
  # Return maximized net benefit
  dat <- data.frame(spray = choice, cd = new_cv[4], damage = d*p, harvest = h, nb = nb)
  return(dat)
}

