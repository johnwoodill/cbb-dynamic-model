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
# @param cost_h cost to harvest per pound
# @param h harvest schedule (0 or 1)
# @param cherry cherry available on farm
# @param harvestedcherry total amount of cherry harvested
# @param cv current infestation level values (vector 1x4)
# @param d damage done in current period

 
#  p = 2
#  cost_s = 200
#  cost_h = .5
#  cherry = 7500
#  h = .25
#  sp_matrix = sp_mcListFit$estimate[[8]][]
#  nsp_matrix = nsp_mcListFit$estimate[[8]][]
#  cv = c(0.90, 0.08, 0.01, 0.01)
# d <- 0.01

maxnb <- function(p, cost_s, cost_h, h, cherry, harvestedcherry, cv, d){
  # Determine optimal level of cherry harvest (%)
  harvest_check <- c(0, 0)     # c(nb, harvest %)
  for (j in unique(seq(0, (cherry - harvestedcherry), 1))){
    check <- p * j * (1 - d) - cost_s*choice - cost_h*(j)   
    harvest_check[1] <- ifelse(check > harvest_check[1], check, harvest_check[1])
    harvest_check[2] <- ifelse(check > harvest_check[2], j, harvest_check[2])
  }
  
  nb <- h * p * (harvest_check[2]) * (1 - d) - cost_s*choice - h * cost_h * (harvest_check[2]) 
  
  # Return maximized net benefit
  dat <- data.frame(month = i, 
                    spray = choice, 
                    ni = cv[1], 
                    ab_live = cv[2], 
                    ab_dead = cv[3], 
                    cd = cv[4], 
                    price = p, 
                    damage = d*cherry*p, 
                    cost = cost_s, 
                    harvest_s = h, 
                    harvest_c = h*harvest_check[2], 
                    harvest_p = round(h*harvest_check[2]/(acres*cherry_per_acre),2), 
                    nb = nb)
  return(dat)
}

