#' Determines dynamic cherry pricing based on Greenwell farms (8/26/2016)
#' returns price based on level of infestation
#' 
#' @param x infestation rate
#' 
#' @example cherrypricing(0.15)
#' returns 1.75 

cherrypricing <- function(x){
  x <- round(x, 2)
  if (x >= 0 & x < 0.06) return(1.80)
  if (x >= 0.06 & x < 0.11) return(1.70)
  if (x >= 0.11 & x < 0.16) return(1.60)
  if (x >= 0.16 & x < 0.21) return(1.45)
  if (x >= 0.21 & x < 0.31) return(1.20)
  if (x >= 0.31 & x < 0.41) return(0.60)
  if (x == 0.41) return(0.59)
  if (x == 0.42) return(0.57)
  if (x == 0.43) return(0.56)
  if (x == 0.44) return(0.54)
  if (x == 0.45) return(0.53)
  if (x == 0.46) return(0.51)
  if (x == 0.47) return(0.50)
  if (x == 0.48) return(0.48)
  if (x == 0.49) return(0.47)
  if (x == 0.50) return(0.45)
  if (x == 0.51) return(0.44)
  if (x == 0.52) return(0.42)
  if (x == 0.53) return(0.41)
  if (x == 0.54) return(0.39)
  if (x == 0.55) return(0.38)
  if (x == 0.56) return(0.36)
  if (x == 0.57) return(0.35)
  if (x == 0.58) return(0.33)
  if (x == 0.59) return(0.33)
  if (x == 0.60) return(0.30)
  if (x > 0.60) return(0)
}



#' Cherry growth based on logistical function
#' returns annual cherry growth 
#' 
#' Logistic growth model
#' @param t time
#' @param x size
#' @param alpha upper asymptote
#' @param beta growth range 
#' @param r growth rate 
#' @usage logistic(t, alpha, beta, k)
#' 
#' @example cherrygrowth(-12:12, acres*cherry_per_acre, beta = 1, r = .6)

cherrygrowth <- function(t, alpha, beta, r) {
    result <- alpha / (1 + beta * exp(-r * t))
    result <- result[c(1,3,5,7,9,11,13,15,17,19,21,23)]
  return(result)
}


#' Determines whether to spray or not spray
#' if return 1, then spray
#' if return 0, then don't spray
#' 
#' @param p price of cherry
#' @param cost_s cost to spray
#' @param sp_matrix spray transition matrix
#' @param cv current infestation level values (vector 1x4)
#' @param cherry cherry available
#' 
#' @example decision(100, 100, sp_matrix = dat_mcListFit$estimate[[1]][], cv = c(20,50,10,20))
#' returns 1
#' @example decision(0, 100, sp_matrix = dat_mcListFit$estimate[[1]][], cv = c(20,50,10,20), cherry = 7500)
#' returns 0
#' 

decision <- function(cost_s, cherry, nsp_matrix, cv){
  # Determine whether to spray/not spray
  # If damage > cost to spray then spray
  # If cost > damage then choose not to spray
  spray <- cv %*% nsp_matrix
  spray_growth <- spray[4] - cv[4]
  sp_damage <- spray_growth * cherry * cherrypricing(spray[4])
  
  # Get decision : 1 (spray), 0 (no spray)
  dec <- ifelse(sp_damage >= cost_s, 1, 0)
  return(dec)
}


#' Calibrates nonhomogenous markov chains from projected levels of CBB infestation
#' including not infested, A/B live, A/B dead, and C/D 
#' 
#' @param dat data.frame in following format where A = not infested, B = A/B live
#' C = A/B dead, and D = C/D
#' 
#'     A  B  C  D
#' 1  49 40 10  1
#' 2  46 37 15  2
#' 3  42 35 20  3
#' 4  42 30 23  5
#' 5  36 27 30  7
#' 6  37 21 34  8
#' 7  42 15 34  9
#' 8  37 16 37 10
#' 9  28 21 40 11
#' 10 28 19 41 12
#' 11 27 17 43 13
#' 12 27 15 44 14
#' 
#' Example data frame construction
#' # Establish projected levels of CBB from SHAC data
#' sp_mat <- data.frame(A = c(rep(0, 12)),                                       # Not Infested
#'                   B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15),      # A/B Live
#'                   C = c(10, 15, 20, 23, 30, 34, 34, 37, 40, 41, 43, 44),      # A/B Dead
#'                   D = c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14))             # C/D Position
#'                   sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))      # Not Infested

#' @example markovchain(dat)
#' 
#' Returns list of n nonhomogenous markov chains

library(markovchain)

markovcalibration <- function(dat){
    
  # Build sequence from mat
  dat_tseq <- apply(t(dat), 2, function(x) rep(row.names(t(dat)), x))
  
  # Fit Markov Matrices to sequences
  dat_mcListFit <- markovchainListFit(data = dat_tseq)
  
  return(dat_mcListFit)
}


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

