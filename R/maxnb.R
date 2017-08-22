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

maxnb <- function(p, cost_s, cost_h, h, cherryforharvest, cherry_on_farm, harvestedcherry, cv, d, i){
  # Determine optimal level of cherry harvest (%)
  harvest_check <- c(0, 0)     # c(nb, harvest %)
  if(cv[3] < 0.25){
  for (j in unique(seq(0, (cherryforharvest), 1))){
    check <- p * (j * (1 - cv[3])) - cost_s*choice - cost_h*(j)   
    harvest_check[1] <- ifelse(check >= harvest_check[1], check, harvest_check[1])
    harvest_check[2] <- ifelse(check >= harvest_check[1], j, harvest_check[2])
  }}
  
  nb <- h * p * (harvest_check[2]) * (1 - cv[3]) - cost_s*choice - h * cost_h * (harvest_check[2]) 
  
  nsp_damage <- 0
  
  # Calculate no spray damage from decision.R
  if (i <= 9){
      nspray <- cv %*% nsp_mcListFit$estimate[[i]][]
      nspray_growth <- nspray[3] - cv[3]
      nsp_damage <- nspray_growth * cherry_on_farm * cherrypricing(nspray[3])
  }
  
  # Return maximized net benefit
  dat <- data.frame(Month = i+2, 
                    field_ablive = cv[1], 
                    field_abdead = cv[2], 
                    field_cd = cv[3], 
                    spray = choice, 
                    inf = cv[1] + cv[2] + cv[3],
                    ni = cv[4],
                    chart = NA,
                    price = p, 
                    nsp_damage = nsp_damage,
                    cd_damage = cv[3]*cherry_on_farm*p, 
                    cost = cost_s, 
                    harvest_s = h, 
                    harvest_c = h*harvest_check[2], 
                    harvest_p = round(h*harvest_check[2]/(acres*cherry_per_acre),2), 
                    nb = nb)
  return(dat)
}

