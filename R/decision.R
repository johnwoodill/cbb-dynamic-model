#' Determines whether to spray or not spray
#' if return 1, then spray
#' if return 0, then don't spray
#' 
#' @param p price of cherry
#' @param cost_s cost to spray
#' @param sp_matrix spray transition matrix
#' @param cv current infestation level values (vector 1x4)
#' @param cherry cherry available
#' @param type "cost" or "infestation" decision (new IPM recommendations)
#' 
#' @example decision(100, 100, sp_matrix = dat_mcListFit$estimate[[1]][], cv = c(20,50,10,20))
#' returns 1
#' @example decision(0, 100, sp_matrix = dat_mcListFit$estimate[[1]][], cv = c(20,50,10,20), cherry = 7500)
#' returns 0
#' 

decision <- function(cost_s, cherry, nsp_matrix, cv, decision_type){
  if (decision_type == "cost"){
  # Determine whether to spray/not spray
  # If damage > cost to spray then spray
  # If cost > damage then choose not to spray
  spray <- cv %*% nsp_matrix
  spray_growth <- spray[4] - cv[4]
  sp_damage <- spray_growth * cherry * cherrypricing(spray[4])
  
  # Get decision : 1 (spray), 0 (no spray)
  dec <- ifelse(sp_damage >= cost_s, 1, 0)
  return(dec)}
  
  if (decision_type == "infestation"){
    perc_inf <- 1 - cv[1] 
    ablive_inf <- cv[2]
    if (perc_inf >= 0.01 & ablive_inf >= .80) return(1) 
    if (perc_inf >= 0.02 & ablive_inf >= .50) return(1) 
    if (perc_inf >= 0.03 & ablive_inf >= .35) return(1) 
    if (perc_inf >= 0.04 & ablive_inf >= .25) return(1) 
    if (perc_inf >= 0.05 & ablive_inf >= .20) return(1) 
    if (perc_inf >= 0.10 & ablive_inf >= .10) return(1) 
    if (perc_inf >= 0.15 & ablive_inf >= .05) return(1) 
    if (perc_inf >= 0.20 & ablive_inf >= .04) return(1) 
    if (perc_inf >= 0.25 & ablive_inf >= .04) return(1)
    if (perc_inf >= 0.30 & ablive_inf >= .04) return(1) 
    if (perc_inf >= 0.35 & ablive_inf >= .03) return(1) 
    if (perc_inf >= 0.40 & ablive_inf >= .03) return(1) 
    if (perc_inf >= 0.45 & ablive_inf >= .03) return(1) 
    if (perc_inf >= 0.50 & ablive_inf >= .02) return(1) else return(0)
    
  }
}
