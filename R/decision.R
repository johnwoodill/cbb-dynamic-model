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

  # cost_s = cost_s
  # cherry = cherryonfarm[i]*cv[4]
  # nsp_matrix = nsp_mcListFit$estimate[[i]][]
  # cv = cv

decision <- function(acres, cost_s, cherry, nsp_matrix, sp_matrix, cv){
  # Determine whether to spray/not spray
  # If damage > cost to spray then spray
  # If cost > damage then choose not to spray
  nspray <- cv %*% nsp_mcListFit$estimate[[i]][]
  # spray <- cv %*% sp_mcListFit$estimate[[i]][]
  
  
  nsp_damage <- nspray[3]*(cherryonfarm[i+1])*cherrypricing(nspray[3]) -
                cv[3]*(cherryonfarm[i])*cherrypricing(cv[3])

  
  # Get decision : 1 (spray), 0 (no spray)
  dec <- ifelse(nsp_damage >= cost_s*acres, 1, 0)
  return(dec)
}
