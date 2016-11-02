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
