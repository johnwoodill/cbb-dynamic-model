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

