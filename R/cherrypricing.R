#' Determines dynamic cherry pricing based on Greenwell farms
#' returns price based on level of infestation
#' 
#' @param x infestation rate
#' 
#' @example cherrypricing(0.15)
#' returns 1.75 

cherrypricing <- function(x){
  if (x >= 0 & x < 0.06) return(2)
  if (x >= 0.06 & x < 0.11) return(1.85)
  if (x >= 0.11 & x < 0.16) return(1.75)
  if (x >= 0.16 & x < 0.21) return(1.65)
  if (x >= 0.21 & x < 0.31) return(1.35)
  if (x >= 0.31 & x < 0.40) return(1.05)
  if (x == 0.41) return(0.89)
  if (x == 0.42) return(0.87)
  if (x == 0.43) return(0.86)
  if (x == 0.44) return(0.84)
  if (x == 0.45) return(0.83)
  if (x == 0.46) return(0.81)
  if (x == 0.47) return(0.80)
  if (x == 0.48) return(0.78)
  if (x == 0.49) return(0.77)
  if (x == 0.50) return(0.75)
  if (x == 0.51) return(0.74)
  if (x == 0.52) return(0.72)
  if (x == 0.53) return(0.71)
  if (x == 0.54) return(0.69)
  if (x == 0.55) return(0.68)
  if (x == 0.56) return(0.66)
  if (x == 0.57) return(0.65)
  if (x == 0.58) return(0.63)
  if (x == 0.59) return(0.62)
  if (x == 0.60) return(0.60)
  if (x > 0.60) return(0)
}

