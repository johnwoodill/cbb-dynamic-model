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
# sp_mat <- data.frame(A = c(rep(0, 12)),                                       # Not Infested
#                    B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15),      # A/B Live
#                    C = c(10, 15, 20, 23, 30, 34, 34, 37, 40, 41, 43, 44),      # A/B Dead
#                    D = c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14))             # C/D Position
#                    sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))      # Not Infested

#' @example markovchain(dat)
#' 
#' Returns list of n nonhomogenous markov chains

library(markovchain)

markovcalibration <- function(dat){
    
  # Build sequence from mat
  dat_tseq <- apply(t(dat), 2, function(x) rep(row.names(t(dat)), x))
  
  # Fit Markov Matrices to sequences
  dat_mcListFit <- markovchainListFit(data = dat_tseq[,1:ncol(dat_tseq)])
  
  return(dat_mcListFit)
}
