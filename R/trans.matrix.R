#' Creates maximum likelihood estimation of tran.matrix
#' 
#' @param X sequence matrix
#' @param prob produces either the transition counts (prob = FALSE) 
#' or estimated transition probabilities (prob = TRUE)
#' 
#' @example 
#' 
#' # Build data.frame
#' mat <- data.frame(A = c(rep(0, 12)),                                      # Not Infested
#'                   B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15),  # A/B Live
#'                   C = c(10, 15, 20, 23, 30, 34, 34, 37, 40, 41, 43, 44),  # A/B Dead
#'                   D = c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14))         # C/D Position
#'
#' mat$A <- apply(mat, 1, function(x) 100 - sum(x))                          # Not Infested
#' 
#'  # Build sequence from mat
#'  tseq <- apply(t(mat), 2, function(x) rep(row.names(t(mat)), x))
#' 
#' # Build transition matrix
#' trans.matrix(mat, prob = T)
#'  
#'  # Output transition matrix
#'  
#'           A          B          C          D
#' A 0.93236715 0.06763285 0.00000000 0.00000000
#' B 0.02158273 0.80935252 0.16906475 0.00000000
#' C 0.00000000 0.00000000 0.96024465 0.03975535
#' D 0.00000000 0.00000000 0.00000000 1.00000000

trans.matrix <- function(X, prob=T)
{
    tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
    if(prob) tt <- tt / rowSums(tt)
    tt
}