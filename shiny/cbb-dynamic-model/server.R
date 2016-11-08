library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")



# Simulation and Shiny Application of Flue Season Dynamics
shinyServer(function(input, output) {
  
# Total Net Benefit data.frame
totalnb <- data.frame()

  # Hit counter
  # output$counter <- 
  #   renderText({
  #     if (!file.exists("counter.Rdata")) counter <- 0
  #     if (file.exists("counter.Rdata")) load(file="counter.Rdata")
  #     counter <- counter + 1
  #     
  #     save(counter, file="counter.Rdata")     
  #     paste0("Hits: ", counter)
  #   })
  
  mydata <- reactive({
    
  #source("shiny/cbb-dynamic-model/functions.R")
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

  
    # Model Parameters:
    req(input$acres)
    req(input$cherry_per_acre)
    req(input$cost_s)
    req(input$cost_h)
    req(input$harvestschedule)
    req(input$ab_live)
    req(input$ab_dead)
    req(input$cd)
    req(input$ab_live + input$ab_dead + input$cd <= 100)
    acres <- as.numeric(input$acres)
    decision_type <- ifelse(input$radio_decision == "cost", "cost", "infestation")
    cherry_per_acre <- as.numeric(input$cherry_per_acre)
    cost_s <- as.numeric(input$cost_s)
    cost_h <- as.numeric(input$cost_h)
    i_harvestschedule <- input$harvestschedule
    harvestschedule <- c(rep(0,12))
    for (i in 1:length(i_harvestschedule)){
      if (i_harvestschedule[i] == "jan") harvestschedule[1] <- 1
      if (i_harvestschedule[i] == "feb") harvestschedule[2] <- 1
      if (i_harvestschedule[i] == "mar") harvestschedule[3] <- 1
      if (i_harvestschedule[i] == "apr") harvestschedule[4] <- 1
      if (i_harvestschedule[i] == "may") harvestschedule[5] <- 1
      if (i_harvestschedule[i] == "jun") harvestschedule[6] <- 1
      if (i_harvestschedule[i] == "jul") harvestschedule[7] <- 1
      if (i_harvestschedule[i] == "aug") harvestschedule[8] <- 1
      if (i_harvestschedule[i] == "sep") harvestschedule[9] <- 1
      if (i_harvestschedule[i] == "oct") harvestschedule[10] <- 1
      if (i_harvestschedule[i] == "nov") harvestschedule[11] <- 1
      if (i_harvestschedule[i] == "dec") harvestschedule[12] <- 1
    }
    
    ni <- 1 - as.numeric(input$ab_live + input$ab_dead + input$cd)
    ab_live <- as.numeric(input$ab_live)
    ab_dead <- as.numeric(input$ab_dead)
    cd <- as.numeric(input$cd)
    cv <- c(ni/100, ab_live/100, ab_dead/100, cd/100)
    
    harvestedcherry <- 0
    
    # Total Net Benefit data.frame
    totalnb <- data.frame()
  
    #' 2-calibrate_markov_chains
    #' 
    #' 
    # Load function for time-inhomogenous Markov chain estimation
    #source("R/markovcalibration.R")
    
    # ------------------------------------------------------
    # Spray calibration
    sp_mat <- data.frame(A = c(rep(0, 13)),                                       # Not Infested
                      B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15, 14),  # A/B Live
                      C = c(10, 15, 20, 23, 30, 34, 34, 37, 40, 41, 43, 44, 43),  # A/B Dead
                      D = c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15))         # C/D Position
    
    sp_mat$A <- apply(sp_mat, 1, function(x) 100 - sum(x))                        # Not Infested
    
    sp_mcListFit <- markovcalibration(sp_mat)
    
    # No spray calibration
    nsp_mat <- data.frame(A = c(rep(0, 13)),                                        # Not Infested
                      B = c(40 ,37, 35 ,30, 27, 21, 15, 16, 21, 19, 17, 15, 14),    # A/B Live
                      C = c(5, 7, 10, 12, 15, 17, 18, 19, 20, 22, 23, 24, 25),      # A/B Dead
                      D = c(1, 5, 8, 11, 15, 20, 25, 35, 40, 50, 55, 60, 61))       # C/D Position
    
    nsp_mat$A <- apply(nsp_mat, 1, function(x) 100 - sum(x))                        # Not Infested
    
    nsp_mcListFit <- markovcalibration(nsp_mat)
    # ------------------------------------------------------
    
    cherryonfarm <- cherrygrowth(-12:12, acres*cherry_per_acre, beta = 1, r = .3)



  #---------------------------------------
  # Dynamic optimization problem
  
  for (i in 1:12){
    
    # Dynamic cherry pricing
    p <- cherrypricing(cv[4])
    
    # Calculate decision and infestation values
    choice <- decision(cost_s, cherryonfarm[i], nsp_mcListFit$estimate[[i]][], cv, decision_type = decision_type)
    
    # Get new current infestation values based on spray decision
    new_cv <- choice * (cv %*% sp_mcListFit$estimate[[i]][]) + (1 - choice) * (cv %*% nsp_mcListFit$estimate[[i]][])
    
    # C/D damage
    d <- new_cv[4] - cv[4]
    
    # Update dynamic cherry pricing
    p <- cherrypricing(new_cv[4])
    
    # Optimize Net Benefit (NB)
    nb <- maxnb(p = p, 
    cost_s = cost_s, 
    cost_h = cost_h, 
    cherry = cherryonfarm[i], 
    h = harvestschedule[i], 
    harvestedcherry =  harvestedcherry,
    cv = new_cv,
    d = d)
    
    # Build data frame with results
    totalnb <- rbind(totalnb, nb)
    
    # New infestation levels for next period
    cv <- new_cv
    harvestedcherry <- sum(totalnb$harvest_c)
  }
    totalnb
  
    })
  
  inf_data <- reactive({
    dat <- select(mydata(), month, ni, ab_live, ab_dead, cd)
    dat <- gather(dat, key = month, value = value)
    names(dat) <- c("month", "Infestation", "value")
    dat$Infestation <- factor(dat$Infestation, levels = c("ni", "ab_live", "ab_dead", "cd"))
    dat
  })
  
  totalnb <- reactive({
    tot <- mydata()
    round(sum(tot$nb),2)
  })
  
  output$totalnetben <- renderText({
    paste("Total Net Benefit: ", totalnb())})
          
          
  output$datatable <- renderTable(mydata())
  
     
  output$graph1 <- renderPlot({
     p <- ggplot(inf_data(), aes(month, value, color = Infestation)) + geom_line()    
     print(p)
   })
  # 
  # output$graph2 <- renderPlot({
  #   data2 <- mydata()[["wide"]]
  #       
  #   change <- data2[-1,]-data2[-nrow(data2),]
  #   
  #   long <- data.frame(
  #     Period=rep((1:nrow(change)),3), 
  #     Population = c(change[,1], change[,2], change[,3]), 
  #     Indicator=rep(c("Uninfected", "Infected", "Recovered"), 
  #                   each=nrow(change)))
  #   
  #   p <- ggplot(long, 
  #               aes(x=Period, y=Population, group=Indicator))    
  #   p <- p + geom_line(aes(colour = Indicator), size=1.5) + 
  #     ggtitle("Change in Population")
  #   print(p)
  # })
  
  
})
  