# implement repetiton on sampling ? to get first a tabel with the results and 
# than evaluate them

Evaluation <- function(data, num.of.plots, expert, volunteer, num.experts, 
                       frequency.year, frequency.month, frequency.day,
                       outputall, repetition, year.steps.simulation) {
  outputdata <- NULL
#  outputdata <- outputdata 
#   a <- 1
  
#   regression.truth <- lm(data$num.of.individuals ~ data$year)
#   
#   result.matrix <- matrix(0, nrow = ((num.of.plots / year.steps.simulation) * 
#                                        repetition + 1), ncol = 6) 
#   
#   result.matrix [1, ] <- c(0, coef(regression.truth)[1], 100, 
#                            coef(regression.truth)[2], 100, 0) # maybe NA
  
  for (j in seq(1, num.of.plots, year.steps.simulation)) { 
    
    experts <- round(j * num.experts / 100) # better idea for calculating the number of plots visited by an expert?
    
    for (k in 1:repetition) {
      
      
      
      newdata <- Sampling (data, j, expert, volunteer, experts, 
            frequency.year, frequency.month, frequency.day,
            outputall)
      
      newdata$num.plots <- j      
      newdata$repetition <- k

      
      outputdata <- rbind(outputdata, newdata) 
      
#       linear.regression <- lm(newdata$num.of.individuals ~ newdata$year)
#       coefficient <- coef(linear.regression)
#       
#       a <- a + 1
#       
#       result.matrix[a, ] <- c(j, coefficient[1], 
#                               (coefficient[1] * 100 / result.matrix[1, 2]), 
#                               coefficient[2], 
#                               (coefficient[2] * 100 / result.matrix[1, 4]),
#                               experts)
    }

  }
  
# result.matrix <- data.frame(result.matrix) 
# colnames (result.matrix) <- c("num.plots", "intercept", "intercept.in.percent", 
#                      "slope", "slope.in.percent", "experts")

return (outputdata)
}