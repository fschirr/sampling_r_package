Evaluation <- function(data, num.of.plots, expert, volunteer, num.experts, 
                       frequency.year, frequency.month, frequency.day,
                       outputall, repetition) {
  
  a <- 1
  
  regression.truth <- lm(data$num.of.individuals ~ data$year)
  #regall <- coef(regression.truth)
  
  result.matrix <- matrix(0, nrow = (num.of.plots * repetition + 1), ncol = 6) 
  result.matrix [1, ] <- c(0, coef(regression.truth)[1], 100, 
                           coef(regression.truth)[2], 100, 0) # maybe NA
  
  for (j in 1:num.of.plots) {
    
    experts <- round(j * num.experts / 100) # better idea?
    
    for (k in 1:repetition) {
      
      
      
      newdata <- Sampling (data, j, expert, volunteer, experts, 
            frequency.year, frequency.month, frequency.day,
            outputall)
      
      linear.regression <- lm(newdata$num.of.individuals ~ newdata$year)
      coefficient <- coef(linear.regression)
      
      a <- a + 1
      
      result.matrix[a, ] <- c(j, coefficient[1], 
                              (coefficient[1] * 100 / result.matrix[1, 2]), 
                              coefficient[2], 
                              (coefficient[2] * 100 / result.matrix[1, 4]),
                              experts)
    }

  }
  
result.matrix <- data.frame(result.matrix) 
colnames (result.matrix) <- c("num.plots", "intercept", "intercept.in.percent", 
                     "slope", "slope.in.percent", "experts")

return (result.matrix)
}