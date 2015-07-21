Evaluation <- function(data, num.of.plots, expert, volunteer, num.experts, 
                       frequency.year, frequency.month, frequency.day,
                       outputall, repetition) {
  
  regall <- lm(data$num.of.individuals ~ data$year)
  regall <- coef(regall)
  
  totalresult <- matrix(c(0, regall[1], 100, regall[2], 100), nrow = 1, ncol = 5)
  
  for (j in 1:num.of.plots) {
    for (k in 1:repetition) {
      
      regmatrix <- matrix(0, nrow = 1, ncol = 5)  
      
      newdata <- Sampling (data, j, expert, volunteer, num.experts, 
            frequency.year, frequency.month, frequency.day,
            outputall)
      
      reg <- lm(newdata$num.of.individuals ~ newdata$year)
      reg <- coef(reg)
      
      regmatrix[1, 1] <- j
      regmatrix[1, 2] <- reg[1]
      regmatrix[1, 4] <- reg[2]

      regmatrix[1, 3] <- (reg[1] * 100 / totalresult[1, 2])
      regmatrix[1, 5] <- (reg[2] * 100 / totalresult[1, 4])
      
      totalresult <- rbind(totalresult, regmatrix)
      
    }

  }
totalresult <- data.frame(totalresult) 
colnames (totalresult) <- c("num.plots", "intercept", "intercept.in.percent", 
                     "slope", "slope.in.percent")
return (totalresult)
}