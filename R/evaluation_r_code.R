Evaluation <- function(data, num.of.plots, expert, volunteer, num.experts, 
                       frequency.year, frequency.month, frequency.day,
                       outputall, repetition, year.steps.simulation) {
  outputdata <- NULL
  
  for (j in seq(1, num.of.plots, year.steps.simulation)) { 
    
    experts <- round(j * num.experts / 100) # better idea for calculating the 
                                            # number of plots visited by an expert?
    
    for (k in 1:repetition) {  
      
      newdata <- Sampling (data, j, expert, volunteer, experts, 
            frequency.year, frequency.month, frequency.day,
            outputall)
      
      newdata$number.experts <- experts
      newdata$num.plots <- j      
      newdata$repetition <- k
      
      outputdata <- rbind(outputdata, newdata) 
    }
  }

return (outputdata)
}

# a <- 1
# result.matrix <- matrix(0, nrow = 25, ncol = 6) 
# 
# for (i in seq(1, 100, 5)) {
# 
#   linear.regression <- lm(evaluationresult$num.of.individuals[evaluationresult$num.plots == i] ~ evaluationresult$year[evaluationresult$num.plots == i])
#   coefficient <- coef(linear.regression)
#                                              
#   result.matrix[a, ] <- c(j, coefficient[1], 0
#                          , 
#                           coefficient[2], 0
#                          ,
#                           0)
#   a <- a + 1
# 
# }
# result.matrix <- data.frame(result.matrix) 
# colnames (result.matrix) <- c("num.plots", "intercept", "intercept.in.percent", 
#                               "slope", "slope.in.percent", "experts")
# 
# aggregate(num.of.individuals ~ num.plots + year + repetition, data = evaluationresult, FUN = mean)
# 
# library(plyr)
# a <- ddply(evaluationresult, c("num.plots","repetition"), function(df)
#   c(coeffsinter(df$num.of.individuals, df$year),
#     coeffsslope(df$num.of.individuals, df$year)))
# 
# coeffsinter <- function(a, b) {
#   
#   linear.regression <- lm(a ~ b)
#   coefficient <- coef(linear.regression)
# 
#   return(coefficient[1])
#   
# }
# 
# coeffsslope <- function(a, b) {
#   
#   linear.regression <- lm(a ~ b)
#   coefficient <- coef(linear.regression)
# 
#   return(coefficient[2])
#   
# }
# 
# plot (a)
# 
# library(doBy)
# summaryBy(num.of.individuals ~ num.plots + repetition + year, data= evaluationresult, FUN=lm,na.rm=T)