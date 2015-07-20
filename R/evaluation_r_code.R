Evaluation <- function(data, num.of.plots, expert, volunteer, num.experts, 
                       frequency.year, frequency.month, frequency.day,
                       outputall, repetition) {
  
  totalresult <- 0
  totalresult <- totalresult [-1]
  
  for (j in 2:num.of.plots) {
    for (k in 1:repetition) {
        
      newdata <- Sampling (data, j, expert, volunteer, num.experts, 
            frequency.year, frequency.month, frequency.day,
            outputall)
      
      newdata$num.plots <- j
      totalresult <- rbind(totalresult, newdata)
      
    }

  }
return (totalresult)
}