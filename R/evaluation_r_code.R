Evaluation <- function(data, num.of.plots, expert, volunteer, num.experts, 
                       frequency.year, frequency.month, frequency.day,
                       outputall, repetiton) {
  
  for (j in 1:num.of.plots) {
    for (k in 1:repetition) {
        
      Sampling (data, j, expert, volunteer, num.experts, 
            frequency.year, frequency.month, frequency.day,
            outputall)
      
    }
  }
}