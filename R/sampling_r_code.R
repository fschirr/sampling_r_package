Sampling <- function(data, num.of.plots, expert, volunteer, 
                     num.experts, frequency.year, frequency.month, frequency.day,
                     outputall){
  
  CheckingInputs (data, num.of.plots, expert, volunteer, 
                  num.experts, frequency.year, frequency.month, frequency.day)  

  data$expert.volunteer <- 0
  data$costs <- 0
  
  choosenplots <- ChoosePlots (data, num.of.plots)
  expert.plots <- ExpertPlots (choosenplots, num.of.plots, num.experts)
  volunteer.plots <- VolunteerPlots (choosenplots, expert.plots)
  
  year <- unique (data[, 6]) 
  year <- year[seq (1, length(year), frequency.year)]
  
  if (frequency.month > 0) {
    month <- unique (data[, 7]) 
    month <- month[seq (1, length(month), frequency.month)]
  } else {
    month <- 0
  }

  # (length(month)%/%frequency.month / 2) starting in the middle or at the beginning??
  
  # Need to be tested for a different amount of days per month or different dates
  
  if (frequency.day > 0) {
    day <- unique (data[, 8])  # not sure - maybe using the total amount of days of a month
    day <- day[seq (1, length(day), frequency.day)]
  } else {
    day <- 0
  } 
      
    data$expert.volunteer[data[, 1] %in% expert.plots & data[, 6] 
                          %in% year & data[, 7] %in% month & data[, 8] %in% day] <- "expert" 
    
    data$costs[data[, 1] %in% expert.plots & data[, 6] %in% year 
                & data[, 7] %in% month & data[, 8] %in% day] <- expert[5]
    
      data$expert.volunteer[data[, 1] %in% volunteer.plots & data[, 6] 
                %in% year & data[, 7] %in% month & data[, 8] %in% day] <- "volunteer"
    
      data$costs[data[, 1] %in% volunteer.plots & data[, 6] %in% year 
                & data[, 7] %in% month & data[, 8] %in% day] <- volunteer[5]
  
   if (num.experts > 0) {
    data <- SamplingEcologist (data, expert.plots, expert[1],
                              expert[2], expert[3], expert[4]) 
   }
  
   if (num.experts != num.of.plots) {
    data <- SamplingEcologist (data, volunteer.plots, volunteer[1],
                              volunteer[2], volunteer[3], volunteer[4])
   }
  
  if (outputall) {
      return (data)
  } else {
    data <- data[data$expert.volunteer != 0, ]
    return (data)
  }
}

CheckingInputs <- function (data, num.of.plots, expert, volunteer, 
                            num.experts, frequency.year, frequency.month, frequency.day) {
  
  if (num.of.plots < num.experts) { 
    stop ("num.of.plots need to be equal or more than num.experts")
  }
  if (frequency.year < 1) {
    stop ("frequency.year has to be 1 or higher")
  }
  if (frequency.month == 0 & data[1, 7] > 0) {
    stop ("frequency.month has to be 1 or higher")
  }
  if (frequency.day == 0 & data[1, 8] > 0) {
    stop ("frequency.day has to be 1 or higher")
  }
}
