 Sampling <- function(data, num.of.plots, expert, volunteer, num.experts, 
                     frequency.year, frequency.month, frequency.day,
                     outputall) {
  
  CheckingInputs (data, num.of.plots, expert, volunteer, 
                  num.experts, frequency.year, frequency.month, frequency.day)  
  
  currentdata <- data
  
  currentdata$expert.volunteer <- 0
  currentdata$costs <- 0
  
  chosenplots <- ChoosePlots (currentdata, num.of.plots)
  expert.plots <- ExpertPlots (chosenplots, num.of.plots, num.experts)
  volunteer.plots <- VolunteerPlots (chosenplots, expert.plots)
  
  year <- sort( unique (currentdata[, 6])) 
  year <- year[seq (1, length(year), frequency.year)]
  
  if (frequency.month > 0) {
    month <- sort (unique (currentdata[, 7])) 
    month <- month[seq (1, length(month), frequency.month)]
  } else {
    month <- 0
  }

  # (length(month)%/%frequency.month / 2) starting in the middle or 
  # at the beginning??
  
  # Need to be tested for a different amount of days per month or different 
  # dates
  
  if (frequency.day > 0) {
    day <- sort (unique (currentdata[, 8]))  # not sure - maybe using the total amount of days of a month
    day <- day[seq (1, length(day), frequency.day)]
  } else {
    day <- 0
  } 
      
    currentdata$expert.volunteer[currentdata[, 1] %in% expert.plots &
                                 currentdata[, 6] %in% year & currentdata[, 7] 
                                 %in% month & currentdata[, 8] 
                                 %in% day] <- "expert" 
    
    currentdata$costs[currentdata[, 1] %in% expert.plots & currentdata[, 6] 
                      %in% year & currentdata[, 7] %in% month & currentdata[, 8] 
                      %in% day] <- expert[5]
    
      currentdata$expert.volunteer[currentdata[, 1] %in% volunteer.plots & 
                                     currentdata[, 6] %in% year & 
                                     currentdata[, 7] %in% month & 
                                     currentdata[, 8] %in% day] <- "volunteer"
    
      currentdata$costs[currentdata[, 1] %in% volunteer.plots & currentdata[, 6] 
                        %in% year & currentdata[, 7] %in% month & 
                          currentdata[, 8] %in% day] <- volunteer[5]
  
   if (num.experts > 0) {
    
     currentdata <- SamplingEcologist (currentdata, expert.plots, expert[1],
                              expert[2], expert[3], expert[4]) 
   }
  
   if (num.experts != num.of.plots) {
    
     currentdata <- SamplingEcologist (currentdata, volunteer.plots, 
                                       volunteer[1], volunteer[2], volunteer[3], 
                                       volunteer[4])
   }
  
  if (outputall) {
    return (currentdata)
  } else {
    currentdata <- currentdata[currentdata$expert.volunteer != 0, ]
    return (currentdata)
  }
}

#' Error messages for the function \code{Sampling}
#' 
#' \code{CheckingInputs} tests the inputs of the function \code{Sampling} and 
#' returns an error message if necessary.
#' 
#' 

CheckingInputs <- function (data, num.of.plots, expert, volunteer, num.experts, 
                            frequency.year, frequency.month, frequency.day) {
  
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
  # checking expert/volunteer input with is.nummeric == T. For all or for each number 
}
