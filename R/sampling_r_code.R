#' Package for monitoring optimization
#' 
#' Reweitting the dataset to make it useable for the Sampling function of this package. 
#' The function comibines the different columns and creates a new dataset. If there are now data fro a column the 
#' Column will be filled with zeros.
#'
#' 
#' @param data The input dataset
#' @param plot number of the column with the plot number
#' @param num.of.individuals 
#' @param species
#' @param x.coord
#' @param y.coord
#' @param visit.year
#' @export
#'   

PrepareDataset <- function(data, plot, num.of.individuals, 
                           species, x.coord, y.coord, visit.year, visit.month, visit.day) {
  
  column.plot <- ifelse (plot > 0, data[plot], rep(0, length(data[, 1])))
  column.num.of.individuals <- ifelse (num.of.individuals > 0, data[num.of.individuals],
                                      rep(0, length(data[, 1])))
  column.species <- ifelse (species > 0, data[species], rep(0, length(data[, 1])))
  column.x.coord <- ifelse (x.coord > 0, data[x.coord], rep(0, length(data[, 1])))
  column.y.coord <- ifelse (y.coord > 0, data[y.coord], rep(0, length(data[, 1])))
  column.visit.year <- ifelse (visit.year > 0, data[visit.year], rep(0, length(data[, 1])))
  column.visit.month <- ifelse (visit.month > 0,data[visit.month], rep(0, length(data[, 1])))
  column.visit.day <- ifelse (visit.day > 0, data[visit.day], rep(0, length(data[, 1])))
  
  
  data <- data.frame (column.plot, column.num.of.individuals, column.species, # other name than data
              column.x.coord, column.y.coord, column.visit.year, 
              column.visit.month, column.visit.day)
  
  colnames (data) <- c("plot", "num.of.individuals", "species", 
                      "x.coord", "y.coord", "year", "month", "day")
  
  return (data)
}

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

  #(length(month)%/%frequency.month / 2) starting in the middle or at the beginning??
  
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

#'ChoosePlots
#'

ChoosePlots <- function (data, num.of.plots) {
  
  choosenplots <- sort (sample (unique (data[, 1]), num.of.plots, replace = F))
  
  return (choosenplots)
}

ExpertPlots <- function (choosenplots, num.of.plots, num.experts) {
  
  if (num.of.plots == 1 & num.experts == 1) {
    expert.plots <- choosenplots
  } else {
    expert.plots <- sample (choosenplots, num.experts)
  }
  
  return (expert.plots)
}

VolunteerPlots <- function (choosenplots, expert.plots) {
  
  volunteer.plots <- choosenplots[!(choosenplots %in% expert.plots)]
  
  return (volunteer.plots)
}

#'CreateEcologist
#'
#'Creat a vector with 5 values.  
#'
#'
CreateEcologist <- function (sampling.area, detection.probability, identification.error, 
                           probability.missed.visits, costs) {
  
  ecologist <- c(sampling.area, detection.probability, identification.error, 
               probability.missed.visits, costs)
  
  return (ecologist)  
}

#' SamplingEcologist
#' 
#' This function combines the different sampling functions of the Ecologist sampling behaviour and runs 
#' them for chossen plots. It returns a dataset. 

SamplingEcologist <- function(data, plots, sampling.area,
                              detection.probability, identification.error, probability.missed.visits) {
  data[data[, 1] %in% plots, 2] <- SamplingArea (data[data[, 1] %in% plots, 2], sampling.area)
  data[data[, 1] %in% plots, 2] <- SamplingDetectionProbability (data[data[, 1] %in% plots, 2], detection.probability)
  data[data[, 1] %in% plots, 2] <- SamplingIdentificationError (data[data[, 1] %in% plots, 2], identification.error)
  data[data[, 1] %in% plots, 2] <- SamplingMissedVisits (data[data[, 1] %in% plots, 2], probability.missed.visits)
  return(data)
}
 
# SamplingArea
#
# Replaces each number in the column by a new on which 
# is a simulated random variable from a binomal distribution.
#
#   
#
# Args:
#  data: The column of the dataset which should be used for this function. 
#        The data content is the column with the number of individuals (num.of.individuals)
#  sampling.area: Sampled area of the plot in percent
#
# Returns:
#  New numbers

SamplingArea <- function (data, sampling.area) {
  for(i in 1:length(data)) {
    data[i] <- rbinom(1, data[i], (sampling.area / 100))
  }
  return(data)
}

SamplingIdentificationError <- function (data, identification.error) {
  for(i in 1:length(data)) {
    data[i] <- rbinom(1, data[i], (1 - (identification.error / 100)))
  }
  return(data)
}

SamplingDetectionProbability <- function (data, detection.probability) {
  for(i in 1:length(data)) {
    data[i] <- rbinom(1, data[i], (detection.probability / 100))
  }
  return(data)
}

SamplingMissedVisits <- function (data, missed.visits) {
  for(i in 1:length(data)) {
    data[i] <- ifelse (sample(0:100, 1) < missed.visits, NA, data[i]) #maybe with if and rbinom
  }
  return(data)
}
