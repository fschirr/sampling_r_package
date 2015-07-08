#' Select plots of the dataset.
#' 
#' \code{ChoosePlots} selects randomly a certain number of plots from the 
#' dataset.
#' 
#' @param data Complete dataset.
#' @param num.of.plots A number.
#' 
#' @return A vector with the name/number of the selected plots.

ChoosePlots <- function (data, num.of.plots) {
  
  choosenplots <- sort (sample (unique (data[, 1]), num.of.plots, replace = F))
  
  return (choosenplots) #chosenplots
}

#' Selects the plots which are monitored by experts.
#' 
#' \code{ExpertPlots} selects the plots of the \code{choosenplots} which are 
#' monitored by experts.
#' 
#' @param choosenplots Output of the function \code{ChoosePlots}
#' @param num.of.plots A number.
#' @param num.experts A number.
#' 
#' @return A vector with the name/number of the plots.

ExpertPlots <- function (choosenplots, num.of.plots, num.experts) {
  
  if (num.of.plots == 1 & num.experts == 1) {
    expert.plots <- choosenplots
  } else {
    expert.plots <- sample (choosenplots, num.experts)
  }
  
  return (expert.plots)
}

#' Selects the plots which are monitored by volunteers.
#' 
#' \code{VolunteerPlots} selects the plots of the \code{choosenplots} which are 
#' not monitored by experts.
#' 
#' @param choosenplots Output of the function \code{ChoosePlots}
#' @param num.experts A number.
#' 
#' @return A vector with the name/number of the plots.

VolunteerPlots <- function (choosenplots, expert.plots) {
  
  volunteer.plots <- choosenplots[!(choosenplots %in% expert.plots)]
  
  return (volunteer.plots)
}

# SamplingEcologist
# 
#This function combines the different sampling functions of the Ecologist 
# sampling behaviour and runs them for chossen plots. It returns a dataset. 

SamplingEcologist <- function(data, plots, sampling.area, detection.probability, 
                              identification.error, probability.missed.visits) {

  data[data[, 1] %in% plots, 2] <- SamplingArea (data[data[, 1] %in% plots, 2], 
                                                 sampling.area)
  data[data[, 1] %in% plots, 2] <- SamplingDetectionProbability (data[data[, 1] 
                                          %in% plots, 2], detection.probability)
  data[data[, 1] %in% plots, 2] <- SamplingIdentificationError (data[data[, 1] 
                                          %in% plots, 2], identification.error)
  data[data[, 1] %in% plots, 2] <- SamplingMissedVisits (data[data[, 1] 
                                          %in% plots, 2], 
                                          probability.missed.visits)
  return(data)
}

# SamplingArea
#
# Replaces each number in the column by a new on which 
# is a simulated random variable from a binomal distribution.
#
# Args:
#  data: The column of the dataset which should be used for this function. 
#        The data content is the column with the number of individuals 
#        (num.of.individuals)
#  sampling.area: Sampled area of the plot in percent
#
# Returns:
#  New numbers

SamplingArea <- function (data, sampling.area) { #other name clarification: proportion of individuals sampled may vary
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
    data[i] <- ifelse (sample(0:100, 1) < missed.visits, NA, data[i]) #uniform dist
  }
  return(data)
}
