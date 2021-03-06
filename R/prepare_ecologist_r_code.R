#' Rewrite the dataset into a usable format.
#' 
#' \code{PrepareDataset} rewrites the dataset to make it useable for the 
#' Sampling function of this package. The function comibines the different 
#' columns and creates a new dataset. If there are now data fro a column the 
#' Column will be filled with zeros.
#'
#' @param data The input dataset.
#' @param plot 
#' @param num.of.individuals 
#' @param species
#' @param x.coord
#' @param y.coord
#' @param visit.year
#' 
#' @export Dataset in which the columns in the right order. 
#'   

PrepareDataset <- function(data, plot, num.of.individuals, species, x.coord, 
                           y.coord, visit.year, visit.month, visit.day) {
  
  column.plot <- ifelse (plot > 0, data[plot], rep(0, length(data[, 1])))
  column.num.of.individuals <- ifelse (num.of.individuals > 0, 
                                       data[num.of.individuals],
                                       rep(0, length(data[, 1])))
  column.species <- ifelse (species > 0, data[species], rep(0, 
                                                            length(data[, 1])))
  column.x.coord <- ifelse (x.coord > 0, data[x.coord], rep(0, 
                                                            length(data[, 1])))
  column.y.coord <- ifelse (y.coord > 0, data[y.coord], rep(0, 
                                                            length(data[, 1])))
  column.visit.year <- ifelse (visit.year > 0, data[visit.year], 
                               rep(0, length(data[, 1])))
  column.visit.month <- ifelse (visit.month > 0,data[visit.month], 
                                rep(0, length(data[, 1])))
  column.visit.day <- ifelse (visit.day > 0, data[visit.day], 
                              rep(0, length(data[, 1])))
  
  
  data <- data.frame (column.plot, column.num.of.individuals, column.species, # other name than data
                      column.x.coord, column.y.coord, column.visit.year, 
                      column.visit.month, column.visit.day)
  
  colnames (data) <- c("plot", "num.of.individuals", "species", 
                       "x.coord", "y.coord", "year", "month", "day")
  
  return (data)
}

#' Creates a vector with the behaviour of an ecologist.
#' 
#' \code{CreateEcologist} creates a vector with characteristics of the behaviour
#' of an ecologist in the field. The characteristics are: The sampled area of a
#' plot, the detection probability, the identification error, the probability
#' of missed vistis and the costs. The first four characteristics are expected 
#' to be in percent. 
#' 
#' @param sampling.area A number in percent.
#' @param detection.probability A number in percent.
#' @param identification.error A number in percent.
#' @param propability.missed.visits A number in percent.
#' @param costs A number in monetary units.
#' 
#' @export A vector with the five characteristics of an ecologist.


CreateEcologist <- function (sampling.area, detection.probability, 
                             identification.error, probability.missed.visits, 
                             costs) {
  
  ecologist <- c(sampling.area, detection.probability, identification.error, 
                 probability.missed.visits, costs)
  
  return (ecologist)  
}
