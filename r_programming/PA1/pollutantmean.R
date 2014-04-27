pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  source('getmonitor.R')
  data <- lapply(id,FUN=function(id){getmonitor(id,directory)})
  library(plyr)
  data <- ldply(data)
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  mean(data[,pollutant],na.rm=T)
}