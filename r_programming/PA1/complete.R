complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nobs <- c()
  for(i in id){
    data <- getmonitor(i,directory)
    nob <- length(na.omit(data)[,1])
    nobs <- c(nobs,nob)
  }
  ret <- data.frame(id,nobs)
  print(ret)
  ret
}