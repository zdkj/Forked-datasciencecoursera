corr <- function(directory, threshold = 0, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ret <- c()
  for(i in id){
    odata <- getmonitor(i,directory)
    data <- na.omit(odata)
    if( length(data[,1])>=threshold ) {
      ret = c(ret, cor(data$sulfate,data$nitrate))
    }
  }
  ret
}