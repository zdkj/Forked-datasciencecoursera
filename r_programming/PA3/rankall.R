rankall <- function(outcome, num = "best", data = read.csv('outcome-of-care-measures.csv', colClasses='character')) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  diseases <- c("heart attack","heart failure","pneumonia")
  col <- c(11,17,23)
  df <- data.frame(diseases,col)
  if( !outcome %in% df$diseases) {
    stop("invalid outcome")
  }else {
    colnum <- df[diseases==outcome,]$col
  }
  
  ranks <- suppressWarnings(as.numeric(data[,colnum]))
  hranks <- data.frame(Hospital=data$Hospital.Name,Rate = as.numeric(ranks),State = data$State ,stringsAsFactors=FALSE)
  hranks <- hranks[complete.cases(hranks),]
  hranks <- hranks[order(hranks$State,hranks$Rate,hranks$Hospital),]
  
  helper <- function(data,num) {
      if(num=="best"){
        data[1]
      }else if(num=="worst"){
        data[length(data)]
      }else{
        data[num]
      }
  }
  ret <- aggregate(Hospital~State,data = hranks, FUN= function(data) { helper(data,num)}  ) 
  data.frame(hospital = ret$Hospital, state = ret$State)
}