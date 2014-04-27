best <- function(state, outcome, data = read.csv('outcome-of-care-measures.csv', colClasses='character')) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  sdata <- subset(data, data$State==state)
  diseases <- c("heart attack","heart failure","pneumonia")
  
  if(length(sdata[,1])==0) {
    stop('invalid state')
  }
  if( !outcome %in% diseases) {
    stop("invalid outcome")
  }
  
  if(outcome == "heart attack"){
    ranks <- suppressWarnings(as.numeric(sdata[,11]))
  }else if(outcome == "heart failure"){
    ranks <- suppressWarnings(as.numeric(sdata[,17]))
  }else{
    ranks <- suppressWarnings(as.numeric(sdata[,23]))
  }
  hranks <- data.frame(Hospital=sdata$Hospital.Name,Rate = as.numeric(ranks),stringsAsFactors=FALSE)
  hranks <- hranks[complete.cases(hranks),]
  hranks <- hranks[order(hranks$Rate,hranks$Hospital),]
  hranks[1,]$Hospital
}