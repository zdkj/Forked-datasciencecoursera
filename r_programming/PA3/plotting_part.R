
loaddata <- function(){
  outcome <<- read.csv('outcome-of-care-measures.csv', colClasses='character')
  hospital <<- read.csv('hospital-data.csv',colClasses = 'character')
  outcome.hospital <<- merge(outcome,hospital, by = 'Provider.Number')
  
  outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <<-
    as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  
  outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <<-
    as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  
  outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <<-
    as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
}

part1 <- function() {
  par(mfrow=c(1,1))
  hist(outcome[,11],xlab='30-day Death Rate', main='Heart Attack 30-day Death Rate')
}

part2 <- function() {
  par(mfrow=c(3,1))
  hist(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
       ,xlab='30-day Death Rate',main='Heart Attack',xlim=range(0,20))
  hist(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
       ,xlab='30-day Death Rate',main='Heart Failure',xlim=range(0,20))
  hist(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
       ,xlab='30-day Death Rate',main='Pneumonia',xlim=range(0,20))
}


part3 <- function() {
  par(mfrow=c(1,1))  
  states <- outcome$State
  states <- subset(states, states>20)
  outcome2 <- subset(outcome, outcome$State %in% states)
  death<-outcome2[,11]
  state<-outcome2$State
  boxplot(death~state,ylab = "30-day Death Rate",main="Heart Attack 30-day Death by State",las = 2)
  state.median <- tapply(outcome2[,11], outcome2$State, median, na.rm=TRUE)
  state.medianordered<-sort(state.median)
}

part4 <- function() {
  death <- as.numeric(outcome.hospital[,11])
  npatient <- as.numeric(outcome.hospital[,15])
  owner <- factor(outcome.hospital$Hospital.Ownership)
  library(lattice)
  xyplot(death~npatient|owner,data = outcome.hospital,main= "Heart Attack 30-day Death Rate by Ownership",xlab="Number of Patients Seen",ylab = "30-day Death Rate",
         groups =owner,panel =function(x,y,...){
           panel.xyplot(x,y,...)
           panel.lmline(x,y,col =2)
         })
}

