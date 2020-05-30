best <- function(state, outcome) {
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  list_of_states<-unique(data$State)
  
  if (state %in% list_of_states){
  
    if (outcome=="heart attack"){
      column<-11
    }else if(outcome=="heart failure"){
      column<-17
    }else if(outcome=="pneumonia"){
      column<-23
    }else{
      message<-paste("Error in best"," ( ",state," ,",outcome," ) ",": invalid outcome")
      stop(message)
    }
    
  }else {
    message<-paste("Error in best"," ( ",state," ,",outcome," ) ",": invalid state")
    stop(message)
  
  }
      
  #foo[foo[ ,c(1,5,9)], ]
  data_outcome<-data[data[,c(7)]==state,]
  data_outcome<-data_outcome[,c(2,7,column)]
  data_outcome[3] <- lapply(data_outcome[3], as.numeric)
  
  data_outcome<-data_outcome[order(data_outcome[3]),]
  output<-data_outcome[1,1]
  output
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  }