rankhospital <- function(state, outcome, num = "best") {
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
  
  data_outcome<-data_outcome[complete.cases(data_outcome),]
  data_outcome<-data_outcome[order(data_outcome[3]),]
  
  
  data_outcome$rank <- 1:nrow(data_outcome)
  #print(data_outcome)
  check_if_num=as.numeric(num)
  
  if (is.na(check_if_num)==TRUE){
    if (nrow(data_outcome)<as.numeric(num)){
      out<-NA
    }
  }else if(num=="best"){
    out<-data_outcome[data_outcome$rank==1,1]
  }else if(num=="worst"){
    out<-data_outcome[data_outcome$rank==nrow(data_outcome),1]
  }else{
    #print(data_outcome)
    number<-num+1
    out<-data_outcome[data_outcome$rank==number,1]
  }

  out
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
