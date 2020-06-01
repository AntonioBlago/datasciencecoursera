rankall <- function(outcome, num = "best") {
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
  
  hospital_rank<-function (list_of_states,num="best"){
    data_frame_out<-data.frame(Hospital.Name = character(), State = character())
    states_num<-length(list_of_states)
    for (i in 1:states_num){
      ## create dataframe
      
      state<-list_of_states[i]
      
      data_outcome<-data[data[,c(7)]==state,]
      data_outcome<-data_outcome[,c(2,7,column)]
      data_outcome[3] <- lapply(data_outcome[3], as.numeric)
      
      data_outcome<-data_outcome[complete.cases(data_outcome),]
      data_outcome<-data_outcome[order(data_outcome[3]),]
      
      ##rank hospitals in states
      data_outcome$rank <- 1:nrow(data_outcome)
      
      ##get rank
      #print(data_outcome)
      #print(is.numeric(num))

      if (is.character(num)==TRUE){
        if (num=="worst"){
          #print("worst")
          num_row<-nrow(data_outcome)
          df<-data.frame()
          df<-data_outcome[data_outcome$rank==num_row,][,c(1,2)]
          row.names(df)<-state
          data_frame_out<-rbind(data_frame_out,df)
          #data_frame_out
        }else if (num=="best"){
          #print("worst")
          num_row<-nrow(data_outcome)
          df<-data.frame()
          df<-data_outcome[data_outcome$rank==num_row,][,c(1,2)]
          row.names(df)<-state
          data_frame_out<-rbind(data_frame_out,df)
          #data_frame_out
          
        }
        
      }else if (is.numeric(num)==TRUE){
        number<-num
        if (number>nrow(data_outcome)){
          #print("NA")
          #df<-data_outcome[data_outcome$rank==num_row,][,c(1,2)]
          df<-data.frame(Hospital.Name = NA, State = state)
          #df<-c("NA",state)
          .rowNamesDF(df, make.names=FALSE)<-state
          data_frame_out<-rbind(data_frame_out,df)
        }else{
          number<-number
          #print(state)
          #print(num)
          #print("check")
          df<-data.frame()
          df<-data_outcome[data_outcome$rank==number,][,c(1,2)]
          .rowNamesDF(df, make.names=FALSE)<-state
          data_frame_out<-rbind(data_frame_out,df)
        
      
        }
      }
    }
    data_frame_out<-data_frame_out[order(data_frame_out[2]),]
    data_frame_out
    }
  
  #foo[foo[ ,c(1,5,9)], ]
  
  dataframe_out<-data.frame(Hospital.Name = character(), State = character())
  #check_if_num=as.numeric(num)
  
  if (is.na(num)==TRUE){
    dataframe_out<-NA
  }else if(num=="best"){
    
    dataframe_out<-hospital_rank(list_of_states,num="best")
    #out<-data_outcome[data_outcome$rank==1,1]
  }else if(num=="worst"){
    dataframe_out<-hospital_rank(list_of_states,num="worst")
    #out<-data_outcome[data_outcome$rank==nrow(data_outcome),1]
  }else{
    #print("Check")
    dataframe_out<-hospital_rank(list_of_states,num=num)
    #out<-data_outcome[data_outcome$rank==num-1,1]
  }

  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  dataframe_out
}