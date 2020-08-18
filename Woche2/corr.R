corr <-function(directory,threshold=0){
  
  dataframe<-complete(directory)
  
  dataframe<-dataframe[dataframe$nobs>threshold,]
  corr_overall<-c()
  
  for(i in 1:nrow(dataframe)){
      len=nchar(dataframe[i,1])
      id_1=paste(rep(0, 3-len),sep="",collapse = "")
      id_1=paste(id_1,dataframe[i,1],sep="",collapse = "")
      id_1=paste(id_1,".csv",sep="",collapse = "")
      dire<-paste(getwd(),"rprog_data_specdata",directory,id_1,sep="/")
      data<-read.csv(dire)
      data_count <- data[(!is.na(data[,2])), ]
      
      data_count <- data_count[(!is.na(data_count[,3])), ]
      
      corr_overall<-c(corr_overall,cor(data_count[,2],data_count[,3]))
    }
    #print(mean_overall)
    corr_overall
  
}