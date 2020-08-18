complete <-function(directory,id=1:332){
  data_frame1<-data.frame(id=numeric(0), nobs=numeric(0))
  
  for (i in id){
    len<-nchar(i)
    id_1<-paste(rep(0, 3-len),sep="",collapse = "")
    id_1<-paste(id_1,i,sep="",collapse = "")
    id_1<-paste(id_1,".csv",sep="",collapse = "")
    dire<-paste(getwd(),"rprog_data_specdata",directory,id_1,sep="/")
    data<-read.csv(dire)
    data_count <- data[(!is.na(data$sulfate)), ]
    data_count <- data_count[(!is.na(data_count$nitrate)), ]
    nobs <- nrow(data_count)
    data_frame1<-  rbind(data_frame1, data.frame(id=i, nobs=nobs))
  }
  #print(mean_overall)
  data_frame1
}