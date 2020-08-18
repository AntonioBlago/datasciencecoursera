pollutantmean<- function(directory,pollutant,id=1:332){
  mean_overall<-c()
  
  for (i in id){
    len=nchar(i)
    id_1=paste(rep(0, 3-len),sep="",collapse = "")
    id_1=paste(id_1,i,sep="",collapse = "")
    id_1=paste(id_1,".csv",sep="",collapse = "")
    dire<-paste(getwd(),"rprog_data_specdata",directory,id_1,sep="/")
    data<-read.csv(dire)
    mean_overall<-c(mean_overall,data[pollutant][!is.na(data[pollutant]),])
  }
  mean(mean_overall)
  
}


