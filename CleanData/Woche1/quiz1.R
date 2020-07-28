data<-read.csv("getdata_data_ss06hid.csv")

#data$VA<-as.numeric()
mill<-data[data$VAL==24 & is.na(data$VAL)== FALSE,]

library(readxl)
library(xlsx)

#Read rows 18-23 and columns 7-15 into R and assign the result to a variable called:
dat<-read_excel("getdata_data_DATA.xlsx")[17:22, 7:15] #rowIndex =18:23 , colIndex = 7:15)
dat.names<-dat[1,]
colnames(dat) <- dat.names
dat<-dat[2:6,]
dat$Zip<-as.numeric(dat$Zip)
dat$Ext<-as.numeric(dat$Ext)

res<-sum(dat$Zip*dat$Ext,na.rm=T)

#Read the XML data on Baltimore restaurants from here:
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
#How many restaurants have zipcode 21231? 

library(xml2)
library(XML)

data <- xmlTreeParse("getdata_data_restaurants.xml",useInternal=TRUE)
rootnode<-xmlRoot(data)

zipcodes<-xpathSApply(rootnode,"//zipcode",xmlValue)
zipcodes[zipcodes=="21231"]

# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
# using the fread() command load the data into an R object

install.packages("data.table")
library("data.table")
DT<-fread("getdata_data_ss06pid.csv")


# rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
# 
# tapply(DT$pwgtp15,DT$SEX,mean)
# 
# sapply(split(DT$pwgtp15,DT$SEX),mean)
# 
# DT[,mean(pwgtp15),by=SEX]
# 
# mean(DT$pwgtp15,by=DT$SEX)
# 
# mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)


system.time(mean(DT$pwgtp15,by=DT$SEX))


