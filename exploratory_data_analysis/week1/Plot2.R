#import data
data=read.csv("household_power_consumption.txt", sep=";")

#transform to numeric
data$Global_active_power=as.numeric(data$Global_active_power)

#transform to date
data$Date=as.Date(data$Date,format="%d/%m/%Y")


#create column datetime
data$datetime=paste(data$Date,data$Time)

#transform to DateTime
data$datetime=as.POSIXct(strptime(data$datetime,"%Y-%m-%d %H:%M:%S"))
  
#subsetting data
sub=subset(data, Date > as.Date('2007-01-31') & Date < as.Date('2007-02-03'))

#plot
png("Plot2.png",width = 480,height = 480,units = "px", bg = "transparent")
with(sub, plot(datetime, Global_active_power, type="l", xlab = "", ylab = "Global Active Power (kilowatts)"))
 graphics.off()
 