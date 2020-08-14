#import data
data=read.csv("household_power_consumption.txt", sep=";")

#transform to numeric
data$Global_active_power=as.numeric(data$Global_active_power)

png("Plot2.png",width = 480,height = 480,units = "px", bg = "transparent")
hist(data$Global_active_power,
     main="Global Active Power",
     xlab="Global active Power (kilowatts)",
     col="red")
graphics.off()
