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

#sub Plot
png("Plot4.png",width = 480,height = 480,units = "px", bg = "transparent")
par(mfrow = c(2,2), mar = c(4, 4, 4, 2), oma = c(0, 0, 1, 0)) #bottom, left, top, right

#plot 1
with(sub, plot(datetime, Global_active_power, type="l", xlab = "", ylab = "Global Active Power"))
#plot 2
with(sub, plot(datetime, Voltage, type="l"))
#plot 3
with(sub, plot(datetime, Sub_metering_1, col = "black", type="l", xlab = "", ylab = "Energy sub metering"))
with(sub, lines(datetime, Sub_metering_2, col = "red", type="l"))
with(sub, lines(datetime, Sub_metering_3, col = "blue", type="l"))
legend("topright", lty = c(1,1), bty = "n", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#plot 4
with(sub, plot(datetime, Global_reactive_power, type="l"))

graphics.off()
