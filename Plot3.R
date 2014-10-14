setwd(paste0(getwd(),"/Exploratory_Assignment1"))
data <- read.table("household_power_consumption-2.txt",sep=";",na.strings="?",header=TRUE)
options("max.print"=1000000000)
mydata <- subset(data,Date>="1/2/2007" & Date<="2/2/2007")
df1 <- subset(data,data$Date=="1/2/2007" | data$Date=="2/2/2007")

# Third Plot: Multi-series Plot
par(mfrow=c(1,1))
plot(df2$Sub_metering_1~df2$Date,type="l",col="black",xlab="",ylab="Energy sub metering")
lines(df2$Sub_metering_2~df2$Date,type="l",col="red")
lines(df2$Sub_metering_3~df2$Date,type="l",col="blue")
dev.copy(png,"Plot3.png",width=480,height=480)
dev.off()