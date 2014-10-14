setwd(paste0(getwd(),"/Exploratory_Assignment1"))
data <- read.table("household_power_consumption-2.txt",sep=";",na.strings="?",header=TRUE)
options("max.print"=1000000000)
mydata <- subset(data,Date>="1/2/2007" & Date<="2/2/2007")
df1 <- subset(data,data$Date=="1/2/2007" | data$Date=="2/2/2007")

# First Plot: Histogram
par(mfrow=c(1,1))
hist(df1$Global_active_power,main="Global Active Power",col="red",xlab="Global Active Power (kilowatts)",ylab="Frequency")
dev.copy(png,"Plot1.png",width=480,height=480)
dev.off()