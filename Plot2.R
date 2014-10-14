setwd(paste0(getwd(),"/Exploratory_Assignment1"))
data <- read.table("household_power_consumption-2.txt",sep=";",na.strings="?",header=TRUE)
options("max.print"=1000000000)
mydata <- subset(data,Date>="1/2/2007" & Date<="2/2/2007")
df1 <- subset(data,data$Date=="1/2/2007" | data$Date=="2/2/2007")

# Second Plot: Line Plot
par(mfrow=c(1,1))
df1$Date <- as.Date(as.character(df1$Date),format="%d/%m/%Y")
df2 <- df1
df2$Date <- as.POSIXct(paste(df1$Date,df1$Time),format="%Y-%m-%d %H:%M:%S")
plot(df2$Global_active_power~df2$Date,type="l",ylab="Global Active Power (kilowatt)",xlab="")
dev.copy(png,"Plot2.png",width=480,height=480)
dev.off()