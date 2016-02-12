plot4 <- function() {
        file <- file.path(getwd(),"household_power_consumption.txt")
        
        ## Reading the first row to get the column classes
        testdata <- read.table(file, header = T, sep = ";", nrows = 1, na.strings = c(NA, "?"), stringsAsFactors = F)
        classes <- sapply(testdata, class)
        
        ## Reading the lines as character with readLines and run charmatch() to get the indices for running read.table with
        ## specifically identified values of 'skip' and 'nrows' to read only the required set of data. The indices for first line on dates 1/2/2007
        ## and 3/2/2007 are identified from which the reqired indices for skip and nrow parameters are calculated in the read.table function
        f <- readLines(file)
        
        ## Reading only the required data with dates from 01-02-2007 and 02-02-2007
        f1 <- charmatch(c("1/2/2007;00:00:00;0.326;0.128;243.150;1.400;0.000;0.000;0.000",
                          "3/2/2007;00:00:00;3.614;0.106;240.990;15.000;0.000;1.000;18.000"), f)
        powerdata <- read.table(file, skip = (f1[1]-2), 
                                nrows = (f1[2]-f1[1]), dec = ".", header = T, sep = ";", 
                                na.strings = c(NA, "?"), stringsAsFactors = F, colClasses=classes)
        
        ## Setting the column names with previous read dataframe with single row
        colnames(powerdata) <- colnames(testdata)
        
        ## Merging Date and Time columns in a single Date_Time column and making the Date_Time column as the first column of the data frame
        ## Also the earlier Date and Time columns are removed
        powerdata$Date_Time <- paste(powerdata$Date, powerdata$Time)
        powerdata$Date <- NULL; powerdata$Time <- NULL
        powerdata <- powerdata[c(8, 1:7)]
        
        ## Creating a new column for the date with POSIXlt class
        powerdata$weekday <- strptime(powerdata$Date_Time, format="%d/%m/%Y %H:%M:%S")
        
        ## Setting up png device to create plot4.png
        png(filename = "plot4.png", width = 480, height = 480, units = "px")
        
        ## Setting up the panel with 2 columns and 2 rows
        par(mfcol = (c(2, 2)))
        
        ## First plot @ top left corner
        plot(powerdata$weekday, powerdata$Global_active_power, 
             xlab = "", ylab = "Global Active Power", 
             type="l", main = "")
        
        ## Second plot @ bottom left corner
        plot(powerdata$weekday, powerdata$Sub_metering_1, 
             xlab = "", ylab = "Energy sub metering",
             type="l", main = ""); par(new=T)
        legend("topright", colnames(powerdata[6:8]), lty = 1, inset = 0.005, box.lty = 0, col = c("black", "red", "blue"))
        lines(powerdata$weekday, powerdata$Sub_metering_2, col = "red"); par(new=T)
        lines(powerdata$weekday, powerdata$Sub_metering_3, col = "blue")
        
        ## Third plot @ top right corner
        plot(powerdata$weekday, powerdata$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
        
        ## Fourth plot @ bottom right cornder
        plot(powerdata$weekday, powerdata$Global_reactive_power, type = "l", xlab = "datetime", ylab = colnames(powerdata[3]))
        
        ## Closing graphics device
        invisible(dev.off())
}