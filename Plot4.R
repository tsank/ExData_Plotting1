plot4 <- function() {
        file <- file.path(getwd(),"household_power_consumption.txt")
        
        ## Reading first 1000 rows to get the column classes and reading the complete file as character with readLines()
        testdata <- read.table(file, header = T, sep = ";", nrows = 1000, na.strings = c(NA, "?"), stringsAsFactors = F)
        classes <- sapply(testdata, class)
        f <- readLines(file)
        
        ## Estimation of file size: Extrapolating the size of 1000 line object to calculate the size of 2075259 rows of data
        size <- object.size(testdata)
        estimated.size <- round(((size/1000)*2075259)/1e+9, digits = 2)
        print(paste("Estimated object size: ", estimated.size, "GB"))   ## Output: [1] "Estimated object size:  0.27 GB"
        
        ## Identifying index for first row for date 1/2/2007 and first row for date 3/2/2007 and storing in numeric vector i
        ## str_detect is in package stringr
        library(stringr)
        f1 <- f[which(str_detect(f, fixed("1/2/2007")))][1]; f2 <- f[which(str_detect(f, fixed("3/2/2007")))][1]
        i <- charmatch(c(f1, f2), f)
        
        ## Reading only the required data with dates from 01-02-2007 and 02-02-2007 passting i[] to skip and nrows parameters
        powerdata <- read.table(file, skip = (i[1] - 2), 
                                nrows = (i[2] - i[1]), dec = ".", header = T, sep = ";", 
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