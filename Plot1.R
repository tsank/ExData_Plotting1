plot1 <- function() {
        file <- file.path(getwd(),"household_power_consumption.txt")
        
        ## Reading the first row to get the column classes
        testdata <- read.table(file, header = T, sep = ";", nrows = 1, na.strings = c(NA, "?"), stringsAsFactors = F)
        classes <- sapply(testdata, class)
        
        ## Reading the lines as character with readLines and run charmatch() to get the indices for running read.table() with
        ## specifically identified values of 'skip' and 'nrows' to read only the required set of data. The indices for first line on dates 1/2/2007
        ## and 3/2/2007 are identified from which the reqired indices for skip and nrow parameters are calculated in the read.table function
        f <- readLines(file)
        f1 <- charmatch(c("1/2/2007;00:00:00;0.326;0.128;243.150;1.400;0.000;0.000;0.000",
                          "3/2/2007;00:00:00;3.614;0.106;240.990;15.000;0.000;1.000;18.000"), f)
        
        ## Reading only the required data with dates from 01-02-2007 and 02-02-2007
        powerdata <- read.table(file, skip = (f1[1]-2), 
                                nrows = (f1[2]-f1[1]), dec = ".", header = T, sep = ";", 
                                na.strings = c(NA, "?"), stringsAsFactors = F, colClasses=classes)
        
        ## Setting the column names with previous read dataframe with single row
        colnames(powerdata) <- colnames(testdata)
        
        ## Openning the png graphics device and plot the histogram in plot1.png in working directory
        png(filename = "plot1.png", width = 480, height = 480, units = "px")
        hist(powerdata$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", 
             main = "Global Active Power", ylab = "Frequency")
        
        ## closing the graphics device
        invisible(dev.off())
}