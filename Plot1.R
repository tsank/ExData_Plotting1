plot1 <- function() {
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
        
        ## Openning the png graphics device and plot the histogram in plot1.png in working directory
        png(filename = "plot1.png", width = 480, height = 480, units = "px")
        hist(powerdata$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", 
             main = "Global Active Power", ylab = "Frequency")
        
        ## closing the graphics device
        invisible(dev.off())
}