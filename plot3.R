## plot3.R
## home: github.com/dpomondo/... {to be filled in  later}
##
plot3 <- function(verbose=FALSE) {
    targetfile <- './data/household_power_consumption.txt'
    ## First, we get the file in the right place!
    if (!file.exists('data')) {
        if (verbose==TRUE) { print("creating data directory") }
        dir.create('data')
    }
    if (!file.exists(targetfile)) {
        if (verbose==TRUE) { print("Downloading dataset...") }
        fileUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
        ## download and unzip
        dest <- './data/dataset.zip'
        download.file(fileUrl, destfile=dest, method='curl')
        if (verbose==TRUE) { print("Unzipping dataset...") }
        unzip(dest, exdir='./data/')
    }
    ## Now that we know we have the file where we want it, we read it into memory
    ## First, make sure it's not already IN memeory:
    if (!exists('household_power') || !is.data.frame(household_power)) {
        ## use a SQL query to pull out the specific rows we want
        if (verbose==TRUE) {print("Loading sqldf library...")}
        library('sqldf')
        if (verbose==TRUE) {print("Loaded.")}
        household_power <<- read.csv2.sql(targetfile, 
                            sql = 'select * from file where Date="1/2/2007" or Date="2/2/2007" ', 
                            colClasses="character") }
    else if (verbose==TRUE) {print("returning cached 'household_power' dataset")
    }
    ## test to see if there's a date_time column
    if (is.null(household_power$date_time)) {
        household_power$date_time <- as.POSIXct(paste(household_power[, 'Date'], 
                                                      household_power[, 'Time']),
                                                format='%d/%m/%Y %H:%M:%S')
        # reorder the columns, just because
        household_power <<- cbind(household_power[10], 
                                  household_power[3:9])
    }
    ## And finally make the plot!
    ## png call goes here
    png(file='rplot3.png')
    ##  note: killing the x-axis tick marks until I can figure out how to get day labels
    plot(x=household_power$date_time, y=household_power$Sub_metering_1, 
         type='s', ylim=c(0, 38), xlab='date_time', ylab='Energy Sub Metering', 
         col='black')
    lines(x=household_power$date_time, 
          y=household_power$Sub_metering_2, col='red')
    lines(x=household_power$date_time, 
          y=household_power$Sub_metering_3, col='blue')
    legend('topright', legend=c('Sub Metering 1', 
                                'Sub Metering 2',
                                'Sub Metering 3'), 
           col=c('black', 'red', 'blue'), lwd=2, inset=0.04, yjust=0)
    dev.off()
}
